Reified types
=============

Every type in this language must have a corresponding C type: we call this
process *type reification*.  We assume the mapping is of the form:

    TS.Type -> [C.Type]

This is a formality that makes unpacking types easier: it means things like
`(A, B) -> C` are translated directly into `C (*)(A, B)`, rather than the
clunkier `C (*)(struct { A; B; })`.

We're not going to talk about the reification of *monomorphic* types like:

    Int  ===> int
    Char ===> char

This is boring and easy.  Instead, we want to talk about *polymorphic* types,
because these have no obvious equivalent in C.

## Reification of polymorphic values

Let's think about how to translate a polytype like `a`, as found in say the
identity function:

    identity : a -> a
    identity x = x

Since polymorphic types don't exist in C, we must make it monomorphic …
*somehow*.  Before we look at the approaches, let's try to understand what the
fundamental limitation is:

> In C, all types must have a size that is known at compile-time.

(This also means that variable-length arrays (VLAs) in C99 are really more of
a hack.)  This constraint is what forbids naive implementations of
polymorphism.  The size of a *truly* polymorphic value is indefinite, but
there's no way to declare such a thing in C:

    char[?] identity(char[?]);

In principle though, there's no reason why this has to be the case!  Since
`alloca` and VLAs are not in fact mythological creatures, it would in fact be
sensible to implement such a thing if you could access the underlying machine
code.  In fact I think there *is* a way to do it in C, even, but it's such an
ugly hack that I feel it's not very worthwhile (see bottom of this post for an
explanation).  Remember, one of the principal goals of this language is to
have a very simple and straightforward mapping to C, so if it's so convoluted
then it's probably not worth it.

In any case, barring these hacks, there are two general approaches seen in
most languages nowadays:

  - **Monomorphization**: This is the technique C++ and Rust use.  Here, we
    must specialize the `identity` function for *every type that gets used*:

    ```c
    int identity_i(int);
    double identity_d(int);
    ...
    ```

    Advantages: can more efficient for primitive types; no dynamic allocation
    needed

    Disadvantages: requires name mangling; causes code bloat; breaks module
    boundaries (this is why in C++ you have to put templates in the header!).

  - **Boxing** (for the lack of a better name): This is what most other
    languages use.  From an OOP perspective, you define a universal type
    called "object" that is the supertype of every class so now you can
    manipulate all objects at will.  For a machine-level perspective, you
    treat everything as a pointer!

    Advantages: (all the disadvantages of monomorphization!)

    Disadvantages: everything requires an indirection now, so it's slow;
    it also requires dynamic allocation, so it could potentially fail
    (bad for embedded systems).

(As a remark: even though static allocation is often said to be better than
dynamic, I'm not entirely sure that's always the case since at least with a
malloc you can tell when you've OOM, whereas if you run out of stack space you
are really screwed for good.)

Anyway, so I think boxing is *simpler*, so the language shall default to
boxing as the default approach.  Monomorphization is still available: the
developer can choose to specialize the polymorphic types at their own
discretion.

Therefore, all polymorphic values will be stored as a plain pointer (`void *`).
The main catch is that *unboxed* values must be boxed in order to be fed into
a polymorphic function.

    identity : a -> a
    identity x = x
    ==>
    identity_1 : VoidPtr -> VoidPtr
    identity_1 x = x

    example : Int
    example = identity 42
    ==>
    example_1 : Int
    example_1 = ..
      p1 = xmalloc(sizeof(proxy : Proxy Int))
      @assign(p1, 42)
      p2 = identity_1(p1)
      r = @dereference(cast_pointer(p2))
      free(p2)
      r

The implementation of `identity` is trivial, since it just returns the same
pointer that was passed in.

`@dereference` is quite special and isn't a true function.  If it were it
would have this signature:

    dereference : Ptr a -> a

But this would be a polymorphic function and thus we have a chicken-and-egg
problem!  (Similarly for `@assign`.)

Note that *had we specialized or inlined* the `identity` function, we would
have avoided all this boxing and unboxing:

    identity_Int : Int -> Int
    identity_Int x = x

    example_specialized : Int
    example_specialized = identity_Int 42

    # or alternatively:

    example_inlined : Int
    example_inlined = ..
      x = 42
      x

## Additional notes

### Hypothetical implementation of quasi-unboxed polymorphic values (QUPV)

(In order to implement "true" unboxed polymorphic values you'd need assembly.)

This implementation offers *quasi-unboxed* polymorphic values (QUPV), which
offer the following advantage over boxed polymorphic values: namely that no
dynamic allocation is necessary; everything is allocated on the stack,
provided that `alloca` or VLAs are available.  (Corollary: the disadvantage is
that there's a lot more copying, but if the values are small then this is
probably acceptable.)

The difference being that because C does not really provide a good way to
manipulate the stack directly, we are going to have to emulate it to some
degree.  Furthermore, we will need to keep track of alignments too, since we
are now allocating everything ourselves.

The representation of QUPVs is still a pointer:

```c
typedef void *QUPV;
```

However, this is now a pointer to the *stack*.  This gives us a very
unfortunate limitation: we cannot safely *return* such values in a naive
manner due to limited lifetime of a stack pointer (it expires once the
enclosing scope ends).

Consider the prototypical example of the `identity` function, implemented
using QUPV:

```c
/* identity
    : a -> a
    ~ QUPV a => a -> a
    ~ QUPV a => (Ptr a, ConstPtr a) -> ()
    ~ (Size, VoidPtr, ConstVoidPtr) -> ()
  where the `QUPV a` constraint is isomorphic to `Size` */
void identity(size_t sizeof_a, void *ret, const void *x)
{
    memcpy(ret, x, sizeof_a);
}
```

We have to pass in a pointer to the future return value because anything
allocated within this function will expire once it returns.  This means the
*caller* is responsible for allocating this buffer.  For example:

```c
…
char ret[sizeof_a];
identity(sizeof_a, ret, x);
…
```

Note that we have assumed the constraint `QUPV` in this function.  This is
okay, because `QUPV` can be derived for *any* type, even polymorphic ones like
`Pair a b`.  However, it's a little tricky though, due to the presence of
alignment (and this is where it gets ugly).

We don't particularly care about the alignment of the polymorphic variable
itself since by the time it gets used again as a concrete variable it will
have been stored in a concrete-type variable and therefore its alignment will
be correct by then.  Rather, we are concerned about the other *concrete types*
that come bundled with it.  Consider the following algebraic type:

    struct Foo<a> { int; a };
    struct Bar<a> { double, Foo<a>; }

Due to alignment constraints, we have the following (written informally, but
hopefully you get the idea):

```c
size_t sizeof_Foo_a(size_t sizeof_a)
{
    return sizeof(int) + pad_to(sizeof_a, alignof(int));
}
size_t sizeof_Bar_a(size_t sizeof_a)
{
    return sizeof(double) + pad_to(sizeof_a, alignof(double));
}
```

This information is required in order for the program to construct `Bar a` in
an example such as this:

    make_Bar : Double -> Int -> a -> Bar a
    make_Bar d i x = Bar d (Foo i x)

which would translate into something like:

```c
void make_Bar(size_t sizeof_a, void *ret, double d, int i, const void *x)
{
    memcpy(ret, &d, sizeof(d));
    memcpy(ret + sizeof(d), &i, sizeof(i));
    memcpy(ret + sizeof(d) + sizeof(i), x, sizeof(*x));
}
```

with the caller responsible for allocating the `ret` as usual:

```c
char ret[sizeof_Bar_a(sizeof_a)];
void make_Bar(sizeof_a, ret, d, i, x);
```
