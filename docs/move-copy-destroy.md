# Moving, copying, and destroying

*This section is still a very rough draft.*

We assume that all values with concrete type are movable.

We also assume that all values with polymorphic types are movable too.  This
is not true in the presence of pointers to the stack (such as the *QUPV* idea
in *polymorphic-reification.md*), but it makes our life easier to assume this.
As a result we don't normally allow pointers to the stack to be used:
manipulating them is considered unsafe.

Unlike most languages, we don't assume all values are copyable.  This isn't
merely because we want to avoid garbage collection.  We *could* probably get
away with reference-counted pointers even.  But uncopyable pointers do give us
advantages: it allows mutation while preserving purity, and also forces us to
be cautious about how resources are to be used and consumed.

(Similarly, we don't assume values are destructible.  However, for simplicity
we will assume use the term "copyable" to also imply destructible, even though
technically we could have them be independent.  Perhaps we'll explore them
later -- they are called "affine types" and "relevant types"; there's also
apparently something called "ordered types" that can emulate stacks … but that
might be going too far perhaps?)

In order to make a value copyable, it must be supplied with a `Copyable`
constraint.  Such a constraint must contain a raw `copy` and `destroy`
function:

```c
struct Copyable {
    void *(*copy)(const void *);
    void (*destroy)(void *);
};
```

We assume that `copy` and `destroy` can never fail (or their failures are
sufficiently rare that you may as well just abort; unfortunately, if the
failure modes are of your concern, then you're better off avoiding copyable
types altogether!).  We also assume that the functions do NOT have a context
argument; if they did, they we wouldn't know how to dispose of them!  (Another
chicken-and-egg problem.)

Reference-counted pointers (`SharedPtr`) can be used to obtain a `Copyable`
constraint on a type that doesn't by itself satisfy the constraint:

    # you still need a destructor though;
    # maybe it would be better to have a separate `Destructible` constraint?
    make_shared_ptr : (a -> ()) -> a -> SharedPtr a

    # notice that there is no `Copyable a =>` prerequisite
    instance Copyable (SharedPtr a)

Note that ref-counting is actually somewhat safe from cycles (I think?)
because we don't allow mutations and enforce purity; loophole can only occur
if mutations and/or laziness is introduced.

Example: consider the `const` function:

    # bikeshed: better name for the trait?
    const : Copyable a => (b, a) -> b
    const (x, _) = x

    # after de-generalization and trait reification
    const : (Copyable a, Ptr b, Ptr a) -> Ptr b
    const (i, x, y) = ..
      i.destroy(y)
      x

## Zoology of functions

### Raw function

Can _only_ appear at the top level, just like in C.

### Precious function

Can only be called once.  Not copyable, nor destructible.

### Abundant function

Can be called many times.  Copyable and destructible.

(more…?)
