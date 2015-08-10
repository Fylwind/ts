# TS

## Goals

We would like to sketch out a simple language with a linear, higher rank type
system that maps directly to the low-level constructs in C.

- **simplicity**: everything should have a clear mapping to C so that no
  "foreign interface" is required: its ABI is identical to that of C.  It also
  needs to have clear modular boundaries: no inter-module optimization unless
  asked for, as inlining disrupts the modular nature of libraries.

- **linear types**: we *care* whether a variable is used once, never, or
  multiple times.  This allows us to keep aliasing under strict control, which
  gives us a way to manage memory and resources carefully *without a garbage
  collector*.  It also provides a means to *mutate* values while preserving
  purity.

- **records and variants**: kind of like what Elm does.

- **higher rank types**: improves expressivity at a minor cost to type
  inference; needed for expressing scoped references.

- **traits/type classes??**: doubtful we'll explore this very much; the design
  space is very big and it's also a gateway/pandora's box for other advanced
  features.
