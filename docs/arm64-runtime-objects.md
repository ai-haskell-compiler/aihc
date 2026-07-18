# ARM64 runtime objects

Native heap objects use a one-word tagged header followed by shape-specific
payload words. The low three header bits are the physical tag. The remaining
bits contain either an aligned function entry address or an immediate info
identifier.

```text
saturated constructor: [header] [fields...]
thunk:                 [header] [environment...]
partial application:   [header] [shape] [fields...]
indirection:           [header] [target]
blackhole:             [header] [unused]
```

The shape word exists only for closures, partial constructors, and primitives.
It packs the remaining logical arity and current field count into one word.
Saturated constructors derive their field count from constructor metadata;
thunks derive it from their function signature.

Every updateable object reserves at least two words. Evaluating a thunk changes
its header to `BLACKHOLE`, executes the entry encoded by its old header, then
changes the same object to `INDIRECTION` and writes the returned heap pointer
into its first payload word. There is no separate cell allocation.

Exceptions have no native heap tag or object representation. They are removed
before native runtime lowering. Unused tag values remain reserved.
