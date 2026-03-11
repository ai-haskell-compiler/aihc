# path-like - Type classes for the Path library.

This library exports type classes `PathLike`, `FileLike` and `DirLike` which serve as a
common interface for compiling types down to `Path b File` and `Path b Dir`.
The only contract that should be respected is that your type should be at least
as strict as the `Path` type itself, i.e no String/FilePath instances. This is
the case with the [within](https://hackage.haskell.org/package/within) type.

This library also exports a concatenation function `(/>)` which defers to the
underlying `(</>)` function in `Path`, but works for any `DirLike` and
`PathLike Rel`. This may change to shadow the existing definition if these
classes turn out to be generally useful. They tend to be particularly useful for
generalising file IO, but you may find you need to cast more when you first
construct a `Path` using TemplateHaskell.

For more information see the [path](https://hackage.haskell.org/package/path)
library documentation.
