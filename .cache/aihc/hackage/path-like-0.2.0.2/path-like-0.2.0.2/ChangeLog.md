# Changelog for path-like

## (v0.2.0.0)

* Revert ConstraintKinds mess.

## (v0.1.1.0)

* Add 'PathLike` superclass to `FileLike` and `DirLike`.

## (v0.1.0.0)

* Add `FileLike` and `DirLike` type class for using stricter types as `Path b File` and `Path b Dir` respectively.
* Add `(/>)` combinator for producing a `Path` from a `DirLike` and a `FileLike Rel`.
