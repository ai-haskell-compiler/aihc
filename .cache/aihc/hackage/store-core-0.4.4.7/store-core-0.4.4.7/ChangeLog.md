# ChangeLog

## 0.4.4.4

* Fixes build of `store-core` with `ghc-prim-0.7.0`
  (`ghc-9.0.0-alpha1`). See [#155][].

[#155]: https://github.com/mgsloan/store/issues/155

## 0.4.4.3

* Now only depends on `fail` shim for `ghc < 8`.

## 0.4.4.2

* Build fixed for GHC-7.10. See
  [#146](https://github.com/fpco/store/issues/146)

## 0.4.4.1

* Now builds with GHC-8.8.

## 0.4.4

* Build fixed with  `--flag store-core:force-alignment` / on architectures
  like PowerPC.

## 0.4.3

* Now builds with primitive >= 0.6.4.0

## 0.4.2

* Adds `unsafeMakePokeState`, `unsafeMakePeekState`, and
  `maybeAlignmentBufferSize`, so that library users can write their own
  `encode` / `decode` functions.
  See [#126](https://github.com/fpco/store/pull/126)

## 0.4.1

* Less aggressive inlining, resulting in faster compilation / simplifier
  not running out of ticks

## 0.4

* Changes result of Peek function to be strict.
  (See [#98](https://github.com/fpco/store/pull/98))

## 0.3

* Adds support for alignment sensitive architectures, by using temporary buffers
  when necessary. This required changing the type of both Poke and Peek. Most
  user code should be unaffected, but this is still a breaking change.

## 0.2.0.1

* Fixes a bug that could result in segfaults when reading corrupted data.

## 0.2.0.0

* First public release
