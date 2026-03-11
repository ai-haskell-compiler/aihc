### 0.2.2 [2019.04.28]
* `mtl-compat` now uses automatic flags instead of manual ones. This has a
  number of benefits:

  * There is no need for making several simultaneous releases to support each
    flag combination.
  * As a result, the `cabal-install` constraint solver should have a much
    easier time figuring out install-plans involving `mtl-compat`.

  Due to old `cabal-install` bugs, `cabal-install-1.16` and older may have a
  harder time installing this package, so it is recommended that you use
  `cabal-install-1.18` or later. (Or, if you must use `cabal-install-1.16` or
  older, installing `mtl-compat` with the appropriate flags should help.)

### 0.2.1.3
* A specific build with no flags explicitly enabled, intended for use with the latest version of `mtl`. This is a workaround for `cabal` backtracker bugs.

### 0.2.1.2
* A specific build with the `-ftwo-point-two` flag explicitly enabled. This is a workaround for `cabal` backtracker bugs.

### 0.2.1.1
* A specific build with the `-ftwo-point-one` flag explicitly enabled. This is a workaround for `cabal` backtracker bugs.

## 0.2.1
* Require use of `transformers-compat-0.4` or greater when building with `mtl-2.1.3.1` or earlier. `transformers-compat-0.4.0.*` adds the missing `ExceptT` instances, which means that `mtl-compat`'s only purpose is to backport the `Control.Monad.Except` module for those who want an `mtl`-style import for `ExceptT` and/or `MonadError`.

  I would recommend just using `Control.Monad.Trans.Except` from `transformers-compat-0.4.0.*` and `Control.Monad.Error.Class` instead, since they accomplish the same thing as `mtl-compat` without an extra dependency.

## 0.1.1
* Allowed the `two-point-one` flag to toggle on/off automatically

# 0.1
* Initial commit
