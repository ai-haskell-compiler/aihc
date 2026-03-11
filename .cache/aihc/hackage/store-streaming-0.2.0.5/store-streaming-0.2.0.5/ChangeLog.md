# ChangeLog

## 0.2.0.3

* Test compilation fixed with `network >= 3`.

## 0.2.0.2

* Now only depends on `fail` / `semigroups` shim for `ghc < 8`.

## 0.2.0.1

* Now builds with GHC-7.10 - compatibility was broken in 0.6.0 due to
  the fix for GHC-8.8. See
  [#146][https://github.com/fpco/store/issues/146].

## 0.2.0.0

* Now builds with GHC-8.8. This is a major version bump because
  MonadFail constraints were added to some functions, which is
  potentially a breaking change.

## 0.1.0.0

* `Data.Store.Streaming` forked from `store-0.4.3.2`
