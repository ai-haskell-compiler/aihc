# Changelog for shake-plus

## v0.3.4.0

* Fix for GHC 9's simplified subsumption change.

## v0.3.3.0

* Re-export `RuleResult` and `ShakeValue`.

## v0.3.2.0

* Re-export `Development.Shake.Forward`.

## v0.3.1.0

* Remove unnecessary comonad dependency.

## v0.3.0.0

* Drop dependency on path-like.
* Drop dependency on within.
* Drop `Development.Shake.Plus.Loaders` module.
* Restore monomorphic `Path` variants of everything.
* Move `within` style combinators to [shake-plus-extended](https://hackage.haskell.org/package/shake-plus-extended).

## v0.2.0.0

* Add dependency on [path-like](https://hackage.haskell.org/package/path-like).
* Update interface to use `FileLike` and `DirLike` and drop `Path` and `Within` specific variants.

## v0.1.10.0

* Add unlifted versions of `parallel`, `forP` and `par`.

## v0.1.9.0

* Add lower bound to aeson.
* Remove hashable dependency.

## v0.1.8.0

* Reexport `Development.Shake.Classes` and `Development.Shake.RuleResult`.

## v0.1.7.0

* Add `runSimpleShakePlus` for quickrunning with args and a log function.

## v0.1.6.0

* Re-export `Path` and `Within`.

## v0.1.5.0

* Add `getDirectoryFilesWithinIO` and `getDirectoryFilesWithinIO'`

## v0.1.4.0

* Re-export `Development.Shake.shakeArgs`.

## v0.1.3.0

* Re-export `Development.Shake.Command` with lifted `command` and `command_`.

## v0.1.2.0

* Add `copyFileWithin'` and `copyFileChangedWithin`.

## v0.1.1.0

* Make `Within` style functions more consistent in that they actually take `Within` values
  across the board.
* Make `batchLoad` functions more consistent and usable.
* `need` and `want` variants now take any `Traversable`.

## v0.1.0.0

* Update to `within-0.1.1.0` which changes the data type to an `Env` comonad.
* Drop the `loadSortFilterApply` loader functions in favour of a simpler
  `batchLoad` set of functions which accepts a loading function which can be
  cached.
* Add enough documentation to get started.

## v0.0.2.1

* Add functions based on [Within](https://hackage.haskell.org/package/path) to better
  keep track of source and target directories.
* Reexport more shake functions to remove need to import vanilla shake qualified.
* Add convenience functions `loadSortFilterApply` and `loadSortFilterApplyW` for batch
  loading via `MonadAction`.
* Add more of the API surface with `Path` and `Within` including variants of `need`, `want`
  `readFile`, `writeFile`.

## v0.0.1.0

* Initial sketch of shake-plus with reexported functions. Mostly oracles,
  filepaths and directory functions using
  [Path](https://hackage.haskell.org/package/path), and `MonadAction`,
  `MonadUnliftAction` and `MonadRules` with `ReaderT` transformers in a similar
  style to [RIO](https://hackage.haskell.org/package/rio)
