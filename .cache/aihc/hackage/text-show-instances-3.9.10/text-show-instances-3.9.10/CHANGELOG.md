### 3.9.10 [2024.11.05]
* Support building with `quickcheck-instances-0.3.32` or later in the test
  suite.

### 3.9.9 [2024.10.23]
* Support building with GHC 9.12.
* Drop support for GHC 8.0.

### 3.9.8 [2024.04.20]
* Support building with GHC 9.10.
* Add a `TextShow` instance for `NamespaceSpecifier` in
  `TextShow.Language.Haskell.TH` when building with `template-haskell-2.22.0.0`
  (GHC 9.10.1) or later.

### 3.9.7 [2023.10.11]
* Add `TextShow(1)` instances for `aeson`'s `KeyMap` type.
* Add `TextShow(1)` instances for `NonEmptyVector` from the `nonempty-vector`
  library.

### 3.9.6 [2023.08.06]
* Support building with GHC 9.8.
* Add `TextShow` instance for `BndrVis` in `TextShow.Language.Haskell.TH`.

### 3.9.5 [2023.03.05]
* Allow building with `unix-2.8.*`.

### 3.9.4 [2022.12.30]
* Allow building with `aeson-2.1.*`.

### 3.9.3 [2022.12.28]
* Add `TextShow` instances for `Value` and `Key` from `aeson`.
* Drop support for GHC 7.8 and 7.10.

### 3.9.2 [2022.10.05]
* Require `text-show-3.10` or later in the test suite.

### 3.9.1 [2022.07.28]
* Add a `TextShow` instance for `UUID` from `uuid-types`.

## 3.9 [2022.05.28]
* Remove instances for `ErrorT` and `ListT`, which had long been deprecated and
  were finally removed from the `transformers` library in version 0.6.0.0.

### 3.8.6 [2021.11.21]
* Add a `TextShow` instance for `Scientific` from the `scientific` package.
* Require `quickcheck-instances-0.3.27` or later in the test suite.

### 3.8.5 [2021.10.31]
* Allow building with GHC 9.2.
* Require `quickcheck-instances-0.3.26` or later in the test suite.

### 3.8.4 [2020.10.03]
* Allow building with `template-haskell-2.17.0.0` (GHC 9.0).

### 3.8.3 [2019.11.08]
* Make the `TextShow1` and `TextShow2` instances for `Fix` and `Join` in
  `TextShow.Data.Bifunctor` match the behavior the corresponding `Show1` and
  `Show2` instances in `bifunctors`.

### 3.8.2 [2019.09.03]
* Make the test suite compile on GHC 8.8.1.

### 3.8.1 [2019.06.03]
* Require `quickcheck-instances-0.3.21` or later.

## 3.8 [2019.05.02]
* Support GHC 8.8.
* Add a `TextShow` instance for `ShortText` (from the `text-short` package) in
  `TextShow.Data.ShortText`.
* Drop support for `base-4.5` and `base-4.6` (GHC 7.4 and 7.6, respectively).
* `liftShowbTaggedPrec` is no longer exported from `TextShow.Data.Tagged`. Use
  `Tagged`'s `TextShow1` instance instead.
* `showbVectorGenericPrec` and `liftShowbVectorGenericPrec` are no longer
  exported from `TextShow.Data.Vector`. Use the various `TextShow(1)` instances
  defined in `TextShow.Data.Vector` instead.
* Require `vector-0.12` or later.

## 3.7 [2018.10.07]
* Remove `hoopl` dependency. Instances are no longer provided for data types
  in the `hoopl` library.
* Depend on `QuickCheck-2.12` or later.
* Depend on `quickcheck-instances-0.3.19` or later.

### 3.6.5 [2018.07.03]
* Fix the tests on GHC 8.6.

### 3.6.4 [2018.04.07]
* Use `base-compat-batteries`.

### 3.6.3 [2018.03.15]
* GHC 8.4 fixes

### 3.6.2 [2017.06.18]
* Require `QuickCheck-2.10`/`quickcheck-instances-0.3.16` or later

### 3.6.1 [2017.06.04]
* Drop support for GHC 7.0 and 7.2
* Allow building with `quickcheck-instances-0.3.14`

## 3.6 [2017.04.22]
* New `TextShow` instances:
  * `directory`: `XdgDirectory` (from `System.Directory`)
  * `ghc-boot-th`: `ForeignSrcLang` (from `GHC.ForeignSrcLang.Type`)
  * `time`: `UniversalTime` (from `Data.Time.Clock`) and `SystemTime` (from `Data.Time.Clock.System` with `time-1.8` or later)
  * `Win32`: Lots of datatypes added in `Win32-2.5.0.0`
* Remove most monomorphic `TextShow` functions, as their utility is questionable, and their maintenance burden is undeniable
* Made the instances in `TextShow.Data.Bifunctor` poly-kinded when possible
* Fix testsuite compilation on older GHCs

## 3.5 [2017.01.07]
* Add `TextShow(1)` instances for `Data.Graph.SCC`
* `TextShow.Instances` no longer reexports the entirety of `TextShow`. Doing so meant that `text-show-instances` would be burdened with bumping its major version number every time that `text-show` made an API change in `TextShow` in order to follow the PVP.
* Remove `TextShow.GHC.PackageDb` module
* Refactor test suite

## 3.4
* Require `text-show-3.4` or later
* Update testsuite to be buildable with `text-show-3.4`

## 3.3
* Add the `TextShow.GHC.LanguageExtensions.Type` and `TextShow.GHC.PackageDb` modules, which define instances if using `ghc-boot`
* Add `TextShow` instances for `NameFlavour`, `NameSpace`, `PatSynArgs`, and `PatSynDir` in `TextShow.Language.Haskell.TH`
* Require `text-show-3.3` or later, which has slightly different TH derivation behavior. As a result, the context for the `TextShow1` instance for `Clown` in `TextShow.Data.Bifunctor` had to be changed slightly.
* Allow building with `QuickCheck-2.9`
* Fix GHC HEAD build

### 3.2.1
* Fix build with GHC 8.0
* Add `TextShow` instance for `Overlap` in `TextShow.Language.Haskell.TH`

## 3.2
* Allow build with `text-show-3.2`

### 3.0.1
* Require quickcheck-instances >= 0.3.12 in test suite due to presence of new orphan Arbitrary instances for `vector` datatypes

# 3
* GHC 8.0 support
* Rename functions that previously ended with the suffix `-With` to instead have the prefix `lift-`, consistent with `text-show-3`
* Removed the `TextShow.Data.Semigroup` and `TextShow.Data.List.NonEmpty` modules, as they have been moved to `text-show-3` (as part of moving `Semigroup` into `base`)
* Removed the functions for `Compose`, `Product`, and `Sum` in `TextShow.Data.Functor.Trans`, as they have been moved to `text-show-3` (as part of moving them to `base`)
* Add `TextShow`/`TextShow1` instances for `Fix` and `Sum` in `TextShow.Data.Bifunctor`
* Add `TextShow`/`TextShow1` instances for the datatypes in `Text.PrettyPrint.Annotated` (introduced in @pretty-1.1.1.3@)
* Add `TextShow` instances for the new datatypes in @template-haskell-2.11.0.0@

## 2.1
* Reexport the `TextShow` classes and module from `TextShow.Instances`. This helps Haddock readers discover what new instances are added with `text-show-instances`.
* Make `Tagged` instances poly-kinded

## 2.0.1
* Allow building with `vector-0.11` and above. Be aware that the `Show` instances for `Vector` types in `vector-0.11.0` are different from other versions of `vector`.

# 2
* Changed the top-level module name from `Text.Show.Text` to `TextShow`, since the former was extremely verbose without much benefit. As a result, this will break all existing code that depends on `text-show-instances`.

# 1
* Allow building with `text-show-1`. Also changed the monomorphic functions to match the naming conventions introduced in `text-show-1`.
* Added instances for the `bifunctors` library
* Removed `utf8-string` instance, since it wasn't as useful as I had imagined
* Revamped test suite

# 0.4
* Allow building with `text-show-0.8`
* Modules which were previously exported only if using a recent-enough version of GHC/`base` (e.g., `Text.Show.Text.System.Win32`) are now always exposed. If the functionality that the module provides is not available on a given version of GHC/`base`, the module will not expose anything.
* Change test-suite to use `hspec`, allowing for it to be built on GHC 7.0 and 7.2

### 0.3.0.1
* Fixed tests on Windows

# 0.3
* Added `showbArgPrec` (and corresponding `Show` and `Show1` instances) to `Text.Show.Text.Data.Semigroup`
* Added `Show1` instances for data types in `Text.Show.Text.Data.List.NonEmpty` and `Text.Show.Text.Data.Semigroup`
* Added `showbDoc` (and corresponding `Show` instance) to `Text.Show.Text.Language.Haskell.TH`
* Renamed `showbDoc` in `Text.Show.Text.Text.PrettyPrint` to `renderB`

# 0.2
* Exposed `showbKindPrec` and `showbPredPrec` with later versions of Template Haskell
* Added `renderStyleB` to `Text.Show.Text.Text.PrettyPrint` (for `time-1.5` and later)
* Added `showbTimeLocalePrec` to `Text.Show.Text.Data.Time`
* Added instances for the `binary`, `haskeline`, `hoopl`, `terminfo`, and `utf8-string` libraries
* Removed `transformers-four` flag

# 0.1
* Initial commit
