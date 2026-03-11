0.8.3 [2024.12.06]
------------------
* Drop support for pre-8.0 versions of GHC.

0.8.2 [2022.05.07]
------------------
* Allow building with `mtl-2.3.*`.

0.8.1 [2018.04.24]
------------------
* Support `exceptions-0.10.0`.

0.8
---
* `reflection` 2 support
* Fixed missing `duplicate` implementation in the `Comonad` instance.
* Compile warning-free on GHC 7.10

0.7.1
-----
* Support for `contravariant` 1.0

0.7
---
* Enable `-XPolyKinds`
* Added the Tagged monad synonym
* Added useful functions currently in Data.Tagged for both Tagged and TaggedT

0.6.5
---
* Relaxed `mtl` upper bound
* Removed unused `transformers` dependency

0.6.4
---
* Allow exceptions 0.6

0.6.3
---
* Removed unused `array` dependency

0.6.2
---
* Allow exceptions 0.4

0.6.1
---
* Update dependencies

0.6
---
* Removed `self`/`selfM`. Use `pure` and `return`.

0.5
---
* Updated dependencies
* Added `uninterruptiblMask` to the `MonadCatch` instance.

0.4.1
---
* Build fixes for GHC < 7.6

0.4
---
* Added `MonadCatch` instance.

0.3.1
-----
* Marked the package `Trustworthy`.
