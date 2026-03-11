# Revision history for data-effects-core

## 0.1.0.0 -- 2023-09-18

* Initial public release.

## 0.2.0.0 -- 2024-10-10

* Remove the type operator for HCont.
* Rename the terms "instruction" and "signature" to the simpler "FOE" and "HOE".
* Support for GHC 9.8.2.

## 0.4.0.0 -- 2025-04-16

* The new v4 interface.
    * Unified first-order and higher-order effect interfaces.
    * Added a generic `Eff` carrier type.

## 0.4.3.0 -- 2025-05-21

* Add type classes for weaving
* Fix the missing type role specification in `Data.Effect.OpenUnion`
