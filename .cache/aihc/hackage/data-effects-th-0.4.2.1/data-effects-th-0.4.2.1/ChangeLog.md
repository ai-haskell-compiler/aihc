# Revision history for data-effects-th

## 0.1.0.0 -- 2023-09-18

* Initial public release.

## 0.1.1.0 -- 2024-09-01
* Fix an issue where constraints in the effect constructor were not properly reflected in 'genLiftFOEPatternSynonyms'.
* Fixed an issue where the auto-generation would fail when the name of the effect constructor started with a colon (:) operator.

## 0.2.0.0 -- 2024-10-10
* Support for the core version upgrade to 0.2.
    * Support for GHC 9.8.2.

## 0.4.0.0 -- 2025-04-16

* Adopt to the new v4 interface.
    * Unified first-order and higher-order effect interfaces.
    * Added a generic `Eff` carrier type.
