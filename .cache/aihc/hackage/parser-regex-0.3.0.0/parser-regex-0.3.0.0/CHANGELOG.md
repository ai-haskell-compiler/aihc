### 0.3.0.0 -- 2025-04-19

* Breaking changes—`Internal` modules only
  * Changes to internal representations
* Performance improvements
  * Fix a pessimization due to boxity analysis on GHC >= 9.4. Improves parsing
    time by up to 23%.

### 0.2.0.2 -- 2025-03-15

* Compatibility with [MicroHs](https://github.com/augustss/MicroHs)
* Performance improvements
  * Avoid some closure allocations, improving parsing time by up to 10%.
* Fix `compileBounded`'s behavior on negative bounds

### 0.2.0.1 -- 2024-12-25

* Documentation improvements

### 0.2.0.0 -- 2024-11-24

* Breaking changes
  * Parsing fails more eagerly. This affects lazy list parsing and parsing via
    the `Regex.Base` functions `prepareParser` and `stepParser`.
* Additions
  * Added `Regex.Base.parseNext`.
* Other
  * Some internal modules are now exported.

### 0.1.0.0 -- 2024-03-04

* First version.
