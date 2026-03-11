### 0.2.4 [2026.01.10]
* Allow building with `template-haskell-2.24.*` (GHC 9.14).
* Remove unused `ghc-prim` dependency.

### 0.2.3 [2024.10.22]
* Drop support for pre-8.0 versions of GHC.

### 0.2.2
* Add `isKeyCode` function [ryantrinkle]

### 0.2.1
* Bugfix: the keycodes for `-` and `=` were swapped on Gecko-based browsers (e.g., Firefox)

## 0.2
* Add `Data`, `Typeable`, `Generic`, and `Lift` instances for `Key` when possible

### 0.1.1
* Bugfix involving keycode 74 (should be J, was incorrectly giving K) [roboguy13]

## 0.1
* Initial commit
