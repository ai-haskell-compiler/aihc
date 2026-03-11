### 0.1.7 [2026.01.03]
* Allow building with GHC 9.14.

### 0.1.6 [2024.12.05]
* Drop support for pre-8.0 versions of GHC.

### 0.1.5 [2024.03.19]
* Support building with `template-haskell-2.22.*` (GHC 9.10).

### 0.1.4 [2022.07.23]
* Backport the `getPackageRoot` and `makeRelativeToProject` functions
  introduced in `template-haskell-2.19.0.0` (GHC 9.4).
* Implement `qGetPackageRoot` in the `Quasi` instance for `QuoteToQuasi` when
  building with `template-haskell-2.19.0.0` (GHC 9.4) or later.

### 0.1.3 [2021.08.29]
* Implement `qGetDoc` and `qPutDoc` in the `Quasi` instance for `QuoteToQuasi`.
* Add `expToSplice`.

### 0.1.2 [2021.03.12]
* Add `bindSplice`, `bindSplice_`, `examineSplice`, `joinSplice`,
  `hoistSplice`, `liftSplice`, and `unTypeSplice` to
  `Language.Haskell.TH.Syntax.Compat`.

### 0.1.1 [2021.02.07]
* Mark `Language.Haskell.TH.Syntax.Compat` as `Trustworthy`.

## 0.1 [2020.09.29]
* Initial release
