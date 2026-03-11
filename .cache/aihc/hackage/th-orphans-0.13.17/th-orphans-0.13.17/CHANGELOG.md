### 0.13.17 [2026.01.10]
* Allow building with `template-haskell-2.24.*` (GHC 9.14).
* Remove unused `ghc-prim` dependency in the test suite.

### 0.13.16 [2024.12.05]
* Support building with `template-haskell-2.23.0`.

### 0.13.15 [2024.10.22]
* Support building with `template-haskell-2.22.1`.
* Drop support for pre-8.0 versions of GHC.

### 0.13.14 [2022.08.21]
* Only depend on `th-lift-instances` for `ghc <= 7.8`.

### 0.13.13 [2022.05.19]
* Implement `qGetPackageRoot` (introduced in `template-haskell-2.19.0.0`)
  for the `Quasi` instances defined in `th-orphans`.

### 0.13.12 [2021.08.30]
* Implement `qGetDoc` and `qPutDoc` (introduced in `template-haskell-2.18.0.0`)
  for the `Quasi` instances defined in `th-orphans`.

### 0.13.11 [2020.09.29]
* Allow building with `template-haskell-2.17.0.0` (GHC 9.0).
* Define `Quote` instances for `ReaderT`, `StateT`, `WriterT`, and `RWST`. In
  order to define these instances on as many versions of `template-haskell` as
  possible, this library now depends on the `th-compat` library, which
  backports the `Quote` class to older versions of `template-haskell`.
* Backport the `Semigroup`, `Monoid`, and `MonadFix` instances for `Q` that
  were introduced in `template-haskell-2.17.0.0`.

### 0.13.10 [2020.04.13]
* Implement `liftTyped` in the `Lift Bytes` instance.

### 0.13.9 [2019.09.28]
* Implement `qReifyType` (introduced in `template-haskell-2.16.0.0`) for the
  `Quasi` instances defined in `th-orphans`.

### 0.13.8 [2019.09.04]
* Backport the `Bounded` instance for `Extension`
  (from `Language.Haskell.TH.LanguageExtensions`), which was introduced in
  `template-haskell-2.15.0.0`.

### 0.13.7 [2019.03.24]
* Backport the `MonadFail Q` instance.
* Allow building with `template-haskell-2.16` by manually implementing
  `Lift` for `Bytes`. See [#27]

[#27]: https://github.com/mgsloan/th-orphans/issues/27

### 0.13.6 [2018.07.01]
* Allow building with `template-haskell-2.14`.
* Implement `qAddForeignFilePath` and `qAddTempFile` for `Quasi` instances

### 0.13.5 [2018.01.18]
* Implement `qAddCorePlugin` for `Quasi` instances

### 0.13.4 [2017.07.26]
* Implement `qAddForeignFile` for `Quasi` instances

### 0.13.3 [2016.11.09]
* Backport `Generic NameFlavour` instance
