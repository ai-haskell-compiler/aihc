Change log for `crypton-pem`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.3.0

* Change default language to `Haskell2010`.
* Remove deprecated `PEM` instance of type class `NormalForm` and the direct
  dependency on the `basement` package.
* Depend on the `base64` package, remove direct dependency on the `memory`
  package and the indirect dependency on the `basement` package.

## 0.2.5

* Move library modules to directory `src`.
* Rename test-suite module as `test/Main.hs`.
* Add `CHANGELOG.md` and `README.md` to package.
* Make `PEM` an instance of type class `NFData`.
* Deprecate `PEM` instance of type class `NormalForm`.

## 0.2.4

* Rename `pem-0.2.4` package as `crypton-pem-0.2.4`.
* Change maintainer field to `Mike Pilgrem <public@pilgrem.com>` and
  `Kazu Yamamoto <kazu@iij.ad.jp>`.
* Add `CHANGELOG.md`.
* Cabal file specifies `cabal-version: 1.12` (not `>= 1.8`).
* Cabal file specifies expressly `default-language: Haskell98`
