# Changelog for `aeson-warning-parser`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1.1 - 2023-12-07

* `...:` and `...:?` no longer smother `fail` messages if a single key is
  present in the object.

## 0.1.0 - 2023-07-08

* Spin out module `Pantry.Internal.AesonExtended` from package `pantry-0.8.3`.
