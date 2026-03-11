# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 2.2.0.0 -- 2024-10-05

* Similarly to the change in 2.1.0.0, change the order of the type arguments to `SmallUnliftedArray_` and
  `SmallMutableUnliftedArray_`. This makes the library work better with
  the typeclasses in the `contiguous` library.

## 2.1.0.0 -- 2023-06-28

* Change the order of the type arguments to `UnliftedArray_` and
  `MutableUnliftedArray_`. This makes the library work better with
  the typeclasses in the `contiguous` library.

## 2.0.0.0 -- 2023-06-27

* Use legitimate unlifted primitive types and operations, only supporting
  GHC 9.4 and newer.

## 1.0.0.0 -- 2020-11-02

* Redo everything. This uses `unsafeCoerce#` a lot to coerce between
  lifted and unlifted types. Stay on the 0.1.x.x series unless you need
  something from this newer version.

## 0.1.3.0 -- 2020-01-23

* Add `PrimUnlifted` instances for `ShortText` and `ShortByteString`.
* Add `singletonUnliftedArray`.

## 0.1.2.0 -- 2019-07-08

* Add `PrimUnlifted` instances for `MVar`, `IORef`, and `STRef`.

## 0.1.1.0 -- 2019-05-23

* Cannot remember what happened in this release.

## 0.1.0.0 -- 2019-05-17

* First version. Released on an unsuspecting world.
