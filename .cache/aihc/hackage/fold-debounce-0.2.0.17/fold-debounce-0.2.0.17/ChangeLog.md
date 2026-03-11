# Revision history for fold-debounce

## 0.2.0.17  -- 2026-01-08

* Confirm test with `ghc-9.14.1`.

## 0.2.0.16  -- 2025-03-28

* Improve the code, haddock and .cabal file based on hlint and cabal check ( https://github.com/debug-ito/fold-debounce/pull/4 )

## 0.2.0.15  -- 2025-03-19

* Confirm test with `ghc-9.12.1`.

## 0.2.0.14  -- 2024-11-01

* Remove broken links to `Control.Debounce` module. (#3)

## 0.2.0.13  -- 2024-10-31

* Now it depends on `data-default` instead of `data-default-class`.
  This is because `data-default-0.8` now includes and deprecates `data-default-class`.
  See also: https://github.com/commercialhaskell/stackage/issues/7545

## 0.2.0.12  -- 2024-09-13

* Update dependency version bounds with cabal-plan-bounds.
  This adds support for new packages, while drops support for some old ones.

## 0.2.0.11  -- 2023-04-05

* Confirmed test with `ghc-9.4.4` and `time-1.12`.

## 0.2.0.10  -- 2022-11-24

* Confirmed test with `ghc-9.2.5` and `time-1.11`.

## 0.2.0.9  -- 2019-10-04

* Confirmed test with `time-1.9.3`.

## 0.2.0.8  -- 2018-09-23

* Confirmed test with `stm-2.5.0.0`.


## 0.2.0.7  -- 2018-03-14

* Confirmed test with `hspec-2.5.0`.
  Remove its dependency upper bound, because I think it's stable enough.


## 0.2.0.6  -- 2017-07-21

* Confirmed test with `time-1.8`.
* Drop version bounds for `hspec`. It's stable.


## 0.2.0.5  -- 2017-01-24

* Confirmed test with `hspec-2.4.0`.


## 0.2.0.4  -- 2016-11-27

* Confirmed test with `time-1.7`.


## 0.2.0.3  -- 2016-10-09

* Confirmed build with `hspec-2.3.0`.


## 0.2.0.2  -- 2016-05-22

* Confirmed build with `data-default-class-0.1.0` and `base-4.9.0.0`.


## 0.2.0.1  -- 2016-05-02

* Loosen dependency on `data-default` to `data-default-class`.
* Supports `time-1.6` and `hspec-2.2.3`.
* Fix test error in Mac OS X.


## 0.2.0.0  -- 2015-06-01

* The debounce period now starts from the time when the first input
  event is sent. Previously it was the time when the first input event
  is popped from the input queue. That lead to weird behavior when the
  input traffic was too intense.

* Now this module works without -threaded ghc option thanks to new
  dependency on 'stm-delay' package.

* Bumped the major version due to the new dependency.


## 0.1.0.0  -- 2015-05-22

* First version. Released on an unsuspecting world.
