# Revision history for ghc-trace-events

## v0.1.2.10 - 2025-04-05

* Allow base version from GHC 9.12 ([#16](https://github.com/maoe/ghc-trace-events/pull/16))

## v0.1.2.9 - 2024-05-18

* Bump base bounds to support ghc 9.10 ([#15](https://github.com/maoe/ghc-trace-events/pull/15))

## v0.1.2.8 - 2024-01-18

* Bump base, bytestring and text version bounds, include recent GHCs in CI config ([#13](https://github.com/maoe/ghc-trace-events/pull/13))

## v0.1.2.7 - 2023-05-11

* Allow GHC 9.6.1 ([#11](https://github.com/maoe/ghc-trace-events/pull/11))

## v0.1.2.6 - 2022-08-11

* Bump base upper bound to < 4.18 ([#10](https://github.com/maoe/ghc-trace-events/pull/10))

## v0.1.2.5 - 2022-03-06

* Allow text-2.0 ([#9](https://github.com/maoe/ghc-trace-events/pull/9))

## v0.1.2.4 - 2021-12-09

* Relax upper version bound for base to support GHC 9.2.1

## v0.1.2.3 - 2021-06-28

* Relax upper version bound for tasty-bench ([#6](https://github.com/maoe/ghc-trace-events/pull/6))

## v0.1.2.2 - 2021-02-26

* Support GHC 9.0.1 ([#5](https://github.com/maoe/ghc-trace-events/pull/5))
* Switch from Travis CI to GitHub Actions
* Switch from criterion to tasty-bench

## v0.1.2.1 - 2020-06-09

* Delete some haddock comments that are not applicable to this library

## v0.1.2 - 2020-06-09

* Revert "Use unsafeUseAsCString in place of useAsCString in Debug.Trace.Text" because it's unsafe
* Minimize the scope of NOINLINE to get better performance when user tracing is disabled

## v0.1.1 - 2020-06-07 (abondoned)

* Remove redundant checks of userTracingEnabled
* Optimize Debug.Trace.Text

## v0.1.0.1 - 2020-04-06

* Support GHC 8.10.1
* Minor documentation updates

## v0.1.0 - 2019-11-20

* Add traceBinaryEvent and traceBinaryEventIO
* Switch to cabal-version: 2.2
* Rename Debug.Trace.Internal to Debug.Trace.Flags
* Test with GHC 8.8.1 and 8.6.5
* Update docs

## v0.0.0.1 - 2018-08-07

* Test with GHC 8.4.3 and HEAD
* Update docs

## v0.0.0 - 2018-05-21

* First version
