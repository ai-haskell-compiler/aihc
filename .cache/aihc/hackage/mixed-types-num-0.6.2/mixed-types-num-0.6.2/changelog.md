# mixed-types-num change log

* v 0.6.2 2024-10-05
  * Ring now requires HasIntegersWithSample instead of HasIntegers
  * Field now requires HasRationalsWithSample instead of HasRationals
  * all test suites require HasIntegersWithSample instead of HasIntegers
* v 0.5.12 2023-08-14
  * compatible with ghc 9.6.2
  * remove dependency on mtl
* v 0.5.11 2022-08-25
  * left-first and/or for CE/CN monad
* v 0.5.10 2022-07-13
  * isValid and spec helpers for validity of operations
* v 0.5.9 2021-08-04
  * compatible with ghc 9.0.1
  * separated module Mul from Ring
* v 0.5.8 2021-06-02
  * add HasRationals to Field
* v 0.5.7 2021-05-28
  * before: n^m is rational, now: n^m is integer, n^^m is rational
* v 0.5.6 2021-05-27
  * add instances: mixed min/max Double $t
  * add instance: CanGiveUpIfVeryInaccurate Double
* v 0.5.5 2021-05-26
  * powUsingMulRecip etc with custom multiply and recip operations
* v 0.5.4 2021-05-21
  * remove Kleeneans (move them to aern2-mp)
* v 0.5.3 2021-05-15
  * export clearPotentialErrors (from collect-errors)
* v 0.5.2 2021-05-14
  * add OrdGenericBool
* v 0.5.1 2021-05-12
  * if-then-else for CN-wrapped (see collect-errors) condition
  * Documentation now in README
* v 0.5.0 2021-04-13
  * use package collect-errors with a much simpler CN wrapper
  * replace Maybe Bool by Kleenean (a new type)
  * remove very long type constraints in specifications using PartialTypeSignatures
* v 0.4.1 2021-01-21
  * add hasErrorCE and hasErrorCN for testing if CE/CN values contain errors
* v 0.4.0.2 2020-08-02
  * remove smallcheck version upper bound
  * update to cabal-version >= 1.10
* v 0.4.0.1 2019-04-11
  * fix infinite loop in some conversions
* v 0.4.0 2019-04-10
  * eliminated dependency on convertible, improving ghcjs compatibility
* v 0.3.2 2019-01-08
  * added divI and mod
  * added enforceRange
  * used enforceRange in exp tests
* v 0.3.1.5 2018-11-14
  * improved documentation
* v 0.3.1.4 2017-12-06
  * removed upper bounds for dependencies
* v 0.3.1.3 2017-08-22
  * fixed bound on base in test suite
* v 0.3.1.2 2017-08-15
  * provided compatible versions of take, drop, length, replicate, splitAt
  * added missing mixed-type subtraction combination to Ring etc.

* v 0.3.0.1 2017-08-01
  * improve package documentation in module MixedTypesNumPrelude

* v 0.3 2017-08-01
  * renamed the main module to MixedTypesNumPrelude
  * much faster compilation
  * Ring and Field are now classes, not synonyms for large sets of constraints
  * many fixes in collect-error framework and its use in division and power
  * Overloaded if-then-else via -XRebindableSyntax
  * compiling with ghc 8.2.1

* v 0.2.0.1
  * fix compilation bug in test suite
  * minor doc improvements
  * fix Complex instances of error-throwing division (/!)

* v 0.2
  * new CollectErrors wrapper
  * CN, specialisation of CollectErrors to NumErrors
  * numerical partial operators (eg division) return a CN type
  * instances for Data.Complex

* v 0.1
  * first release
