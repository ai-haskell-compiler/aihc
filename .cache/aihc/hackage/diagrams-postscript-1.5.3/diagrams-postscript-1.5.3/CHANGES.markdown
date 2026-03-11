## [1.5.3](https://github.com/diagrams/diagrams-postscript/tree/v1.5.3) (2025-08-13)

  - Export new functions `renderDia'` and `renderDias'`
  - Improve package documentation

## [1.5.2-r2](https://github.com/diagrams/diagrams-postscript/tree/v1.5.2-r2) (2025-06-02)

  - allow `monoid-extras-0.7`

## [1.5.2-r1](https://github.com/diagrams/diagrams-postscript/tree/v1.5.2-r1) (2025-03-28)

  - allow `base-4.21`, `diagrams-lib-1.5`, and test on GHC 9.12

## [1.5.2](https://github.com/diagrams/diagrams-postscript/tree/v1.5.2) (2024-10-31)

  - Require `data-default-0.8`

## [1.5.1.1-r4](https://github.com/diagrams/diagrams-postscript/tree/v1.5.1.1-r4) (2024-08-23)

  - Allow `hashable-1.5`

## [1.5.1.1-r3](https://github.com/diagrams/diagrams-postscript/tree/v1.5.1.1-r3) (2024-07-02)

  - Allow `base-4.20`, `lens-5.3`, and `containers-0.7`
  - Test on GHC 9.10

## [1.5.1.1-r2](https://github.com/diagrams/diagrams-postscript/tree/v1.5.1.1-r2) (2024-01-20)

  - Allow `base-4.19`, `bytestring-0.12`, and test on GHC 9.8

## [1.5.1.1-r1](https://github.com/diagrams/diagrams-postscript/tree/v1.5.1.1-r1) (2023-06-22)

  - Allow `base-4.18` and test on GHC 9.6

## [1.5.1.1](https://github.com/diagrams/diagrams-postscript/tree/v1.5.1.1) (2023-03-03)

  - Support for `mtl-2.3` (thanks to @FinleyMcIlwaine)

## [1.5.1](https://github.com/diagrams/diagrams-postscript/tree/v1.5.1) (2021-12-29)

  - Bump upper bounds to allow:

      - `base-4.16`
      - `semigroups-0.20`
      - `lens-5.1`
      - `hashable-1.4`

  - Add `Eq` instance for `Options Postscript` (needed for `Hashable`
    instance).

  - r1 (2022-01-03): require `diagrams-lib >= 1.4.5`
  - r2 (2022-09-05): allow `base-4.17`, `lens-5.2`, test on GHC 9.4

## [1.5](https://github.com/diagrams/diagrams-postscript/tree/v1.5) (2019-12-13)

  - Make `Result Postscript V2 Double = Builder`, allowing rendering
    in-memory.
  - Add `Semigroup (Render Postscript V2 Double)` instance.
  - Updated to work with GHC 8.8.  Dropped support for GHC 7.10.

  Hackage revision 1 (2020-02-10):

  - allow `lens-4.19`

  Hackage revision 2 (2021-06-21):

  - `base-4.15` (GHC 9.0)
  - `bytestring-0.11`
  - `diagrams-core-1.5`
  - `monoid-extras-0.6`
  - `lens-5.0`

## [1.4](https://github.com/diagrams/diagrams-postscript/tree/v1.4) (2016-10-26)

No significant changes, bumping version to correspond to diagrams 1.4
release.

  - allow `diagrams-core-1.4`
  - allow `diagrams-lib-1.4`

  Hackage revision 1:

  - allow `base-4.10`

## [1.3.0.7](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.7) (2016-07-20)

- allow `dlist-0.8`

## [1.3.0.6](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.6) (2016-05-01)

- allow `base-4.9`
- allow `data-default-class-0.1`
- test with GHC 8.0.1

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.5...v1.3.0.6)


## [1.3.0.5](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.5) (2016-05-01)

- allow `lens-4.14`

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.4...v1.3.0.5)

## [1.3.0.4](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.4) (2015-11-10)

- allow `semigroups-0.18`

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.3...v1.3.0.4)

## [1.3.0.3](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.3) (2015-09-22)

- allow `lens-4.13` and `semigroups-0.17`

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.2...v1.3.0.3)

## [v1.3.0.2](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.2) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-postscript/compare/v1.3.0.1...v1.3.0.2)

[v1.3.0.1](https://github.com/diagrams/diagrams-postscript/tree/v1.3.0.1) (2015-05-26)
--------------------------------------------------------------------------------------

- allow `lens-4.11`

1.3 (19 April 2015)
-------------------

- Require `diagrams-core-1.3` and `diagrams-lib-1.3`

1.1.0.5 (2 April 2015)
----------------------

 - allow `lens-4.9`
 - allow `vector-space-0.10`

1.1.0.4 (13 Jan 2015)
---------------------

- Allow `vector-space-0.9`
- Allow `lens-4.7`

1.1.0.3 (04 Dec 2014)
---------------------

- Allow `semigroups-0.16`

1.1.0.2 (17 November 2014)
--------------------------

- Allow `lens-4.6`

1.1.0.1 (22 August 2014)
------------------------

    - Allow lens-4.4

1.1 (28 May 2014)
------------------

    - Changes to reflect `Measure` refactoring.
    - Allow `diagrams-core-1.2` and `diagrams-lib-1.2`
    - Allow `semigroups-0.15`
    - Allow `lens-4.2`
    - Allow `mtl-2.2`

1.0.2.4 (10 April 2014)
----------------------

    - Allow `semigroups-0.13`

1.0.2.2 (19 March 2014)
----------------------

  - Allow `lens-4.1`

1.0.2.1 (18 March 2014)
-----------------------

- Allow `dlist-0.7`

1.0.2 (8 March 2014)
--------------------

* **New features**

  - Experimental support for raw CMYK colors.

* **Dependency/version changes**

  - Allow `diagrams-core-1.1` and `diagrams-lib-1.1`.

1.0.1.2 (6 February 2014)
-------------------------

- require diagrams-lib >= 1.0.1 (for Hashable SizeSpec2D instance)

1.0.1.1 (30 January 2014)
-------------------------

- Work around a bug in GHC 7.4.2, which chokes when deriving Generic
  instances for associated data types.

1.0.1 (26 January 2014)
-----------------------

- Add `Hashable (Options Postscript R2)` instance

1.0.0.2 (1 January 2014)
------------------------

- allow semigroups-0.12

1.0.0.1 (30 November 2013)
--------------------------

- Allow dlist-0.6

1.0 (25 November 2013)
----------------------

- Add support for miter limit attribute.
- Use new command-line interface from `diagrams-lib`.
- Export `B` as an alias for `Postscript` token.

0.7.0.2 (27 September 2013)
---------------------------

* allow semigroups-0.11

0.7.0.1 (15 August 2013)
------------------------

* Fix deprecation warning

0.7: 9 August 2013
------------------

First release as an officially supported diagrams backend.

* Simple animation support.
* Upgrade to `monoid-extras-0.3`.
* Allow `base-4.7`.
