# Revision history for tasty-checklist

## 1.0.8.0 -- 2025-10-25

  * Limit diff output to 100 lines and align output.

## 1.0.7.0 -- 2025-10-24

  * Update constraints and flake to support GHC 8.10 -- GHC 9.12.

## 1.0.6.0 -- 2023-04-14

  * Update output formatting to show common input trigger only once for a group
    of check failures rather than on each failure.
  * Small formatting updates to check failure messages.
  * The API is compatible, but any operation checking the text of the output
    messages will need to be updated.

## 1.0.5.1 -- 2023-03-31

  * Remove constraint bounds for GHC builtin libraries.
  * Bump doctest constraints to allow 0.21 release series.

## 1.0.5.0 -- 2023-01-07

  * Enhanced display of multi-line differences to make whitespace more visible.

## 1.0.4.1 -- 2022-12-26

  * Bump upper bound of doctest to enable GHC 9.4 support.

## 1.0.4.0 -- 2022-08-09
  * Added explicit LICENSE file
  * Updated failure messages for clarity and readability
  * Misc. Haddock documentation updates.

## 1.0.3.0 -- 2021-08-13
  * Bump doctest upper constraints (contributed by Felix Yan)
  * Added missing doctest source
  * Added `Observe` to `DerivedVal` to allow using a user-supplied
    observation function on failure.
  * Added `multiLineDiff` helper function

## 1.0.2.0 -- 2021-07-25
  * Added `Got` specifier for simpler specification of boolean
    `DerivedVal` than using `Val`.
  * Better clarity in failure messages.
  * Cleaned up documentation and added doctest for ensuring
    documentation samples are accurate.

## 1.0.1.0 -- 2021-06-27

* ASCII output, no UTF-8.
* More information on failures regarding input and value obtained from
  input relative to expected value.

## 1.0.0.0 -- 2021-04-20

* Initial version.
