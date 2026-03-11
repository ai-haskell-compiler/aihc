# Revision history for error

## 1.0.0.0 -- 2022-10-09

* No changes, just signify that the library is stable now.

## 0.3.0.0 -- 2022-02-16

* Add `IsString` instance for `Error`.

* Breaking: Rename `addContext` to `errorContext`.

  Since `Data.Error` is expected to be imported unqualified,
  `addContext` was not super obviously related to error handling.
  `errorContext` is a lot clearer.
  This is hopefully the last big breaking change before `1.0`.

## 0.2.1.2 -- 2021-11-15

* Fix doctests.

## 0.2.1.1 -- 2021-11-14

* Make compatible with GHC 8.0.2 again by not using `<&>`.

## 0.2.1.0 -- 2021-11-13

* Add `expectErrorIO` and `unwrapErrorIO`.
* Add `ifIOError` and `ifError`.

## 0.2.0.0 -- 2021-10-29

* Add `Show` instance to `Error`.

## 0.1.1.0 -- 2021-10-28

* Add `HasCallStack` to `expectError` and `unwrapError`.
* Add `showToError` and `exceptionToError`.
* Improved documentation of existing functions.

## 0.1.0.0 -- 2021-06-26

* Initial version. More or less feature complete,
  unless we decide we need extra helper functions.
