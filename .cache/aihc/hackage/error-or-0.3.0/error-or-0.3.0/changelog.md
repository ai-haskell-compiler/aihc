## 0.3.0 (2022-06-16)
  * tagIO now wraps any non async exception, used to wrap only `IOExceptions` and
    user errors (from `error` function)
  * compatiblity with mtl-2.3
## 0.2.0.0 (2021-06-24)
  * Renames to reduce the likelihood of name collisions on importing the module unqualified
    - rename `err` to `failText`, to suggest the use of `MonadFail.fail`
    - prepend `ErrAcc` constructors with 'Err' prefix
    - rename `pretty` to `prettyErrAcc`
  * Define a `MonadError` instance for `ErrorOr`

## 0.1.2.0 (2020-12-16)

  * Mark the `OK` and `Error` patterns of `ErrorOr` complete for the exhaustiveness checker

## 0.1.1.0 (2020-12-02)

  * Add `tagIO`
