See also http://pvp.haskell.org/faq

# Revision history for `resolv`

## 0.2.0.3

_2025-08-31, Andreas Abel_

* Drop support for GHC 7
* Tested with GHC 8.0 - 9.14 alpha1

## 0.2.0.2

_2023-06-12, Alexey Radkov and Andreas Abel_

* Support Haiku OS by including `libnetwork` in configure script.
  (PR [#23](https://github.com/haskell-hvr/resolv/pull/23).)

## 0.2.0.1

_2023-03-31, Alexey Radkov and Andreas Abel_

* Fix 0.2.0.0: Ship updated `configure` script.

## 0.2.0.0

_2023-03-31, Alexey Radkov and Andreas Abel_

* Bump `bytestring` to `>= 0.10` for correct `IsString ByteString` instance.
  (PR [#16](https://github.com/haskell-hvr/resolv/pull/16).)
* Fix memory leaks due to missing `res_nclose()` after each `res_ninit()` call.
  (PR [#12](https://github.com/haskell-hvr/resolv/pull/12).)
* Check the value of `h_errno` on failures of `res_nquery()` and throw an
  appropriate exception of type `DnsException` built with one of new
  constructors `DnsHostNotFound`, `DnsNoData`, `DnsNoRecovery`, or `DnsTryAgain`.
  Note that previously such exceptions were thrown by `fail` and had type `IOError`.
  (PR [#17](https://github.com/haskell-hvr/resolv/pull/17).)
* Suppress configure warning on option `--with-compiler` passed by Cabal.
  (PR [#21](https://github.com/haskell-hvr/resolv/pull/21).)
* Tested with GHC 8.0 - 9.6.

## 0.1.2.0

_2020-03-27, Herbert Valerio Riedel_

* Add new high-level API functions `queryPTR`, `arpaIPv4`, and
  `arpaIPv6` for performing reverse address lookups.

## 0.1.1.3

_2019-08-26, Herbert Valerio Riedel_

* GHC 8.8 / `base-4.13` only compat hotfix release; the next release will support
  older GHC/`base` versions again.

## 0.1.1.2

_2018-10-27, Herbert Valerio Riedel_

* Clarify/relax licensing terms.

## 0.1.1.1

_2017-10-26, Herbert Valerio Riedel_

* Improve Autoconf script.

## 0.1.1.0

_2017-10-22, Herbert Valerio Riedel_

* Use Autoconf to detect which library (if any) to link for `res_query(3)`.
* Use reentrant `res_nquery(3)` API if available and signal via new `resIsReentrant :: Bool` constant.
* Expose `DnsException` and `QR`.

## 0.1.0.0

_2017-10-22, Herbert Valerio Riedel_

* First version. Released on an unsuspecting world.
