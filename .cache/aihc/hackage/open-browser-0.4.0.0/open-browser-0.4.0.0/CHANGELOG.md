# Changelog for `open-browser`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.4.0.0 - 2025-04-02

* On Unix-like operating systems, use Haskell to silence the opening
  application or script, rather than the `sh` shell on Linux and BSD.
* Add utility `openBrowserWithExitCode`, to help with debugging failure.
* Improve Haddock documentation.

## 0.3.0.1 - 2025-03-17

* On Windows, remove unnecessary dependency on `process`.
* Improve Haddock documentation.

## 0.3.0.0 - 2025-03-16

* Make operating system-related choices at compilation. The function will never
  succeed on unsupported operating systems, rather than than throwing an
  `ErrorCall` exception.
* Drop support for 32-bit Windows.

## 0.2.1.1 - 2025-03-15

* Add `CHANGELOG.md` and `README.md`.
* Add `stack.yaml` and `stack.yaml.lock` to package description.
* Example executable renamed `open-browser-example` (from `example`).
* The building of the example executable now requires Cabal flag `example`
  (default: false).
* URL in example executable updated to https://www.haskell.org/.

## 0.2.1.0 - 2016-01-05

* As released by rightfold on Hackage. This entry and prior change log is
  reconstructed.
* Support GHC versions before GHC 8.0, by removing the Byte Order Mark from
  source files.
* Add support for 32-bit Windows, by using the `stdcall` calling convention.

## 0.2.0.0 - 2015-07-31

* Remote deprecated `Network.Browser.openBrowser`.

## 0.1.4.0 - 2015-07-30

* On Linux and BSD, silence the `xdg-open` script using the `sh` shell.

## 0.1.3.0 - 2015-07-27

* Add support for BSD, using the `xdg-open` script.
* Add support for Windows, using the Win32 API and the `ccall` calling
  convention.

## 0.1.2.0 - 2015-07-27

* `Web.Browser.openBrowser` added and `Network.Browser.openBrowser` deprecated.

## 0.1.1.0 - 2015-07-27

* Add support for Linux, using the `xdg-open` script.
* On OS X, use the `open` application.

## 0.1.0.0 - 2015-07-25

* Initial version. Only OS X supported, using `open location` in an AppleScript.
