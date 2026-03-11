isbn: ISBN Validation and Manipulation
======================================

[![Build Status](https://github.com/charukiewicz/hs-isbn/workflows/CI/badge.svg)](https://github.com/charukiewicz/hs-isbn/actions?query=workflow%3ACI)
[![Hackage](http://img.shields.io/hackage/v/isbn.svg)](https://hackage.haskell.org/package/isbn)
[![Stackage LTS](http://stackage.org/package/isbn/badge/lts)](http://stackage.org/lts/package/isbn)
[![Stackage Nightly](http://stackage.org/package/isbn/badge/nightly)](http://stackage.org/nightly/package/isbn)
[![Apache-2.0](https://img.shields.io/badge/license-Apache--2.0-blue)](https://www.apache.org/licenses/LICENSE-2.0.html)

All books published by major publishers since 1970 have an International Standard Book Number (ISBN) associated with them. Prior to 2007, all ISBNs issued were ten digit ISBN-10 format. Since 2007, new ISBNs have been issued in the thirteen digit ISBN-13 format. See the [ISBN Wikipedia article](https://en.wikipedia.org/wiki/International_Standard_Book_Number) for more information.

## Overview

This library provides data types and functions both validating and working with ISBNs. For general use, only importing the `Data.ISBN` module is required, as it reexports all functionality for validating and converting between ISBN-10 and ISBN-13 values. The specific implementations for validation are located in the `Data.ISBN.ISBN10` and `Data.ISBN.ISBN13` modules, respectively.

## Usage Example: Validating an ISBN and printing the error

```haskell
import Data.ISBN (ISBN, validateISBN, renderISBNValidationError)

validateUserSuppliedISBN :: Text -> Either Text ISBN
validateUserSuppliedISBN userIsbnInput =
    either (Left . renderISBNValidationError) Right (validateISBN userIsbnInput)


someValidISBN10 =
    validateUserSuppliedISBN "0345816021"    -- Right (ISBN10 "0345816021")

someValidISBN13 =
    validateUserSuppliedISBN "9780807014295" -- Right (ISBN13 "9780807014295")

tooShortISBN =
    validateUserSuppliedISBN "0-345-816"     -- Left "An ISBN must be 10 or 13 characters, not counting hyphens"

invalidISBN10 =
    validateUserSuppliedISBN "0-345-81602-3" -- Left "The supplied ISBN-10 is not valid"
```


## Development

This library is developed using a [Nix](https://nixos.org/nix/) shell. The environment is specified in `shell.nix`. You can enter the environment with `nix` installed via `nix-shell`. The `nix-shell` will install sandboxed copies of `cabal-install`, `ghcid`, `entr`, and `gnumake`, which are the utilities necessary to build the project, run the tests, and create a local copy of the documentation.

#### Entering the development environment and runnning tests

```
$ cd <path-to-repo>
$ nix-shell                       # assumes `nix` is installed
(nix-shell) $ make help           # show all of the make targets
(nix-shell) $ make tests-watch    # build the library and run the tests in `ghcid`
(nix-shell) $ make docs           # build a local copy of the haddock documentation
```
