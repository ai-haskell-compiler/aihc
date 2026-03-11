# componentm-devel

> A library that enhances componentm to work nicely with GHCi and ghcid

## Table Of Contents

* [Installation](#installation)
* [Purpose](#purpose)
 * [Differences with ResourceT](#differences-with-resourcet)
* [Development](#development)
* [Documentation](#documentation)
* [License](#license)

## Installation

[![Hackage](https://img.shields.io/hackage/v/componentm.svg)](https://img.shields.io/hackage/v/componentm.svg)
[![Stackage LTS](http://stackage.org/package/componentm/badge/lts)](http://stackage.org/lts/package/componentm)
[![Stackage Nightly](http://stackage.org/package/componentm/badge/nightly)](http://stackage.org/nightly/package/componentm)

Make sure you include the following entry on your [cabal file's
dependecies](https://www.haskell.org/cabal/users-guide/developing-packages.html#build-information)
section.

```cabal
library:
  build-depends:
      componentm
    , componentm-devel
```

Or on your `package.yaml`

```
dependencies:
- componentm
- componentm-devel
```

## Purpose

This library enhances the
[componentm](https://github.com/Haskell-componentm/componentm) library to
dispose and re-allocate an application on a REPL environment.

## Development
[![Build Status](https://travis-ci.org/roman/Haskell-componentm.svg?branch=master)](https://travis-ci.org/roman/Haskell-componentm)
[![Hackage Dependencies](https://img.shields.io/hackage-deps/v/componentm-devel.svg)](http://packdeps.haskellers.com/feed?needle=componentm-devel)
[![Github](https://img.shields.io/github/commits-since/roman/haskell-componentm/v0.0.0.2.svg)](https://img.shields.io/github/commits-since/roman/haskell-componentm/v0.0.0.2.svg)

This library is intended to be minimal, providing a few functions that work
reliably among many different kind of projects. If you want to contribute, Pull
Request are very welcome! Please try to follow these simple rules:

* Please create a topic branch for every separate change you make.
* Update the README.md file if necessary.
* Please _do not_ change the version number on your Pull Request.

### Open Commit Bit

This project has an open commit bit policy: Anyone with an accepted pull request
gets added as a repository collaborator. Please try to follow these simple
rules:

* Commit directly onto the master branch only for typos, improvements to the
  README and documentation.
* Create a feature branch and open a pull-request early for any new features to
  get feedback.
* Make sure you adhere to the general pull request rules above.

## License

Copyright (c) 2017-current Roman Gonzalez

Permission to use, copy, modify, and/or distribute this software for any
purpose with or without fee is hereby granted, provided that the above
copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
