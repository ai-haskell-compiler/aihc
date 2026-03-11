# bm

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/bm-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/bm-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/bm.svg)](https://hackage.haskell.org/package/bm)
[![Stackage LTS](https://stackage.org/package/bm/badge/lts)](https://stackage.org/package/bm)
[![Stackage Nightly](https://stackage.org/package/bm/badge/nightly)](https://stackage.org/nightly/package/bm)

* [Overview](#overview)
* [CLI](#cli)
    * [Requirements](#requirements)
    * [Installation](#installation)
        * [`.deb` Package Installation](#deb-package-installation)
        * [`.rpm` Package Installation](#rpm-package-installation)
        * [Installation From Hackage](#installation-from-hackage)
        * [Installation From Stackage](#installation-from-stackage)
    * [Usage](#usage)
        * [Examples](#examples)
* [Project](#project)
    * [Links](#links)
    * [Tags](#tags)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

`bm` is a utility for opening bookmarks and queries from the command line.
The bookmarks and queries are configured hierarchically in YAML, and they are
referenced using keyword prefixes.  It allows you to quickly open bookmarks
and perform search queries in your browser using only your keyboard.

## CLI

### Requirements

`bm` is designed to work on any operating system, but it has only been tested
on Linux.

### Installation

#### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

[Releases]: <https://github.com/ExtremaIS/bm-haskell/releases>

#### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

#### Installation From Hackage

Install `bm` from [Hackage][] using [Cabal][] as follows:

```
$ cabal v2-install bm
```

[Hackage]: <https://hackage.haskell.org/package/bm>
[Cabal]: <https://www.haskell.org/cabal/>

#### Installation From Stackage

Install `bm` from [Stackage][] using [Stack][] as follows:

```
$ stack install bm
```

[Stackage]: <https://www.stackage.org/package/bm>
[Stack]: <https://haskellstack.org/>

### Usage

Bookmarks and search queries are configured using a hierarchy of keywords in a
YAML configuration file.  They are selected using keyword prefixes as
command-line arguments.  Command-line completion is available for Bash.  See
the [`bm` man page][] for details.

Note that the command used to open bookmarks and queries can be customized in
the configuration file.  Specifying the command for your specific browser can
make links open considerably faster than with the default (generic) command.

[`bm` man page]: <doc/bm.1.md>

#### Examples

An example configuration file is available in the `config` directory on
GitHub (example for the latest release: [bm.yaml][]).  The following are
example commands using this configuration file.

Open a bookmark link by specifying keywords.  The Nix homepage can be opened
with the following command:

```
$ bm nix
```

The `nixpkgs` manual can be opened using the following command:

```
$ bm nix pkgs manual
```

Keyword prefixes can also be used.  The following command also opens the
`nixpkgs` manual:

```
$ bm n p m
```

When more than one keyword has the same prefix, the first is selected.  If a
selected bookmark does not specify a URL, the first child is processed.

The `/` keyword defines a bookmark with a URL as well as a query.  When no
query is specified, the URL is opened, allowing you to search with suggestions
using the search field.  Open a query by specifying keywords followed by the
query.

```
$ bm / LinearTypes
```

When multiple query arguments are provided, they are joined with a space.

[bm.yaml]: <https://github.com/ExtremaIS/bm-haskell/blob/main/config/bm.yaml>

## Project

### Links

* Hackage: <https://hackage.haskell.org/package/bm>
* Stackage: <https://www.stackage.org/package/bm>
* Flora: <https://flora.pm/packages/@hackage/bm>
* GitHub: <https://github.com/ExtremaIS/bm-haskell>
* GitHub Actions CI: <https://github.com/ExtremaIS/bm-haskell/actions>

### Branches

The `main` branch is reserved for releases.  It may be considered stable, and
`HEAD` is always the latest release.

The `develop` branch is the primary development branch.  It contains changes
that have not yet been released, and it is not necessarily stable.

[Hackage revisions][] are made for metadata changes, such as relaxation of
constraints when new versions of dependencies are released.  The `bm.cabal`
metadata in the `main` branch may therefore not match that of Hackage.  The
`bm.cabal` metadata in the `develop` branch may match, *unless* work is being
done on a new release that contains other changes.

[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

### Tags

All releases are tagged in the `main` branch.  Release tags are signed using
the [`security@extrema.is` GPG key][].

[`security@extrema.is` GPG key]: <https://keyserver.ubuntu.com/pks/lookup?search=0x1D484E4B4705FADF&fingerprint=on&op=index>

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/bm-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the [MIT License][] as specified in the
[`LICENSE`][] file.

[MIT License]: <https://opensource.org/licenses/MIT>
[`LICENSE`]: <LICENSE>
