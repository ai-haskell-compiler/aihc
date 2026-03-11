# Redact

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/redact-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/redact-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/redact.svg)](https://hackage.haskell.org/package/redact)
[![Stackage LTS](https://stackage.org/package/redact/badge/lts)](https://stackage.org/package/redact)
[![Stackage Nightly](https://stackage.org/package/redact/badge/nightly)](https://stackage.org/nightly/package/redact)

* [Overview](#overview)
* [CLI](#cli)
    * [Requirements](#requirements)
    * [Installation](#installation)
        * [`.deb` Package Installation](#deb-package-installation)
        * [`.rpm` Package Installation](#rpm-package-installation)
        * [Installation From Hackage](#installation-from-hackage)
        * [Installation From Stackage](#installation-from-stackage)
    * [Usage](#usage)
* [Project](#project)
    * [Links](#links)
    * [Tags](#tags)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

Redact is a utility for hiding secret text on the terminal.

It is designed to work with Markdown syntax.  Inline code (text enclosed in
backticks) and code blocks that are fenced with backticks are hidden.  Note
that inline code that spans multiple lines is not supported, and other types
of code blocks are not hidden.

Text is hidden by displaying it using the same color for the foreground and
background.  When selecting hidden text, it remains hidden in some terminals
and becomes visible in other terminals.

Be sure to test it in your terminal before relying on it.

## CLI

### Requirements

Redact has only been tested on Linux.  Testing on Windows 10 and macOS will
be added in a future release.

### Installation

#### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

[Releases]: <https://github.com/ExtremaIS/redact-haskell/releases>

#### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

#### Installation From Hackage

Install PhatSort from [Hackage][] using [Cabal][] as follows:

```
$ cabal v2-install redact
```

[Hackage]: <https://hackage.haskell.org/package/redact>
[Cabal]: <https://www.haskell.org/cabal/>

#### Installation From Stackage

Install PhatSort from [Stackage][] using [Stack][] as follows:

```
$ stack install redact
```

[Stackage]: <https://www.stackage.org/package/redact>
[Stack]: <https://haskellstack.org/>

### Usage

See the [`redact` man page][] for usage information.

[`redact` man page]: <doc/redact.1.md>

## Project

### Links

* Hackage: <https://hackage.haskell.org/package/redact>
* Stackage: <https://www.stackage.org/package/redact>
* Flora: <https://flora.pm/packages/@hackage/redact>
* GitHub: <https://github.com/ExtremaIS/redact-haskell>
* GitHub Actions CI: <https://github.com/ExtremaIS/redact-haskell/actions>

### Branches

The `main` branch is reserved for releases.  It may be considered stable, and
`HEAD` is always the latest release.

The `develop` branch is the primary development branch.  It contains changes
that have not yet been released, and it is not necessarily stable.

[Hackage revisions][] are made for metadata changes, such as relaxation of
constraints when new versions of dependencies are released.  The
`redact.cabal` metadata in the `main` branch may therefore not match that of
Hackage.  The `redact.cabal` metadata in the `develop` branch may match,
*unless* work is being done on a new release that contains other changes.

[Hackage revisions]: <https://github.com/haskell-infra/hackage-trustees/blob/master/revisions-information.md#hackage-metadata-revisions--what-they-are-how-they-work>

### Tags

All releases are tagged in the `main` branch.  Release tags are signed using
the [`security@extrema.is` GPG key][].

[`security@extrema.is` GPG key]: <https://keyserver.ubuntu.com/pks/lookup?search=0x1D484E4B4705FADF&fingerprint=on&op=index>

### Contribution

Issues and feature requests are tracked on GitHub:
<https://github.com/ExtremaIS/hr-haskell/issues>

Issues may also be submitted via email to <bugs@extrema.is>.

### License

This project is released under the [MIT License][] as specified in the
[`LICENSE`][] file.

[MIT License]: <https://opensource.org/licenses/MIT>
[`LICENSE`]: <LICENSE>
