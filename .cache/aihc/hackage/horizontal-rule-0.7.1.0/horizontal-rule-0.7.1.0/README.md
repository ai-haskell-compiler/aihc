# hr

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![GitHub CI](https://github.com/ExtremaIS/hr-haskell/workflows/CI/badge.svg?branch=main)](https://github.com/ExtremaIS/hr-haskell/actions)
[![Hackage](https://img.shields.io/hackage/v/horizontal-rule.svg)](https://hackage.haskell.org/package/horizontal-rule)
[![Stackage LTS](https://stackage.org/package/horizontal-rule/badge/lts)](https://stackage.org/package/horizontal-rule)
[![Stackage Nightly](https://stackage.org/package/horizontal-rule/badge/nightly)](https://stackage.org/nightly/package/horizontal-rule)

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
* [Library](#library)
* [Project](#project)
    * [Links](#links)
    * [Tags](#tags)
    * [Contribution](#contribution)
    * [License](#license)

## Overview

`hr` is a utility for displaying a horizontal rule in a terminal.

It is useful for marking a position in your terminal so that you can easily
find it again.  For example, use `hr` to display a horizontal rule before each
build of a project so that you can easily find the beginning of the output of
the last build.

## CLI

### Requirements

`hr` has only been tested on Linux.  It *might* work on Windows and macOS.

### Installation

#### `.deb` Package Installation

Check the [Releases][] page for `.deb` packages.

[Releases]: <https://github.com/ExtremaIS/hr-haskell/releases>

#### `.rpm` Package Installation

Check the [Releases][] page for `.rpm` packages.

#### Installation From Hackage

Install `hr` from [Hackage][] using [Cabal][] as follows:

```
$ cabal v2-install horizontal-rule
```

[Hackage]: <https://hackage.haskell.org/package/horizontal-rule>
[Cabal]: <https://www.haskell.org/cabal/>

#### Installation From Stackage

Install `hr` from [Stackage][] using [Stack][] as follows:

```
$ stack install horizontal-rule
```

[Stackage]: <https://www.stackage.org/package/horizontal-rule>
[Stack]: <https://haskellstack.org/>

### Usage

See the [`hr` man page][] for usage information.

[`hr` man page]: <doc/hr.1.md>

#### Examples

The rule fills with width of the terminal by default:

```
$ hr
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

In cases when the terminal width cannot be determined, a default width is
used.  This default width can be set with an option:

```
$ hr -d 78
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

If desired, the rule width can be specified:

```
$ hr -w 60
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

In cases where the terminal cannot display Unicode, ASCII may be used:

```
$ hr -a
------------------------------------------------------------------------------
```

The rule can include the current time:

```
$ hr -t
━━┫2021-05-27 19:26:09┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

The time format can be specified:

```
$ hr -t -f "%H:%M:%S.%q"
━━┫19:30:44.861779179000┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

The first line read from `STDIN` can be used as a note:

```
$ uname -m | hr -i
━━┫x86_64┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

When input is read, a timeout is used to ensure that `hr` does not "hang" when
there is no input.  The timeout (in milliseconds) can be specified:

```
$ uname -m | hr -i --timeout 100
━━┫x86_64┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

A note can be specified as one or more arguments:

```
$ hr unit tests
━━┫unit tests┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

Different types of notes can be combined:

```
$ uname -m | hr -it unit tests
━━┫2021-05-27 19:48:48┣━┫unit tests┣━┫x86_64┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
```

## Library

You can easily include horizontal rules in the output of your own Haskell
software by using the library.  The Haskell package is named `horizontal-rule`
in [Hackage][] and [Stackage][] because there is an existing package named
[`hR`](https://hackage.haskell.org/package/hR).

## Project

### Links

* Hackage: <https://hackage.haskell.org/package/horizontal-rule>
* Stackage: <https://www.stackage.org/package/horizontal-rule>
* Flora: <https://flora.pm/packages/@hackage/horizontal-rule>
* GitHub: <https://github.com/ExtremaIS/hr-haskell>
* GitHub Actions CI: <https://github.com/ExtremaIS/hr-haskell/actions>

### Branches

The `main` branch is reserved for releases.  It may be considered stable, and
`HEAD` is always the latest release.

The `develop` branch is the primary development branch.  It contains changes
that have not yet been released, and it is not necessarily stable.

[Hackage revisions][] are made for metadata changes, such as relaxation of
constraints when new versions of dependencies are released.  The
`horizontal-rule.cabal` metadata in the `main` branch may therefore not match
that of Hackage.  The `horizontal-rule.cabal` metadata in the `develop` branch
may match, *unless* work is being done on a new release that contains other
changes.

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
