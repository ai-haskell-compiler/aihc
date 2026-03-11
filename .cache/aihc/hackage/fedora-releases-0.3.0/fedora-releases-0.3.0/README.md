# fedora-releases

[![Hackage](https://img.shields.io/hackage/v/fedora-releases.svg)](https://hackage.haskell.org/package/fedora-releases)
[![GPL-3 license](https://img.shields.io/badge/license-GPL--3-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/fedora-releases/badge/lts)](http://stackage.org/lts/package/fedora-releases)
[![Stackage Nightly](http://stackage.org/package/fedora-releases/badge/nightly)](http://stackage.org/nightly/package/fedora-releases)
[![GitHub CI](https://github.com/juhp/fedora-releases/workflows/build/badge.svg)](https://github.com/juhp/fedora-releases/actions)

A Haskell library for Fedora release versions
(formerly [fedora-dists](https://hackage.haskell.org/package/fedora-dists))

There are 3 modules:

- Distribution.Fedora.Branch : top level (Branch type)
- Distribution.Fedora.Release : mid level (Release type)
- Distribution.Fedora.BodhiReleases : low level (aeson Object)

It uses the Releases endpoint data from the Fedora Bodhi API
(via [bodhi-hs](https://github.com/juhp/bodhi-hs)).

See <https://hackage.haskell.org/package/fedora-releases> for documentation.

fedora-releases is released and distributed under GPL version 3 or later.

Repository: <https://github.com/juhp/fedora-releases>
