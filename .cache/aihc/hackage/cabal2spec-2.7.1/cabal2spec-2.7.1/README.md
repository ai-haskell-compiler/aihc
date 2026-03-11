cabal2spec
==========

[![hackage release](https://img.shields.io/hackage/v/cabal2spec.svg?label=hackage)](http://hackage.haskell.org/package/cabal2spec)
[![stackage LTS package](http://stackage.org/package/cabal2spec/badge/lts)](http://stackage.org/lts/package/cabal2spec)
[![stackage Nightly package](http://stackage.org/package/cabal2spec/badge/nightly)](http://stackage.org/nightly/package/cabal2spec)
[![CI Status](https://github.com/peti/cabal2spec/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/peti/cabal2spec/actions/workflows/haskell-ci.yml)

The `cabal2spec` utility converts a given Cabal file into an [RPM spec
file](http://ftp.rpm.org/max-rpm/s1-rpm-build-creating-spec-file.html) that
builds that package with the RPM package manager. This tool is used to generate
various Haskell package sets for the [SUSE](http://www.suse.com/) and
[openSUSE](http://www.opensuse.org) familiy of distributions. Plenty of
examples for generated spec files can be found in the [openSUSE Haskell
development
project](https://build.opensuse.org/project/show/devel:languages:haskell).
