
[![Hackage](https://img.shields.io/hackage/v/swish.svg)](https://hackage.haskell.org/package/swish)
[![Pipeline status](https://gitlab.com/dburke/swish/badges/main/pipeline.svg)](https://gitlab.com/dburke/swish/-/commits/main)

# Introduction

Swish - which stands for Semantic Web Inference Scripting in Haskell -
was written by Graham Klyne as a framework, written in the purely
functional programming language Haskell, for performing deductions in
RDF data using a variety of techniques. Swish was conceived as a
toolkit for experimenting with RDF inference, and for implementing
stand-alone RDF file processors (usable in similar style to CWM, but
with a view to being extensible in declarative style through added
Haskell function and data value declarations). One of the aims was to
explore Haskell as "[a scripting language for the Semantic
Web](http://www.ninebynine.org/RDFNotes/Swish/Intro.html)".

It was updated from version 0.2.1 by Vasili I Galchin so that it would
build with the current version of GHC, and
[released on Hackage](http://hackage.haskell.org/package/swish-0.2.1).

Since then it has been updated to take advantage of recent
developments in the Haskell ecosystem, add support for the NTriples
and Turtle serialisation formats, and a number of convenience
functions. Development is done on GitLab at https://gitlab.com/dburke/swish
and the previous [bitbucket site](https://bitbucket.org/doug_burke/swish/)
is now *outdated* (you may also find a version on GitHub which should
also be ignored).

I attempt to keep Swish buildable on recent GHC versions but it is done
on a best-effort basis, so support for "older" versions of GHC is not
guaranteed.

# Aim

Current development has essentially stalled - I was using this as a
RDF library for I/O with limited querying rather than for inferencing
or use as a flexible graph-processing library (e.g. for extensions to
non-RDF models) - but that project has stopped.

# Copyright

    (c) 2003, 2004 G. Klyne
    (c) 2009 Vasili I Galchin
    (c) 2011 - 2024, 2026 Doug Burke

All rights reserved.

# License

[LGPL V2.1](https://gitlab.com/dburke/swish/raw/master/LICENSE)

# Haskell and the Semantic Web 

Other Haskell packages for RDF support include

 * [rdf4h](http://hackage.haskell.org/package/rdf4h)
 * [hsparql](http://hackage.haskell.org/package/hsparql)
 * [hasparql-client](http://hackage.haskell.org/package/hasparql-client)

# Installation

The following commands will install a command-line tool `Swish` along
with the modules in the `Swish` namespace; documentation can be found 
[on Hackage](http://hackage.haskell.org/package/swish).

## With cabal

Install a recent version of the [Haskell
platform](http://hackage.haskell.org/platform/) and then try

    % cabal update
    % cabal install swish

## With stack

Swish is available as part of the stackage curated package set.

There are several stack configuration files, for different GHC
versions:

    % cd swish
    % stack install
    % STACK_YAML=stack-9.10.yaml stack install   # this uses a nightly install
    % STACK_YAML=stack-9.8.yaml stack install
    % STACK_YAML=stack-9.6.yaml stack install
    % STACK_YAML=stack-9.4.yaml stack install
    % STACK_YAML=stack-9.2.yaml stack install
    % STACK_YAML=stack-9.0.yaml stack install
    % STACK_YAML=stack-8.10.yaml stack install
    % STACK_YAML=stack-8.8.yaml stack install
    % STACK_YAML=stack-8.6.yaml stack install

I do not guarantee they will all work.

## With nix

There is now support for building with the [nix](https://nixos.org/nix/)
package manager:

    % nix-shell
	...
	nix-shell% cabal test

or

    % nix-shell --argstr compiler ghc921
	...
	nix-shell% cabal test

or, with a flake, either of

    % nix build

    % nix develop
    swish:13:13  /path/to/swish cabal test
