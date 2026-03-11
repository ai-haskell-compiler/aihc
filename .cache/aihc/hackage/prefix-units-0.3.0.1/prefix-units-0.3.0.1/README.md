prefix-units package
====================

This package defines a datatype (`Unit`) and associated
parsing/formatting functions so that command line applications can
handle "nice" values like:

    $ cmd create-file foo 100G
    Done.
    $ cmd ls-file foo
    Size is 100Gi
    $ cmd ls-files
    foo 100Gi
    bar  14Ki

And so on. For details on the API, look at the Haddock documentation
for the `Data.Prefix.Units` module.

For building and installing, `cabal configure` and related commands
are enough. Run `cabal configure --enable-tests && cabal build &&
cabal test` if you want to run the unit-tests.

The library is designed to have very few dependencies (only base and a
few GHC extensions), so that it's trivial to use it in projects. Hence
the use of some hand-coded conversions instead of using
TemplateHaskell to generate them automatically.

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/iustin/prefix-units/ci.yml?branch=master)](https://github.com/iustin/prefix-units/actions/workflows/ci.yml)
[![Codecov](https://img.shields.io/codecov/c/github/iustin/prefix-units)](https://codecov.io/gh/iustin/prefix-units)

TODO
----

The current interface of the library works, but is not nicely
composable. I'm still looking for a nicer way to expose the parsing
functionality.

Currently, the IEC (binary) and SI units are mixed in the same
data-type. This works, but at some level I think two separate types
would be more "correct", at the expense of a more complex API.

The RationalConvertible type class has only a few instances; ideally
we'd have `instance Integral a => RationalConvertible a` and similar
for `Fractional`, but this doesn't work as such in Haskell, so we're
stuck with the manual derivation.

The current behaviour is case-sensitive for all units in `ParseExact`
mode, which means that one has to use (in this mode) `Ki` for the
binary unit `Kibi`. This seems suboptimal, since the binary units are
unique irrespective of casing.
