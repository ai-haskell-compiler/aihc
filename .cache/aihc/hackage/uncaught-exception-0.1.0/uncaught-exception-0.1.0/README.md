# uncaught-exception

[![GitHub CI](https://github.com/serokell/uncaught-exception/workflows/CI/badge.svg)](https://github.com/serokell/uncaught-exception/actions)
[![Hackage](https://img.shields.io/hackage/v/uncaught-exception.svg)](https://hackage.haskell.org/package/uncaught-exception)
[![Stackage LTS](http://stackage.org/package/uncaught-exception/badge/lts)](http://stackage.org/lts/package/uncaught-exception)
[![Stackage Nightly](http://stackage.org/package/uncaught-exception/badge/nightly)](http://stackage.org/nightly/package/uncaught-exception)
[![License: MPL 2.0](https://img.shields.io/badge/License-MPL%202.0-brightgreen.svg)](https://github.com/serokell/o-clock/blob/master/LICENSE)

The goal of this small library is to make it easy to customize handling of uncaught exceptions in Haskell applications.
The default handler of uncaught exceptions uses the `Show` type class to print the exception.
However, in some cases you may want to use `displayException` which renders the exception value in a human-friendly manner.
You can find more details about motivation and implementation of this library in its Haddock documentation and [this blog post](https://serokell.io/blog/uncaught-exception-handling).

## For Contributors

Please see [CONTRIBUTING.md](.github/CONTRIBUTING.md) for more information.

## About Serokell

`uncaught-exception` is maintained and funded with ❤️ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
