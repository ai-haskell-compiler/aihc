Change log
==========

teardown uses [Semantic Versioning][1].
The change log is available [on GitHub][2].

[1]: http://semver.org/spec/v2.0.0.html
[2]: https://github.com/roman/Haskell-teardown/libraries/teardown/CHANGELOG.md

## v0.0.0.2

* Add `pretty-show` dependency
* Improve upon pretty instances of types from this library
* Expose internal modules for flexibility

## v0.0.0.1

* Relax lower version bounds for `prettyprinter`, `containers`, `exceptions`,
  `deepseq`, `tasty`, `tasty-hunit` dependencies

## v0.0.0.0

* Add `ComponentM` type, which supports `Monad` and `Applicative`
* Add `buildComponent` to build components in IO with attached cleanup functions
* Add `buildComponent_` to build components in IO without cleanup functions
* Add `withComponentM` to run an application
