## rio-prettyprint

`rio-prettyprint` is a Haskell package that provides a library that combines the
logging capabilities of the
[`rio` package](https://hackage.haskell.org/package/rio) with the pretty
printing capabilities of the
[`annotated-wl-pprint` package](https://hackage.haskell.org/package/annotated-wl-pprint).

Documents can be annotated with an optional style from a set of named styles,
and the style associated with each named style can be specified as a list of
ANSI Select Graphic Rendition (SGR) commands. These commands are represented by
the constructors of a type provided by the
[`ansi-terminal-types` package](https://hackage.haskell.org/package/ansi-terminal-types)

The library also provides:

* a type that represents a simple, non-customisable environments that provide
  pretty logging functionality; and
* a type that represents pretty exceptions.
