# Egison Pattern Source

[![Actions Status](https://github.com/egison/egison-pattern-src/workflows/latest/badge.svg)](https://github.com/egison/egison-pattern-src/actions?workflow=latest)
[![Actions Status](https://github.com/egison/egison-pattern-src/workflows/release/badge.svg)](https://github.com/egison/egison-pattern-src/actions?workflow=release)
[![Hackage](https://img.shields.io/hackage/v/egison-pattern-src.svg)](https://hackage.haskell.org/package/egison-pattern-src)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/egison-pattern-src.svg)](http://packdeps.haskellers.com/reverse/egison-pattern-src)

The [egison-pattern-src](https://hackage.haskell.org/package/egison-pattern-src) provides a standalone syntax definition for patterns in [the Egison programming language](https://www.egison.org/).
This package enables you to embed Egison's patterns in your parser (pretty-printer) by supplying expression and name parsers (printers) externally.

## Syntax

The following is a simplified syntax of pattern expressions where `v`, `c`, `op` and `e` are meta-variables for variable names, constructor names, user-defined infix operators, and expressions in host language, respectively.

```
p ::= _                     (wildcard pattern)
    | $v                    (pattern variable)
    | #e                    (value pattern)
    | ?e                    (predicate pattern)
    | (p_1, p_2, ..., p_n)  (tuple pattern)
    | [p_1, p_2, ..., p_n]  (collection pattern)
    | p & p                 (and-pattern)
    | p | p                 (or-pattern)
    | !p                    (not-pattern)
    | p op p                (user-defined infix pattern)
    | c p_1 p_2 ... p_n     (constructor pattern)
```

## License

[egison-pattern-src](https://hackage.haskell.org/package/egison-pattern-src) is distributed as open source software under the terms of the 3 clause BSD License. See `LICENSE` for details.
