# reorder-expression

[![License MIT](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![Hackage](https://img.shields.io/hackage/v/reorder-expression.svg)](https://hackage.haskell.org/package/reorder-expression)

A library for reordering expressions in a syntax tree generically according to operator associativity and precedence. This is useful for languages with custom operators which require reordering expressions after collecting their fixities.

Supports:

- Any syntax tree data type, e.g. source position-annotated ones.
- Postfix, prefix, and infix operators, with any arity.
- Left, right, and non-associative operators and precedence with doubles.

See documentation for an example.
