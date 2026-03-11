# Commutative Semigroup

[![Haskell](https://img.shields.io/badge/language-Haskell-orange.svg)](https://haskell.org) [![Hackage](https://img.shields.io/hackage/v/commutative-semigroups.svg)](https://hackage.haskell.org/package/commutative-semigroups)  [![BSD3 License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://github.com/reflex-frp/commutative-semigroups/LICENSE)

A commutative semigroup is a semigroup where the order of arguments to mappend does not matter.

```haskell
class Semigroup g => Commutative g
```
