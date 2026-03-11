# type-level-show
Utilities for writing `Show`-like type families i.e. instead of `showsPrec
:: Int -> a -> ShowS`, we have `ShowsPrec :: Natural -> k -> Symbol`.

Requires at least GHC 9.6 for the builtin `SChar` etc. singletons.

## Why?
[refined-hackage]: https://hackage.haskell.org/package/refined
[rerefined-hackage]: https://hackage.haskell.org/package/rerefined

[refined][refined-hackage] fills out error messages using `TypeRep`s. My rewrite
[rerefined][rerefined-hackage] allows you to use `Typeable` to fill out a
predicate name, or you can do it yourself. But `TypeRep` is a pain to use, and
it seems strange to do runtime work on type representations. Why can't we just
do it on the type level?

We _could_, but the problem is nice formatting. `Show` instances handle
precedence, and the base library provides a bunch of handy utilities such as
`showParen :: Bool -> ShowS -> ShowS`. Worse, I couldn't find a `ShowNat ::
Natural -> Symbol` type family on Hoogle, which is a very simple type family
that should certainly be lying around.

This library intends to provide such utilities, so that rerefined can avoid
`Typeable` altogether.

Unsure if I'll provide a `ShowType`. Lower priority than the plain utilities.

## License
Provided under the MIT license. See `LICENSE` for license text.
