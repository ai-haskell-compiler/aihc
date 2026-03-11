# Self-normalizing applicative expressions [![Hackage](https://img.shields.io/hackage/v/ap-normalize.svg)](https://hackage.haskell.org/package/ap-normalize) [![pipeline status](https://gitlab.com/lysxia/ap-normalize/badges/main/pipeline.svg)](https://gitlab.com/lysxia/ap-normalize/-/commits/main)

Normalize applicative expressions
by simplifying intermediate `pure` and `(<$>)` and reassociating `(<*>)`.

This works by transforming the underlying applicative functor into one whose
operations (`pure`, `(<$>)`, `(<*>)`) reassociate themselves by inlining
and beta-reduction.

It relies entirely on GHC's simplifier. No rewrite rules, no Template
Haskell, no plugins.
Only Haskell code with two common extensions: `GADTs` and `RankNTypes`.

## Example

In the following traversal, one of the actions is `pure b`, which
can be simplified in principle, but only assuming the applicative functor
laws. As far as GHC is concerned, `pure`, `(<$>)`, and `(<*>)` are
completely opaque because `f` is abstract, so it cannot simplify this
expression.

```haskell
data Example a = Example a Bool [a] (Example a)

traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseE go (Example a b c d) =
  Example
    <$> go a
    <*> pure b
    <*> traverse go c
    <*> traverseE go d
  -- Total: 1 <$>, 3 <*>
```

Using this library, we can compose actions in a specialized applicative
functor `Aps f`, keeping the code in roughly the same structure.

```haskell
traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseE go (Example a b c d) =
  Example
    <$>^ go a
    <*>  pure b
    <*>^ traverse go c
    <*>^ traverseE go d
    & lowerAps
  -- Total: 1 <$>, 3 <*>
```

GHC simplifies that traversal to the following, using only two
combinators in total.

```haskell
traverseE :: Applicative f => (a -> f b) -> Example a -> f (Example b)
traverseE go (Example a b c d) =
  liftA2 (\a' -> Example a' b)
    (go a)
    (traverse go c)
    <*> traverseE go d
  -- Total: 1 liftA2, 1 <*>
```

For more details see the `ApNormalize` module.

## Related links

The blog post [*Generic traversals with applicative difference
lists*](https://blog.poisson.chat/posts/2020-08-05-applicative-difference-lists.html)
gives an overview of the motivation and core data structure of this library.

The same idea can be applied to monoids and monads.
They are all applications of Cayley's representation theorem.

- [`Endo`][endo] to normalize `(<>)` and `mempty`, in *base*
- [`Codensity`][codensity] to normalize `pure` and `(>>=)`, in *kan-extensions*

[endo]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Monoid.html#t:Endo
[codensity]: https://hackage.haskell.org/package/kan-extensions-5.2/docs/Control-Monad-Codensity.html
