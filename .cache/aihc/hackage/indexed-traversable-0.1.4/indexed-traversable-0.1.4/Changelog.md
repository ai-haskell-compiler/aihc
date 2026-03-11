# 0.1.4 [2024-05-dd]

- Drop support for GHCs prior 8.6.5

# 0.1.3 [2023-10-04]

- Add `Foldable1WithIndex1` type-class, an indexed version of `Foldable1`.

# 0.1.2.1 [2023-03-12]

- `TraversableWithIndex []` doesn't use the `zip [0..]` idiom anymore.
  https://gitlab.haskell.org/ghc/ghc/-/issues/22673

# 0.1.2 [2021-10-30]

- Changed `(<$>)` + `(<*>)` to `liftA2` to potentially avoid extra `fmap`.
- Add `(#..)`, coercive composition after a 2-parameter function.
- Switched references to lens 'Of'- functions to base functions.

Thanks to wygulmage for contributions.

# 0.1.1 [2020-12-27]

- Mark all modules as `Safe`.
  See https://gitlab.haskell.org/ghc/ghc/-/issues/19127

# 0.1 [2020-12-15]

- Split out and combine this package functionality from `lens` and `optics`
