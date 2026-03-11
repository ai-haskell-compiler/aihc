# Testable functions

A representation of functions for property testing, featuring
random generation, shrinking, and printing.

This package implements the core functionality.
Separate packages integrate it with existing testing frameworks.

- QuickCheck: [quickcheck-higherorder](https://github.com/Lysxia/quickcheck-higherorder)

- Hedgehog: [hedgehog-higherorder](https://github.com/Lysxia/hedgehog-higherorder)

## Summary

This package defines a type of *testable functions* `a :-> r`,
representing functions `a -> r`.

- To interpret a testable function into a function `a -> r`,
  use `applyFun :: (a :-> r) -> a -> r`.

- To pretty-print a testable function,
  use `show :: Show r => (a :-> r) -> String`.

- To shrink a testable function, given a shrinker for `r`,
  use `shrinkFun :: (r -> [r]) -> (a :-> r) -> [a :-> r]`.

- To randomly generate a testable function `a :-> r`,
  apply a *cogenerator* of `a` to a generator of `r`.
  Cogenerators can be defined using combinators from this library.

### Cogenerators

The type of cogenerators of `a` is `Co Gen a r`,
where `Gen` is QuickCheck's monad of random generators
and `r` is an abstract parameter (it's really `forall r. Co Gen a r`).

That type `Co Gen a r` is literally defined as a type synonym of
`Gen r -> Gen (a :-> r)`.
Given both a cogenerator `c :: Co Gen a r`, and a generator `g :: Gen r`,
we can construct the generator of testable functions `c g :: Gen (a :-> r)`.

(Users can just think of `Co Gen` as a whole,
even though the implementation defines a more general `Co`
which may be applied to any monad.
Similarly, the parameter `r` can be ignored most of the time;
it matters to cogenerators of parameterized types.)

There are several combinators to define cogenerators,
covering the following scenarios.

#### Newtypes and embeddings

If we have a newtype `A` around some old type `B`, and we also have
a cogenerator of `B`:

```haskell
newtype A = MkA { unA :: B }

cogenB :: Co Gen B r
```

Then `cogenEmbed` transforms `cogenB` into a cogenerator of `A`:

```haskell
cogenEmbed "unA" unA cogenB :: Co Gen A r
```

This is actually not restricted to newtypes:
any "embedding" function `A -> B` (here, `unA`) can be used to convert a
`Co Gen B r` to a `Co Gen A r`.
(Yes, there is a contravariant functor hiding there.)
Note that `cogenEmbed` expects a name for that function as a `String`
in its first argument, for pretty-printing.

#### Generic data types

To define a cogenerator of a type which is an instance of `Generic` (from
`GHC.Generics`), use `cogenGeneric`. For example, consider this type:

```haskell
data Small a = Zero | One a | Two a a
  deriving Generic
```

The function `cogenGeneric` takes a heterogeneous list of
cogenerators, one for each constructor of the generic type.
This is `cs` in the example below.

The heterogeneous list is constructed using `(:+)` to append
elements and `()` for the end of the list.

For constructors with multiple fields,
use `(.)` to compose cogenerators for individual fields.

For nullary constructors, use `id` as the "nullary cogenerator".

```haskell
cogenSmall ::
  forall a.
  (forall r. Co Gen a r) ->
  (forall r. Co Gen (Small a) r)
cogenSmall cogenA = cogenGeneric cs where
  cs
    =  id                 -- Nullary cogenerator, for the constructor Zero
    :+ cogenA             -- Cogenerator of a, for the constructor One
    :+ (cogenA . cogenA)  -- A cogenerator of a, once for each field of the constructor Two
    :+ ()                 -- End of the list
```

#### Functions

To generate higher-order testable functions `(a -> b) :-> r`,
we need a cogenerator of functions `a -> b`,
which we can define using `cogenFun`.

To a first approximation, the function `cogenFun` transforms
a cogenerator of `b` into a cogenerator of `(a -> b)`, provided
a way to generate, shrink, and show `a`.

This is actually generalized further by allowing one to provide
a way to generate, shrink, and show a *representation* `a0` of `a`,
which can be equal to `a` in simple cases,
but this generalization makes it possible to generate
functions of arbitrarily high order.

Hence, to construct a cogenerator of `a -> b`,
the function `cogenFun` takes the following arguments, in this order:

1. `Concrete a0`: a dictionary containing a shrinker and a pretty-printer of
   representations `a0`;
2. `Gen (Maybe a0)`: a random generator of `a0`, it must generate `Nothing`
   once in a while (say with probability 1/5 if you have no clue);
3. `a0 -> a`: a function from representations to actual values
   (`id` in simple cases);
4. `forall r. Co Gen b r`: a cogenerator of `b`.

## References

- [*Shrinking and showing functions*](https://dl.acm.org/citation.cfm?id=2364516),
  by Koen Claessen, Haskell Symposium 2012.

  This package extends that work with support for higher-order functions.

  Other implementations based on that paper can be found in:

  + [QuickCheck](https://hackage.haskell.org/package/QuickCheck-2.13.2); in
    particular see the [`Fun`
    type](https://hackage.haskell.org/package/QuickCheck-2.13.2/docs/Test-QuickCheck.html#g:14).

  + [hedgehog-fn](https://hackage.haskell.org/package/hedgehog-fn).

## Internal module policy

Modules under `Test.Fun.Internal` are not subject to any versioning policy.
Breaking changes may apply to them at any time.

If something in those modules seems useful, please report it or create a pull
request to export it from an external module.
