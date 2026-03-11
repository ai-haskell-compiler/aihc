# roles

Composable, class-based roles.

# Table of contents

  * [Motivation: What is the cost of a `newtype`?](#what-is-the-cost)
  * [Background](#background)
    * [The magical `Coercible` class](#magical)
    * [Lifting coercions](#lifting)
    * [Roles to the rescue](#roles)
  * [The `roles` library](#library)
    * [What problem does this library solve?](#what-problem)
    * [How can I use this library?](#how-can-i)
    * [History](#history)

# What is the cost of a `newtype`? <a name="what-is-the-cost"></a>

The conventional wisdom is that Haskell's `newtype` gives you a zero-cost
abstraction--wrapping and unwrapping of `newtype`s are purely a compile-time
operation. Unfortunately, this is not *quite* the case:

```haskell
-- A zero-cost abstraction... or is it?
newtype User = User String

-- 'name x' and 'x' will refer to the same in-memory entity at run-time...
name :: User -> String
name (User x) = x

-- ...but 'maybeName x' and 'x' will not be the same at run-time:
-- a new value of type 'Maybe String' is allocated, despite being
-- identical to the input!
maybeName :: Maybe User -> Maybe String
maybeName = fmap name
```

See the POPL '11 paper [Generative Type Abstraction and Type-level Computation](http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf) for a more
through investigation of the problem and a solution, and the ICFP '14 paper [Safe Zero-cost Coercions for Haskell](http://cs.brynmawr.edu/~rae/papers/2014/coercible/coercible-ext.pdf) for
implementation of the `Coercible` typeclass in Haskell. Still more information can be found [on the Haskell wiki](https://ghc.haskell.org/trac/ghc/wiki/Roles).

# Background <a name="background"></a>

## The magical `Coercible` class <a name="magical"></a>

The solution described in the second paper was to introduce a typeclass `Coercible a b` of the
form

```haskell
class Coercible a b where
    coerce :: a -> b
```

There is something a bit magical about a typeclass like this, that requires baked-in
compiler support:
Consider a declaration `newtype New = MkNew Old`. Within the module where `New` is
defined, we should be able to freely `coerce` between `New` and `Old`. But then again,
if  `MkNew` is not exported, then *outside* of the module we should *not* be able to
`coerce` between `New` and `Old`. As a result, the `Coercible` class must involve special
compiler magic to ensure that `coerce` is only available in the appropriate modules.

## Lifting coercions <a name="lifting"></a>

Let's revisit the `maybeName` issue. Ideally, we would like to rewrite the example
to make the coercions explicit, to guarantee zero runtime cost:

```haskell
newtype User = User String

name :: User -> String
name = coerce

-- GHC knows that it can coerce 'User' to 'String', but
-- how about 'Maybe User' to 'Maybe String'?
maybeName :: Maybe User -> Maybe String
maybeName = coerce
```

For this to work, we would need instances of `Coercible User String` (provided by
GHC compiler magic, since we're in the module where `User` is defined) and
also `Coercible a b => Coercible (Maybe a) (Maybe b)`.

You might expect GHC could implement a generic "coercion lifting" rule of the form
`Coercible a b => Coercible (f a) (f b)`. Unfortunately this would be unsound
in the presence of type families:

```haskell
newtype User = MkUser String

type family Fam
type instance Fam String = Int
type instance Fam User   = Double
```

If GHC naively added the coercion lifting rule, then we would be able to
coerce from `Double` to `Int` by:

```haskell
Coercible User String => Coercible (Fam User) (Fam String) -- a.k.a. Coercible Double Int!
```

This is obviously no good.

## Roles to the rescue <a name="roles"></a>

It seems that sometimes we can lift a `Coercion a b` to a `Coercion (f a) (f b)`
(e.g. for `Maybe`) and sometimes we cannot (e.g. for `Fam`). To figure out when
a coercion `a -> b` can be lifted to a coercion `f a -> f b`, GHC infers a *role*
for the type parameter of `f`. If `f` can safely support coercion-lifting, then
we say `f`'s type parameter has a *representational* role; otherwise, it has a
*nominal* role.

Happily, GHC will infer that `Maybe`'s type parameter is representational, while
`Fam`'s type parameter is nominal. This lets our definition `maybeName = coerce`
pass the compiler, while attempting to coerce an `Int` to a `Double` via
`Fam` will fail.

# The `roles` library <a name="library"></a>

## What problem does this library solve? <a name="what-problem"></a>

Unfortunately, in GHC Haskell there is currently (circa late 2017) no way to
write something like this:

```haskell
coerceFirst :: (Coercible a b, Functor f) => [f a] -> Maybe (f b)
coerceFirst []    = Nothing
coerceFirst (x:_) = Just (coerce x)
{- GHC says:
    • Couldn't match representation of type ‘f a’ with that of ‘f b’
        arising from a use of ‘coerce’
      NB: We cannot know what roles the parameters to ‘f’ have;
        we must assume that the role is nominal
-}
```

GHC rightly refuses to lift the coercion from `a` to `b` into a coercion from
`f a` to `f b`: it does not have any assurance that the functor
`f` uses its type parameter representationally.

In other words, this function needs to have a constraint `Representational f` that
means something like "`f`'s type parameter has a representational
role."

This library simply provides the `Representational` typeclass for a variety of
types in `base` and `containers`.

## How can I use this library? <a name="how-can-i"></a>

Since it is not made up of GHC pixie-dust magic, `Representational` needs a way to
convince GHC that the lifted coercion is allowed.
It does this via the lone function of the `Representational` class:

```haskell
class Representational f where
  rep :: Coercion a b -> Coercion (f a) (f b)
```

A value of type `Coercion a b` is like a certificate that tells GHC "you are allowed
to coerce `a` to `b`". You cash the certificate in by using `coerceWith`, yielding
an actual coercion from `a` to `b`. The `rep` function simply converts a certificate
for coercing `a` to `b` into a certificate for coercing `f a` into `f b`.

We can now fix the example from the previous section:

```haskell
coerceFirst :: (Coercible a b, Representational f, Functor f) => [f a] -> Maybe (f b)
coerceFirst []    = Nothing
coerceFirst (x:_) = Just (coerceWith (rep Coercion) x)
--                        ~~~~~~~~~~~~~~~~~~~~~~~~~
--                                   |
--   This means: (1) Get a certificate verifying that we can coerce `a` to `b`.
--                   This certificate is `Coercion`, and we got it by making use
--                   of the constraint `Coercible a b`.
--               (2) Since `f` is `Representational`, we can use `rep` to upgrade
--                   the certificate to a certificate for coercion from `f a` to `f b`.
--               (3) Use `coerceWith` to hand the certificate over to GHC, obtaining
--                   an actual coercion from `f a` to `f b` in return.

{- GHC says: sounds good to me! -}
```

For another usage example, see the `withRecMap` function from `justified-containers`,
and the corresponding test case. A `Representational` constraint is used to ensure
that large maps are not duplicated in memory, despite undergoing a complex series
of `newtype`-related manipulations. An earlier version of `withRecMap` worked by
`fmap`ping newtype wrappers and unwrappers, which caused an accidental duplication
of the map.

## History <a name="history"></a>

This package is a fork of Edward Kmett's original `roles 0.1`. It offers
fewer instances of `Representational`, in exchange for a much smaller set
of dependencies. The instances that involve `new` and `eta` have been
removed, and instances for `containers` have been added.
