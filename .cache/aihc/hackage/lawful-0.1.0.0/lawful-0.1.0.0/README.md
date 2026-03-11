# lawful: Assert that your typeclass instances are lawful

## What is this package for?

This small library provides a single two-parameter typeclass `Lawful c t`,
where `c` is a typeclass and `t` is a type. Declaring an instance of
`Lawful C T` is an assertion that "the instance for `C T` obeys the laws of
the class `C` (whatever *that* means!)"

For example, a lawful instance of `Eq T` should satisfy the reflexive
law `x == x` for all `x :: T`.  This is certainly true for most types, such
as `Int` or `[a]` when `Eq a` is lawful, so we can define

```haskell
Lawful Eq Int
Lawful Eq a => Lawful Eq [a]
```

But it *isn't* true for `Double`:

```
λ> nan = 0 / 0 :: Double
λ> nan == nan
False

```

## Why is there a `c t` constraint on `Lawful c t`?

This constraint lets you use a `Lawful c t` wherever a `c t` would be expected, 
as in:

```haskell
same :: Lawful Eq a => a -> a -> Bool
same x y = x == y
```

## How do I know what laws are expected from a typeclass?

If everybody more-or-less agrees on what the right laws are, hopefully they bothered to
write them down somewhere. If they didn't, then sorry! You're on your own!

## Shouldn't all typeclass instances be lawful anyway, making this package useless?

Wouldn't that be nice?
