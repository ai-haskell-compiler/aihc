# `quickcheck-monoid-subclasses`

<a href="http://jonathanknowles.net/quickcheck-monoid-subclasses/"><img src="https://img.shields.io/badge/API-Documentation-green" /></a>

## Overview

The `quickcheck-monoid-subclasses` library provides:
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck) support for testing instances of type classes defined in the [`monoid-subclasses`](https://hackage.haskell.org/package/monoid-subclasses) library.
- Compatibility with the [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) library.
- Reusable properties for type class laws, in the form of [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definitions.

## Usage

In general, usage is identical to that of the [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) library. If you're already familiar with [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes), then using this library should be straightforward.

### Testing laws for a single type class

To test that the laws of a particular class hold for a particular type, use the [`lawsCheck`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:lawsCheck) function with the [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definition for the class you wish to test.

> #### :stars: Example
>
> To test that the [`Monus`](https://hackage.haskell.org/package/monoid-subclasses/docs/Data-Monoid-Monus.html#t:Monus) laws hold for the [`Sum`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Sum) [`Natural`](https://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural) type:
>
> ```hs
> import Data.Monoid (Sum)
> import Data.Proxy (Proxy (Proxy))
> import Numeric.Natural (Natural)
> import Test.QuickCheck.Classes (lawsCheck)
> import Test.QuickCheck.Classes.Monoid.Monus (monusLaws)
>
> lawsCheck (monusLaws (Proxy :: Proxy (Sum Natural)))
> ```
>
> If all tests pass, you should see output similar to:
>
> ```hs
> Monus: axiom1 +++ OK, passed 100 tests.
> Monus: axiom2 +++ OK, passed 100 tests.
> Monus: axiom3 +++ OK, passed 100 tests.
> Monus: axiom4 +++ OK, passed 100 tests.
> Monus: stripPrefixOverlap +++ OK, passed 100 tests.
> Monus: stripSuffixOverlap +++ OK, passed 100 tests.
> ```

### Testing laws for multiple type classes

To test that the laws of __multiple__ classes hold for a particular type, use the [`lawsCheckOne`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:lawsCheckOne) function with the [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definitions for the classes you wish to test.

> #### :stars: Example
>
> To test that the [`Sum`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Sum) [`Natural`](https://hackage.haskell.org/package/base/docs/Numeric-Natural.html#t:Natural) type satisfies the laws of [`Semigroup`](https://hackage.haskell.org/package/base/docs/Data-Semigroup.html#t:Semigroup) and its subclasses:
>
> ```hs
> import Data.Monoid (Sum)
> import Data.Proxy (Proxy (Proxy))
> import Numeric.Natural (Natural)
> import Test.QuickCheck.Classes
> import Test.QuickCheck.Classes.Monoid.GCD
> import Test.QuickCheck.Classes.Monoid.LCM
> import Test.QuickCheck.Classes.Monoid.Monus
> import Test.QuickCheck.Classes.Monoid.Null
> import Test.QuickCheck.Classes.Semigroup.Cancellative
> import Test.QuickCheck.Classes.Semigroup.Factorial
>
> lawsCheckOne (Proxy :: Proxy (Sum Natural))
>     [ cancellativeLaws
>     , commutativeLaws
>     , distributiveGCDMonoidLaws
>     , distributiveLCMMonoidLaws
>     , factorialLaws
>     , factorialMonoidLaws
>     , gcdMonoidLaws
>     , lcmMonoidLaws
>     , leftCancellativeLaws
>     , leftDistributiveGCDMonoidLaws
>     , leftGCDMonoidLaws
>     , leftReductiveLaws
>     , monoidLaws
>     , monoidNullLaws
>     , monusLaws
>     , overlappingGCDMonoidLaws
>     , positiveMonoidLaws
>     , reductiveLaws
>     , rightCancellativeLaws
>     , rightDistributiveGCDMonoidLaws
>     , rightGCDMonoidLaws
>     , rightReductiveLaws
>     , semigroupLaws
>     , stableFactorialLaws
>     ]
> ```

## Subclasses and superclasses

Each of the [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definitions provided by this library corresponds to exactly __one__ type class, and includes __just__ the laws for that class. Laws for subclasses and superclasses are __not__ automatically included. Therefore, you'll need to __explicitly__ test the laws of every single class you wish to cover.

## Coverage checks

This library includes __coverage checks__ to ensure that important cases are covered, and to reduce the probability of test passes that are false positives. These coverage checks are performed automatically.

To increase coverage of interesting and important cases, this library also checks that laws hold for __combinations__ of generated arbitrary values.

> #### :stars: Example
>
> Suppose we are testing the following law:
>
> ```hs
> isPrefixOf a b == isJust (stripPrefix a b)
> ```
>
> This library will also test that the following __derived__ laws hold:
>
> ```hs
> isPrefixOf a (a <> a) == isJust (stripPrefix a (a <> a))
> isPrefixOf a (a <> b) == isJust (stripPrefix a (a <> b))
> isPrefixOf a (b <> a) == isJust (stripPrefix a (b <> a))
> ```
