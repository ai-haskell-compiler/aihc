# `quickcheck-groups`

<a href="http://jonathanknowles.net/quickcheck-groups/"><img src="https://img.shields.io/badge/API-Documentation-green" /></a>

## Overview

The `quickcheck-groups` library provides:
- [QuickCheck](https://hackage.haskell.org/package/QuickCheck) support for testing instances of type classes defined in the [`groups`](https://hackage.haskell.org/package/groups) library.
- Compatibility with the [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) library.
- Reusable properties for type class laws, in the form of [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definitions.

## Usage

In general, usage is identical to that of the [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes) library. If you're already familiar with [`quickcheck-classes`](https://hackage.haskell.org/package/quickcheck-classes), then using this library should be straightforward.

### Testing laws for a single type class

To test that the laws of a particular class hold for a particular type, use the [`lawsCheck`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#v:lawsCheck) function with the [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definition for the class you wish to test.

> #### :stars: Example
>
> To test that the [`Group`](https://hackage.haskell.org/package/groups/docs/Data-Group.html#t:Group) laws hold for the [`Sum`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Sum) [`Integer`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Integer) type:
>
> ```hs
> import Data.Monoid (Sum)
> import Data.Proxy (Proxy (Proxy))
> import Test.QuickCheck.Classes (lawsCheck)
> import Test.QuickCheck.Classes.Group (groupLaws)
>
> lawsCheck (groupLaws (Proxy :: Proxy (Sum Integer)))
> ```
>
> If all tests pass, you should see output similar to:
>
> ```hs
> Group: groupLaw_invert_mempty    +++ OK, passed 1 test.
> Group: groupLaw_invert_invert    +++ OK, passed 100 tests.
> Group: groupLaw_invert_mappend_1 +++ OK, passed 100 tests.
> Group: groupLaw_invert_mappend_2 +++ OK, passed 100 tests.
> Group: groupLaw_subtract_mempty  +++ OK, passed 100 tests.
> Group: groupLaw_subtract_self    +++ OK, passed 100 tests.
> Group: groupLaw_subtract_other   +++ OK, passed 100 tests.
> Group: groupLaw_pow_zero         +++ OK, passed 100 tests.
> Group: groupLaw_pow_nonNegative  +++ OK, passed 100 tests.
> Group: groupLaw_pow_nonPositive  +++ OK, passed 100 tests.
> ```

### Testing laws for multiple type classes

To test that the laws of __multiple__ classes hold for a particular type, use the [`lawsCheckOne`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#v:lawsCheckOne) function with the [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definitions for the classes you wish to test.

> #### :stars: Example
>
> To test that the [`Sum`](https://hackage.haskell.org/package/base/docs/Data-Monoid.html#t:Sum) [`Integer`](https://hackage.haskell.org/package/base/docs/Prelude.html#t:Integer) type satisfies the laws of [`Abelian`](https://hackage.haskell.org/package/groups/docs/Data-Group.html#t:Abelian) and all superclasses:
>
> ```hs
> import Data.Monoid (Sum)
> import Data.Proxy (Proxy (Proxy))
> import Test.QuickCheck.Classes
> import Test.QuickCheck.Classes.Group
>
> lawsCheckOne (Proxy :: Proxy (Sum Integer))
>     [ semigroupLaws
>     , monoidLaws
>     , groupLaws
>     , abelianLaws
>     ]
> ```

## Subclasses and superclasses

Each of the [`Laws`](https://hackage.haskell.org/package/quickcheck-classes/docs/Test-QuickCheck-Classes.html#t:Laws) definitions provided by this library corresponds to exactly __one__ type class, and includes __just__ the laws for that class. Laws for subclasses and superclasses are __not__ automatically included. Therefore, you'll need to __explicitly__ test the laws of every single class you wish to cover.

## Coverage checks

This library includes __coverage checks__ to ensure that important cases are covered, and to reduce the probability of test passes that are false positives. These coverage checks are performed automatically.
