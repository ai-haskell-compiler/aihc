{-# LANGUAGE Rank2Types #-}
-- | This is the main module for end-users of lens-families.
-- If you are not building your own optics such as lenses, traversals, grates, etc., but just using optics made by others, this is the only module you need.
module Lens.Family2 (
-- * Lenses
--
-- | This module provides 'LF.^.' for accessing fields and 'LF..~' and 'LF.%~' for setting and modifying fields.
-- Lenses are composed with `Prelude..` from the @Prelude@ and `Prelude.id` is the identity lens.
--
-- Lens composition in this library enjoys the following identities.
--
-- * @x^.l1.l2 === x^.l1^.l2@
--
-- * @l1.l2 %~ f === l1 %~ l2 %~ f@
--
-- The identity lens behaves as follows.
--
-- * @x^.id === x@
--
-- * @id %~ f === f@
--
-- The 'LF.&' operator, allows for a convenient way to sequence record updating:
--
-- @record & l1 .~ value1 & l2 .~ value2@
--
-- Lenses are implemented in van Laarhoven style.
-- Lenses have type @'Functor' f => (a -> f a) -> s -> f s@ and lens families have type @'Functor' f => (a i -> f (a j)) -> s i -> f (s j)@.
--
-- Keep in mind that lenses and lens families can be used directly for functorial updates.
-- For example, @_2 id@ gives you strength.
--
-- > _2 id :: Functor f => (a, f b) -> f (a, b)
--
-- Here is an example of code that uses the 'Maybe' functor to preserves sharing during update when possible.
--
-- > -- | 'sharedUpdate' returns the *identical* object if the update doesn't change anything.
-- > -- This is useful for preserving sharing.
-- > sharedUpdate :: Eq a => LensLike' Maybe s a -> (a -> a) -> s -> s
-- > sharedUpdate l f s = fromMaybe s (l f' s)
-- >  where
-- >   f' a | b == a    = Nothing
-- >        | otherwise = Just b
-- >    where
-- >     b = f a

-- * Traversals
--
-- | 'LF.^.' can be used with traversals to access monoidal fields.
-- The result will be a 'Data.Monid.mconcat' of all the fields referenced.
-- The various @fooOf@ functions can be used to access different monoidal summaries of some kinds of values.
--
-- '^?' can be used to access the first value of a traversal.
-- 'Nothing' is returned when the traversal has no references.
--
-- '^..' can be used with a traversals and will return a list of all fields referenced.
--
-- When 'LF..~' is used with a traversal, all referenced fields will be set to the same value, and when 'LF.%~' is used with a traversal, all referenced fields will be modified with the same function.
--
-- A variant of '^?' call 'matching' returns 'Either' a 'Right' value which is the first value of the traversal, or a 'Left' value which is a "proof" that the traversal has no elements.
-- The "proof" consists of the original input structure, but in the case of polymorphic families, the type parameter is replaced with a fresh type variable, thus proving that the type parameter was unused.
--
-- Like all optics, traversals can be composed with '.', and because every lens is automatically a traversal, lenses and traversals can be composed with '.' yielding a traversal.
--
-- Traversals are implemented in van Laarhoven style.
-- Traversals have type @'Applicative' f => (a -> f a) -> s -> f s@ and traversal families have type @'Applicative' f => (a i -> f (a j)) -> s i -> f (s j)@.
--

-- * Grates
--
-- | 'zipWithOf' can be used with grates to zip two structure together provided a binary operation.
--
-- 'under' can be used to modify each value in a structure according to a function.  This works analogous to how 'over' works for lenses and traversals.
--
-- 'LF.review' can be used with grates to construct a constant grate from a single value.  This is like a 0-ary @zipWith@ function.
--
-- 'degrating' can be used to build higher arity @zipWithOf@ functions:
--
-- > zipWith3Of :: AGrate s t a b -> (a -> a -> a -> b) -> s -> s -> s -> t
-- > zipWith3Of l f s1 s2 s3 = degrating l (\k -> f (k s1) (k s2) (k s3))
--
-- Like all optics, grates can be composed with '.', and 'id' is the identity grate.
--
-- Grates are implemented in van Laarhoven style.
--
-- Grates have type @'Functor' g => (g a -> a) -> g s -> s@ and grate families have type @'Functor' g => (g (a i) -> a j) -> g (s i) -> s j@.
--
-- Keep in mind that grates and grate families can be used directly for functorial zipping.  For example,
--
-- > both sum :: Num a => [(a, a)] -> (a, a)
--
-- will take a list of pairs return the sum of the first components and the sum of the second components.  For another example,
--
-- > cod id :: Functor f => f (r -> a) -> r -> f a
--
-- will turn a functor full of functions into a function returning a functor full of results.

-- * Adapters, Grids, and Prisms
--
-- | The Adapter, Prism, and Grid optics are all 'AdapterLike' optics and typically not used directly, but either converted to a 'LensLike' optic using 'under', or into a 'GrateLike' optic using 'over'.
-- See 'under' and 'over' for details about which conversions are possible.
--
-- These optics are implemented in van Laarhoven style.
--
-- * Adapters have type @('Functor' f, 'Functor' g) => (g a -> f a) -> g s -> f s@ and Adapters families have type @('Functor' f, 'Functor' g) => (g (a i) -> f (a j)) -> g (s i) -> f (s j)@.
--
-- * Grids have type @('Applicative' f, 'Functor' g) => (g a -> f a) -> g s -> f s@ and Grids families have type @('Applicative' f, 'Functor' g) => (g (a i) -> f (a j)) -> g (s i) -> f (s j)@.
--
-- * Prisms have type @('Applicative' f, 'Traversable' g) => (g a -> f a) -> g s -> f s@ and Prisms families have type @('Applicative' f, 'Traversable' g) => (g (a i) -> f (a j)) -> g (s i) -> f (s j)@.
--
-- Keep in mind that these optics and their families can sometimes be used directly, without using 'over' and 'under'.  Sometimes you can take advantage of the fact that
--
-- @
--    LensLike f (g s) t (g a) b
--   ==
--    AdapterLike f g s t a b
--   ==
--    GrateLike g s (f t) a (f b)
-- @
--
-- For example, if you have a grid for your structure to another type that has an @Arbitray@ instance, such as grid from a custom word type to 'Bool', e.g. @myWordBitVector :: (Applicative f, Functor g) => AdapterLike' f g MyWord Bool@, you can use the grid to create an @Arbitrary@ instance for your structure by directly applying 'LF.review':
--
-- > instance Arbitrary MyWord where
-- >   arbitrary = review myWordBitVector arbitrary

-- * Building and Finding Optics
--
-- | To build your own optics, see "Lens.Family2.Unchecked".
--
-- For stock optics, see "Lens.Family2.Stock".
--
-- References:
--
-- * <http://www.twanvl.nl/blog/haskell/cps-functional-references>
--
-- * <http://r6.ca/blog/20120623T104901Z.html>
--
-- * <http://comonad.com/reader/2012/mirrored-lenses/>
--
-- * <http://conal.net/blog/posts/semantic-editor-combinators>
--
-- * <https://r6research.livejournal.com/28050.html>

-- * Documentation
    to, LF.view, (LF.^.)
  , folding, LF.views, (^..), (^?)
  , toListOf, allOf, anyOf, firstOf, lastOf, sumOf, productOf
  , lengthOf, nullOf
  , matching
  , over, (%~), set, (.~)
  , LF.review, zipWithOf, degrating
  , under, reset
  , (LF.&)
-- * Pseudo-imperatives
  , (+~), (*~), (-~), (//~), (&&~), (||~), (<>~)
-- * Types
  , Adapter, Adapter'
  , Prism, Prism'
  , Lens, Lens'
  , Traversal, Traversal'
  , Setter, Setter'
  , Getter, Getter'
  , Fold, Fold'
  , Grate, Grate'
  , Grid, Grid'
  , Reviewer, Reviewer'
  , LF.AdapterLike, LF.AdapterLike'
  , LF.LensLike, LF.LensLike'
  , LF.GrateLike, LF.GrateLike'
  , LF.FoldLike, LF.FoldLike'
  , LF.Constant
  , LF.Phantom
  , Identical
  ) where

import qualified Lens.Family as LF
import Lens.Family2.Unchecked

type Grid s t a b = forall f g. (Applicative f, Functor g) => LF.AdapterLike f g s t a b
type Grid' s a = forall f g. (Applicative f, Functor g) => LF.AdapterLike' f g s a

type Fold s t a b = forall f. (LF.Phantom f, Applicative f) => LF.LensLike f s t a b
type Fold' s a = forall f. (LF.Phantom f, Applicative f) => LF.LensLike' f s a

type Getter s t a b = forall f. LF.Phantom f => LF.LensLike f s t a b
type Getter' s a = forall f. LF.Phantom f=> LF.LensLike' f s a

type Reviewer s t a b = forall f. LF.Phantom f => LF.GrateLike f s t a b
type Reviewer' s a = forall f. LF.Phantom f => LF.GrateLike' f s a

-- |'to' promotes a projection function to a read-only lens called a getter.
-- To demote a lens to a projection function, use the section @(^.l)@ or @view l@.
--
-- >>> (3 :+ 4, "example")^._1.to(abs)
-- 5.0 :+ 0.0
to :: (s -> a) -> Getter s t a b
to sa = LF.to sa

-- | 'folding' promotes a \"toList\" function to a read-only traversal called a fold.
--
-- To demote a traversal or fold to a \"toList\" function use the section @(^..l)@ or @toListOf l@.
folding :: Foldable f => (s -> f a) -> Fold s t a b
folding sa = LF.folding sa

-- | Returns a list of all of the referenced values in order.
toListOf :: Fold s t a b -> s -> [a]
toListOf l = LF.toListOf l

-- | Returns true if all of the referenced values satisfy the given predicate.
allOf :: Fold s t a b -> (a -> Bool) -> s -> Bool
allOf l = LF.allOf l

-- | Returns true if any of the referenced values satisfy the given predicate.
anyOf :: Fold s t a b -> (a -> Bool) -> s -> Bool
anyOf l = LF.anyOf l

-- | Returns 'Just' the first referenced value.
-- Returns 'Nothing' if there are no referenced values.
-- See '^?' for an infix version of 'firstOf'
firstOf :: Fold s t a b -> s -> Maybe a
firstOf l = LF.firstOf l

-- | Returns 'Just' the last referenced value.
-- Returns 'Nothing' if there are no referenced values.
lastOf :: Fold s t a b -> s -> Maybe a
lastOf l = LF.lastOf l

-- | Returns the sum of all the referenced values.
sumOf :: Num a => Fold s t a b -> s -> a
sumOf l = LF.sumOf l

-- | Returns the product of all the referenced values.
productOf :: Num a => Fold s t a b -> s -> a
productOf l = LF.productOf l

-- | Counts the number of references in a traversal or fold for the input.
lengthOf :: Num r => Fold s t a b -> s -> r
lengthOf l = LF.lengthOf l

-- | Returns true if the number of references in the input is zero.
nullOf :: Fold s t a b -> s -> Bool
nullOf l = LF.nullOf l

infixl 8 ^..

-- | Returns a list of all of the referenced values in order.
(^..) :: s -> Fold s t a b -> [a]
x^..l = x LF.^.. l

infixl 8 ^?

-- | Returns 'Just' the first referenced value.
-- Returns 'Nothing' if there are no referenced values.
(^?) :: s -> Fold s t a b -> Maybe a
x^?l = x LF.^? l

-- | Returns 'Right' of the first referenced value.
-- Returns 'Left' the original value when there are no referenced values.
-- In case there are no referenced values, the result might have a fresh type parameter, thereby proving the original value had no referenced values.
matching :: Traversal s t a b -> s -> Either t a
matching l = LF.matching l

zipWithOf :: Grate s t a b -> (a -> a -> b) -> s -> s -> t
-- ^ Returns a binary instance of a grate.
--
-- @
-- zipWithOf l f x y = degrating l (\k -> f (k x) (k y))
-- @
zipWithOf l = LF.zipWithOf l

degrating :: Grate s t a b -> ((s -> a) -> b) -> t
-- ^ Demote a grate to its normal, higher-order function, form.
--
-- @
-- degrating . grate = id
-- grate . degrating = id
-- @
degrating l = LF.degrating l

-- | Demote a resetter to a semantic editor combinator.
--
-- @
-- under :: Prism s t a b -> Traversal s t a b
-- under :: Grid s t a b -> Traversal s t a b
-- under :: Adapter s t a b -> Lens s t a b
-- @
--
-- Covert an 'AdapterLike' optic into a 'LensLike' optic.
--
-- Note: this function is unrelated to the lens package's @under@ function.
under :: Resetter s t a b -> (a -> b) -> s -> t
under l = LF.under l

-- | Set all referenced fields to the given value.
reset :: Resetter s t a b -> b -> s -> t
reset l = LF.reset l

-- | Demote a setter to a semantic editor combinator.
--
-- @
-- over :: Prism s t a b -> Reviwer s t a b
-- over :: Grid s t a b -> Grate s t a b
-- over :: Adapter s t a b -> Grate s t a b
-- @
--
-- Covert an 'AdapterLike' optic into a 'GrateLike' optic.
over :: Setter s t a b -> (a -> b) -> s -> t
over l = LF.over l

infixr 4 %~

-- | Modify all referenced fields.
(%~) :: Setter s t a b -> (a -> b) -> s -> t
l %~ f = l LF.%~ f

infixr 4 .~

-- | Set all referenced fields to the given value.
(.~) :: Setter s t a b -> b -> s -> t
l .~ b = l LF..~ b

-- | Set all referenced fields to the given value.
set :: Setter s t a b -> b -> s -> t
set l = LF.set l

infixr 4 +~, -~, *~

(+~), (-~), (*~) :: Num a => Setter s t a a -> a -> s -> t
l +~ a = l LF.+~ a
l -~ a = l LF.-~ a
l *~ a = l LF.*~ a

infixr 4 //~

(//~) :: Fractional a => Setter s t a a -> a -> s -> t
l //~ a = l LF.//~ a

infixr 4 &&~, ||~

(&&~), (||~) :: Setter s t Bool Bool -> Bool -> s -> t
l &&~ a = l LF.&&~ a
l ||~ a = l LF.||~ a

infixr 4 <>~

-- | Monoidally append a value to all referenced fields.
(<>~) :: (Monoid a) => Setter s t a a -> a -> s -> t
l <>~ a = l LF.<>~ a
