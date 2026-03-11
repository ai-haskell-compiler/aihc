-- | This is the main module for end-users of lens-families-core.
-- If you are not building your own optics such as lenses, traversals, grates, etc., but just using optics made by others, this is the only module you need.
module Lens.Family (
-- * Lenses
--
-- | This module provides '^.' for accessing fields and '.~' and '%~' for setting and modifying fields.
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
-- The '&' operator, allows for a convenient way to sequence record updating:
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
-- | '^.' can be used with traversals to access monoidal fields.
-- The result will be a 'Data.Monid.mconcat' of all the fields referenced.
-- The various @fooOf@ functions can be used to access different monoidal summaries of some kinds of values.
--
-- '^?' can be used to access the first value of a traversal.
-- 'Nothing' is returned when the traversal has no references.
--
-- '^..' can be used with a traversals and will return a list of all fields referenced.
--
-- When '.~' is used with a traversal, all referenced fields will be set to the same value, and when '%~' is used with a traversal, all referenced fields will be modified with the same function.
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
-- 'review' can be used with grates to construct a constant grate from a single value.  This is like a 0-ary @zipWith@ function.
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
-- For example, if you have a grid for your structure to another type that has an @Arbitray@ instance, such as grid from a custom word type to 'Bool', e.g. @myWordBitVector :: (Applicative f, Functor g) => AdapterLike' f g MyWord Bool@, you can use the grid to create an @Arbitrary@ instance for your structure by directly applying 'review':
--
-- > instance Arbitrary MyWord where
-- >   arbitrary = review myWordBitVector arbitrary

-- * Building and Finding Optics
--
-- | To build your own optics, see "Lens.Family.Unchecked".
--
-- For stock optics, see "Lens.Family.Stock".
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
    to, view, (^.)
  , folding, views, (^..), (^?)
  , toListOf, allOf, anyOf, firstOf, lastOf, sumOf, productOf
  , lengthOf, nullOf
  , matching
  , over, (%~), set, (.~)
  , review, zipWithOf, degrating
  , under, reset
  , (&)
-- * Pseudo-imperatives
  , (+~), (*~), (-~), (//~), (&&~), (||~), (<>~)
-- * Types
  , AdapterLike, AdapterLike'
  , LensLike, LensLike'
  , FoldLike, FoldLike'
  , GrateLike, GrateLike'
  , AGrate, AGrate'
  , ASetter, ASetter'
  , AResetter, AResetter'
  , PCont
  , First, Last
  , Phantom
-- * Re-exports
  , Constant, Identity, Prod
  , All, Any, Sum, Product
  ) where

import Data.Foldable (traverse_)
import Data.Functor.Constant (Constant(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Functor.Product
import Data.Monoid ( All(..), Any(..)
                   , Sum(..), Product(..)
                   )
import Lens.Family.Phantom
import Lens.Family.Unchecked

type Prod = Data.Functor.Product.Product
newtype PCont i j a = PCont ((a -> j) -> i)

instance Functor (PCont i j) where
  fmap f (PCont h) = PCont $ \k -> h (k . f)

runPCont :: PCont i a a -> i
runPCont (PCont h) = h id

type FoldLike r s t a b = LensLike (Constant r) s t a b
type FoldLike' r s a = LensLike' (Constant r) s a
type AGrate s t a b = GrateLike (PCont b a) s t a b
type AGrate' s a = GrateLike' (PCont a a) s a
type ASetter s t a b = LensLike Identity s t a b
type ASetter' s a = LensLike' Identity s a
type AResetter s t a b = GrateLike Identity s t a b
type AResetter' s a = GrateLike' Identity s a

to :: Phantom f => (s -> a) -> LensLike f s t a b
-- ^ @
-- to :: (s -> a) -> Getter s t a b
-- @
--
-- 'to' promotes a projection function to a read-only lens called a getter.
-- To demote a lens to a projection function, use the section @(^.l)@ or @view l@.
--
-- >>> (3 :+ 4, "example")^._1.to(abs)
-- 5.0 :+ 0.0
to p f = coerce . f . p

view :: FoldLike a s t a b -> s -> a
-- ^ @
-- view :: Getter s t a b -> s -> a
-- @
--
-- Demote a lens or getter to a projection function.
--
-- @
-- view :: Monoid a => Fold s t a b -> s -> a
-- @
--
-- Returns the monoidal summary of a traversal or a fold.
view l = (^.l)

folding :: (Foldable g, Phantom f, Applicative f) => (s -> g a) -> LensLike f s t a b
-- ^ @
-- folding :: (s -> [a]) -> Fold s t a b
-- @
--
-- 'folding' promotes a \"toList\" function to a read-only traversal called a fold.
--
-- To demote a traversal or fold to a \"toList\" function use the section @(^..l)@ or @toListOf l@.
folding p f = coerce . traverse_ f . p

views :: FoldLike r s t a b -> (a -> r) -> s -> r
-- ^ @
-- views :: Monoid r => Fold s t a b -> (a -> r) -> s -> r
-- @
--
-- Given a fold or traversal, return the 'foldMap' of all the values using the given function.
--
-- @
-- views :: Getter s t a b -> (a -> r) -> s -> r
-- @
--
-- 'views' is not particularly useful for getters or lenses, but given a getter or lens, it returns the referenced value passed through the given function.
--
-- @
-- views l f s = f (view l s)
-- @
views l f = getConstant . l (Constant . f)

toListOf :: FoldLike [a] s t a b -> s -> [a]
-- ^ @
-- toListOf :: Fold s t a b -> s -> [a]
-- @
--
-- Returns a list of all of the referenced values in order.
toListOf l = views l (:[])

allOf :: FoldLike All s t a b -> (a -> Bool) -> s -> Bool
-- ^ @
-- allOf :: Fold s t a b -> (a -> Bool) -> s -> Bool
-- @
--
-- Returns true if all of the referenced values satisfy the given predicate.
allOf l p = getAll . views l (All . p)

anyOf :: FoldLike Any s t a b -> (a -> Bool) -> s -> Bool
-- ^ @
-- anyOf :: Fold s t a b -> (a -> Bool) -> s -> Bool
-- @
--
-- Returns true if any of the referenced values satisfy the given predicate.
anyOf l p = getAny . views l (Any . p)

firstOf :: FoldLike (First a) s t a b -> s -> Maybe a
-- ^ @
-- firstOf :: Fold s t a b -> s -> Maybe a
-- @
--
-- Returns 'Just' the first referenced value.
-- Returns 'Nothing' if there are no referenced values.
-- See '^?' for an infix version of 'firstOf'
firstOf l = getFirst . views l (First . Just)

lastOf :: FoldLike (Last a) s t a b -> s -> Maybe a
-- ^ @
-- lastOf :: Fold s t a b -> s -> Maybe a
-- @
--
-- Returns 'Just' the last referenced value.
-- Returns 'Nothing' if there are no referenced values.
lastOf l = getLast . views l (Last . Just)

sumOf :: Num a => FoldLike (Sum a) s t a b -> s -> a
-- ^ @
-- sumOf :: Num a => Fold s t a b -> s -> a
-- @
--
-- Returns the sum of all the referenced values.
sumOf l = getSum . views l Sum

productOf :: Num a => FoldLike (Product a) s t a b -> s -> a
-- ^ @
-- productOf :: Num a => Fold s t a b -> s -> a
-- @
--
-- Returns the product of all the referenced values.
productOf l = getProduct . views l Product

lengthOf :: Num r => FoldLike (Sum r) s t a b -> s -> r
-- ^ @
-- lengthOf :: Num r => Fold s t a b -> s -> r
-- @
--
-- Counts the number of references in a traversal or fold for the input.
lengthOf l = getSum . views l (const (Sum 1))

nullOf :: FoldLike All s t a b -> s -> Bool
-- ^ @
-- nullOf :: Fold s t a b -> s -> Bool
-- @
--
-- Returns true if the number of references in the input is zero.
nullOf l = allOf l (const False)

infixl 8 ^.

(^.) :: s -> FoldLike a s t a b -> a
-- ^ @
-- (^.) :: s -> Getter s t a b -> a
-- @
--
-- Access the value referenced by a getter or lens.
--
-- @
-- (^.) :: Monoid a => s -> Fold s t a b -> a
-- @
--
-- Access the monoidal summary referenced by a traversal or a fold.
s^.l = getConstant $ l Constant s

infixl 8 ^..

(^..) :: s -> FoldLike [a] s t a b -> [a]
-- ^ @
-- (^..) :: s -> Fold s t a b -> [a]
-- @
--
-- Returns a list of all of the referenced values in order.
s^..l = toListOf l s

infixl 8 ^?

(^?) :: s -> FoldLike (First a) s t a b -> Maybe a
-- ^ @
-- (^?) :: s -> Fold s t a b -> Maybe a
-- @
--
-- Returns 'Just' the first referenced value.
-- Returns 'Nothing' if there are no referenced values.
s^?l = firstOf l s

matching :: LensLike (Either a) s t a b -> s -> Either t a
-- ^ @
-- matching :: Traversal s t a b -> s -> Either t a
-- @
--
-- Returns 'Right' of the first referenced value.
-- Returns 'Left' the original value when there are no referenced values.
-- In case there are no referenced values, the result might have a fresh type parameter, thereby proving the original value had no referenced values.
matching l = either Right Left . l Left

review :: GrateLike (Constant ()) s t a b -> b -> t
-- ^ @
-- review :: Grate s t a b -> b -> t
-- review :: Reviewer s t a b -> b -> t
-- @
review l b = l (const b) (Constant ())

zipWithOf :: GrateLike (Prod Identity Identity) s t a b -> (a -> a -> b) -> s -> s -> t
-- ^ @
-- zipWithOf :: Grate s t a b -> (a -> a -> b) -> s -> s -> t
-- @
--
-- Returns a binary instance of a grate.
--
-- @
-- zipWithOf l f x y = degrating l (\k -> f (k x) (k y))
-- @
zipWithOf l f s1 s2 = l (\(Data.Functor.Product.Pair (Identity a1) (Identity a2)) -> f a1 a2)
                        (Data.Functor.Product.Pair (Identity s1) (Identity s2))

degrating :: AGrate s t a b -> ((s -> a) -> b) -> t
-- ^ @
-- degrating :: Grate s t a b -> ((s -> a) -> b) -> t
-- @
--
-- Demote a grate to its normal, higher-order function, form.
--
-- @
-- degrating . grate = id
-- grate . degrating = id
-- @
degrating l = l runPCont . PCont

under :: AResetter s t a b -> (a -> b) -> s -> t
-- ^ @
-- under :: Resetter s t a b -> (a -> b) -> s -> t
-- @
--
-- Demote a resetter to a semantic editor combinator.
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
under l f = l (f . runIdentity) . Identity

reset :: AResetter s t a b -> b -> s -> t
-- ^ @
-- reset :: Resetter s t a b -> b -> s -> t
-- @
-- Set all referenced fields to the given value.
reset l b = under l (const b)

over :: ASetter s t a b -> (a -> b) -> s -> t
-- ^ @
-- over :: Setter s t a b -> (a -> b) -> s -> t
-- @
-- Demote a setter to a semantic editor combinator.
--
-- @
-- over :: Prism s t a b -> Reviwer s t a b
-- over :: Grid s t a b -> Grate s t a b
-- over :: Adapter s t a b -> Grate s t a b
-- @
--
-- Covert an 'AdapterLike' optic into a 'GrateLike' optic.
over l = (l %~)

infixr 4 %~

-- | Modify all referenced fields.
(%~) :: ASetter s t a b -> (a -> b) -> s -> t
l %~ f = runIdentity . l (Identity . f)

infixr 4 .~

-- | Set all referenced fields to the given value.
(.~) :: ASetter s t a b -> b -> s -> t
l .~ b = l %~ const b

-- | Set all referenced fields to the given value.
set :: ASetter s t a b -> b -> s -> t
set = (.~)

infixl 1 &

-- | A flipped version of @($)@.
(&) :: s -> (s -> t) -> t
(&) = flip ($)

infixr 4 +~, -~, *~

(+~), (-~), (*~) :: Num a => ASetter s t a a -> a -> s -> t
l +~ a = l %~ (+ a)
l -~ a = l %~ subtract a
l *~ a = l %~ (* a)

infixr 4 //~

(//~) :: Fractional a => ASetter s t a a -> a -> s -> t
l //~ a = l %~ (/ a)

infixr 4 &&~, ||~

(&&~), (||~) :: ASetter s t Bool Bool -> Bool -> s -> t
l &&~ a = l %~ (&& a)
l ||~ a = l %~ (|| a)

infixr 4 <>~

-- | Monoidally append a value to all referenced fields.
(<>~) :: (Monoid a) => ASetter s t a a -> a -> s -> t
l <>~ a = l %~ (<> a)

-- Local copies of First and Last to hide it from Data.Moniod's pending deprication
newtype First a = First { getFirst :: Maybe a }
newtype Last a = Last { getLast :: Maybe a }

instance Monoid (First a) where
  mempty = First Nothing
  (First Nothing) `mappend` b = b
  a `mappend` _ = a

instance Monoid (Last a) where
  mempty = Last Nothing
  a `mappend` (Last Nothing) = a
  _ `mappend` b = b

instance Semigroup (First a) where
  (<>) = mappend

instance Semigroup (Last a) where
  (<>) = mappend

