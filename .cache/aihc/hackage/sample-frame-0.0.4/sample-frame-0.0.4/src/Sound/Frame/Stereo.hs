{-
This data type can be used as sample type for stereo signals.
-}
module Sound.Frame.Stereo (
   T, left, right, cons, map,
   swap,
   Channel(..), select,
   interleave, sequence, liftApplicative,
   ) where

import qualified Sound.Frame as Frame

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import Control.Applicative (Applicative, pure, (<*>), liftA2, )
import Control.Monad (liftM2, )

import Foreign.Storable (Storable (..), )
import Foreign.Ptr (Ptr, castPtr, )

import Test.QuickCheck (Arbitrary(arbitrary), )

import Prelude hiding (Either(Left, Right), map, sequence, )


data T a = Cons {left, right :: !a}
   deriving (Eq)


instance Show a => Show (T a) where
   showsPrec p x =
      showParen (p >= 10)
         (showString "Stereo.cons " . showsPrec 11 (left x) .
          showString " " . showsPrec 11 (right x))

instance (Arbitrary a) => Arbitrary (T a) where
   arbitrary = liftM2 cons arbitrary arbitrary


{-# INLINE cons #-}
cons :: a -> a -> T a
cons = Cons

{-# INLINE map #-}
map :: (a -> b) -> T a -> T b
map f (Cons l r) = Cons (f l) (f r)


swap :: T a -> T a
swap x = cons (right x) (left x)



data Channel = Left | Right

{-# INLINE select #-}
select :: T a -> Channel -> a
select x c =
   case c of
      Left -> left x
      Right -> right x

{-# INLINE interleave #-}
interleave :: (T a, T b) -> T (a,b)
interleave = uncurry (liftA2 (,))

{-# INLINE sequence #-}
sequence :: (Functor f) => f (T a) -> T (f a)
sequence x = cons (fmap left x) (fmap right x)

{-# INLINE liftApplicative #-}
liftApplicative ::
   (Applicative f) =>
   (f a -> f b) -> f (T a) -> f (T b)
liftApplicative proc =
   Trav.sequenceA . fmap proc . sequence


instance Functor T where
   {-# INLINE fmap #-}
   fmap = map

-- useful for defining Additive instance
instance Applicative T where
   {-# INLINE pure #-}
   pure a = Cons a a
   {-# INLINE (<*>) #-}
   Cons fl fr <*> Cons l r = Cons (fl l) (fr r)

instance Fold.Foldable T where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

-- this allows for kinds of generic programming
instance Trav.Traversable T where
   {-# INLINE sequenceA #-}
   sequenceA ~(Cons l r) = liftA2 Cons l r



{-# INLINE castToElemPtr #-}
castToElemPtr :: Ptr (T a) -> Ptr a
castToElemPtr = castPtr

instance (Storable a) => Storable (T a) where
   {-# INLINE sizeOf #-}
   {-# INLINE alignment #-}
   {-# INLINE peek #-}
   {-# INLINE poke #-}
   -- cf. storable-record:FixedArray.roundUp
   sizeOf ~(Cons l r) =
      sizeOf l + mod (- sizeOf l) (alignment r) + sizeOf r
   alignment ~(Cons l _) = alignment l
   poke p (Cons l r) =
      let q = castToElemPtr p
      in  poke q l >> pokeElemOff q 1 r
   peek p =
      let q = castToElemPtr p
      in  liftM2 Cons
             (peek q) (peekElemOff q 1)


instance Frame.C a => Frame.C (T a) where
   {-# INLINE numberOfChannels #-}
   numberOfChannels y = 2 * Frame.numberOfChannels (left y)
   {-# INLINE sizeOfElement #-}
   sizeOfElement = Frame.sizeOfElement . left
