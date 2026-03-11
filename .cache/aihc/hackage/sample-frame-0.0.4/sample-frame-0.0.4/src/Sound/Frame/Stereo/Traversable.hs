{-
This data type can be used as sample type for stereo signals.
-}
module Sound.Frame.Stereo.Traversable (T, left, right, cons, map, ) where

import qualified Sound.Frame as Frame

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold

import Control.Applicative (Applicative, pure, (<*>), liftA2, )
import Control.Monad (liftM2, )

import Foreign.Storable.Traversable as Store
import Foreign.Storable (Storable (..), )

import Test.QuickCheck (Arbitrary(arbitrary), )

import Prelude hiding (map, )


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
map f ~(Cons l r) = Cons (f l) (f r)

instance Functor T where
   {-# INLINE fmap #-}
   fmap = map

-- useful for defining Additive instance
instance Applicative T where
   {-# INLINE pure #-}
   pure a = Cons a a
   {-# INLINE (<*>) #-}
   ~(Cons fl fr) <*> ~(Cons l r) = Cons (fl l) (fr r)

instance Fold.Foldable T where
   {-# INLINE foldMap #-}
   foldMap = Trav.foldMapDefault

-- this allows for kinds of generic programming
instance Trav.Traversable T where
   {-# INLINE sequenceA #-}
   sequenceA ~(Cons l r) = liftA2 Cons l r


instance (Storable a) => Storable (T a) where
   {-# INLINE sizeOf #-}
   sizeOf = Store.sizeOf
   {-# INLINE alignment #-}
   alignment = Store.alignment
   {-# INLINE peek #-}
   peek = Store.peek (error "instance Traversable Stereo is lazy, so we do not provide a real value here")
   {-# INLINE poke #-}
   poke = Store.poke


instance Frame.C a => Frame.C (T a) where
   {-# INLINE numberOfChannels #-}
   numberOfChannels = Frame.numberOfChannelsFoldable
   {-# INLINE sizeOfElement #-}
   sizeOfElement = Frame.sizeOfElementType
