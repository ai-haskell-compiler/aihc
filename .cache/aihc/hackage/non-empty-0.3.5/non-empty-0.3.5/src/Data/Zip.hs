module Data.Zip where

import qualified Data.NonEmpty.Class as C

import qualified Data.Traversable as Trav
import Data.Traversable (Traversable, )
import Control.Applicative (Applicative, pure, (<*>), )
import Control.DeepSeq (NFData, rnf, )


{- |
Wrap a container such that its Applicative instance is based on zip.
-}
newtype T f a = Cons {decons :: f a}

instance Functor f => Functor (T f) where
   fmap f (Cons xs) = Cons $ fmap f xs

instance (C.Zip f, C.Repeat f) => Applicative (T f) where
   pure a = Cons $ C.repeat a
   Cons f <*> Cons x = Cons $ C.zipWith ($) f x


instance (C.NFData f, NFData a) => NFData (T f a) where
   rnf = C.rnf

instance (C.NFData f) => C.NFData (T f) where
   rnf = C.rnf . decons


{- |
Always returns a rectangular list
by clipping all dimensions to the shortest slice.
Be aware that @transpose [] == repeat []@.
-}
transposeClip ::
   (Traversable f, C.Zip g, C.Repeat g) =>
   f (g a) -> g (f a)
transposeClip =
   decons . Trav.sequenceA . fmap Cons
