module Data.StorableVector.Lazy.PointerPrivate where

import qualified Data.StorableVector.Pointer as VP
import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as VB

import Foreign.Storable (Storable)


data Pointer a =
   Pointer {
      chunks :: [VB.Vector a],
      ptr    :: {-# UNPACK #-} !(VP.Pointer a)
   }


empty :: Storable a => Pointer a
empty =
   Pointer [] (VP.cons V.empty)

{-# INLINE cons #-}
cons :: Storable a => [VB.Vector a] -> Pointer a
cons [] = empty
cons (c:cs) = Pointer cs (VP.cons c)

{-# INLINE viewL #-}
viewL :: Storable a => Pointer a -> Maybe (a, Pointer a)
viewL = switchL Nothing (curry Just)

{-# INLINE switchL #-}
switchL :: Storable a =>
   b -> (a -> Pointer a -> b) -> Pointer a -> b
switchL n j =
   let recourse p =
          let ct = chunks p
          in  VP.switchL
                 (case ct of
                    [] -> n
                    (c:cs) -> recourse (Pointer cs (VP.cons c)))
                 (\a cp -> j a (Pointer ct cp))
                 (ptr p)
   in  recourse
