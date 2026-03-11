{-
Alternative to PointerPrivate implemented at a higher level.
-}
module Data.StorableVector.Lazy.PointerPrivateIndex where

import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as VB

import Foreign.Storable (Storable)


data Pointer a =
   Pointer {chunks :: ![VB.Vector a], index :: !Int}


{-# INLINE cons #-}
cons :: Storable a => [VB.Vector a] -> Pointer a
cons = flip Pointer 0

{-# INLINE viewL #-}
viewL :: Storable a => Pointer a -> Maybe (a, Pointer a)
viewL = switchL Nothing (curry Just)

{-# INLINE switchL #-}
switchL :: Storable a =>
   b -> (a -> Pointer a -> b) -> Pointer a -> b
switchL n j =
   let recourse p =
          let s = chunks p
          in  case s of
                 [] -> n
                 (c:cs) ->
                    let i = index p
                        d = i - V.length c
                    in  if d < 0
                          then j (VB.unsafeIndex c i) (Pointer s (i+1))
                          else recourse (Pointer cs d)
   in  recourse
