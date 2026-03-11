{-# OPTIONS_GHC -funbox-strict-fields -O2 #-}
{-  -dverbose-core2core -ddump-simpl-stats -}
module Main (main) where

import qualified Data.StorableVector.Lazy as SV
import qualified Data.StorableVector.Lazy.Pointer as Pointer

import Data.Tuple.HT (mapFst, )
import Control.Monad (liftM, liftM2, )

import Data.Int (Int16)
import Foreign.Storable (Storable)

import Prelude hiding (zipWith, )


{-# INLINE zipWith #-}
zipWith :: (Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> SV.Vector a
   -> SV.Vector b
   -> SV.Vector c
zipWith f =
   SV.crochetL (\y -> liftM (mapFst (flip f y)) . SV.viewL)


{-# INLINE zipWithPointer #-}
zipWithPointer :: (Storable a, Storable b, Storable c) =>
      (a -> b -> c)
   -> SV.Vector a
   -> SV.Vector b
   -> SV.Vector c
zipWithPointer f =
   SV.crochetL (\y -> liftM (mapFst (flip f y)) . Pointer.viewL)
    . Pointer.cons


{-# INLINE zipWithSize #-}
zipWithSize :: (Storable a, Storable b, Storable c) =>
      SV.ChunkSize
   -> (a -> b -> c)
   -> SV.Vector a
   -> SV.Vector b
   -> SV.Vector c
zipWithSize size f =
   curry (SV.unfoldr size (\(xt,yt) ->
      liftM2
         (\(x,xs) (y,ys) -> (f x y, (xs,ys)))
         (SV.viewL xt)
         (SV.viewL yt)))

{-# INLINE zipWithPointerSize #-}
zipWithPointerSize :: (Storable a, Storable b, Storable c) =>
      SV.ChunkSize
   -> (a -> b -> c)
   -> SV.Vector a
   -> SV.Vector b
   -> SV.Vector c
zipWithPointerSize size f a0 b0 =
   SV.unfoldr size (\(xt,yt) ->
      liftM2
         (\(x,xs) (y,ys) -> (f x y, (xs,ys)))
         (Pointer.viewL xt)
         (Pointer.viewL yt))
      (Pointer.cons a0, Pointer.cons b0)


main :: IO ()
main =
   print $
   SV.foldl' (+) 0 $
   SV.take 10000000 $
   (case (1::Int) of
      0 -> zipWith (+)
      1 -> zipWithPointer (+)
      2 -> zipWithSize SV.defaultChunkSize (+)
      3 -> zipWithPointerSize SV.defaultChunkSize (+)
      _ -> error "invalid choice")
         (SV.iterate SV.defaultChunkSize (subtract 1) 0)
         (SV.iterate SV.defaultChunkSize (1+) (1::Int16))
