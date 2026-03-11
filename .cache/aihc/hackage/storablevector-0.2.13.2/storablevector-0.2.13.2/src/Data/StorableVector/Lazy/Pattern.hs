{- |
Functions for 'StorableVector' that allow control of the size of individual chunks.

This is import for an application like the following:
You want to mix audio signals that are relatively shifted.
The structure of chunks of three streams may be illustrated as:

> [____] [____] [____] [____] ...
>   [____] [____] [____] [____] ...
>     [____] [____] [____] [____] ...

When we mix the streams (@zipWith3 (\x y z -> x+y+z)@)
with respect to the chunk structure of the first signal,
computing the first chunk requires full evaluation of all leading chunks of the stream.
However the last value of the third leading chunk
is much later in time than the last value of the first leading chunk.
We like to reduce these dependencies using a different chunk structure,
say

> [____] [____] [____] [____] ...
>   [__] [____] [____] [____] ...
>     [] [____] [____] [____] ...

-}
module Data.StorableVector.Lazy.Pattern (
   Vector,
   ChunkSize,
   chunkSize,
   defaultChunkSize,
   LazySize,

   empty,
   singleton,
   pack,
   unpack,
   packWith,
   unpackWith,
   unfoldrN,
   iterateN,
   cycle,
   replicate,
   null,
   length,
   cons,
   append,
   concat,
   map,
   reverse,
   foldl,
   foldl',
   any,
   all,
   maximum,
   minimum,
   viewL,
   viewR,
   switchL,
   switchR,
   scanl,
   mapAccumL,
   mapAccumR,
   crochetL,
   take,
   drop,
   splitAt,
   takeVectorPattern,
   splitAtVectorPattern,
   dropMarginRem,
   dropMargin,
   dropWhile,
   takeWhile,
   span,
   filter,
   zipWith,
   zipWith3,
   zipWith4,
   zipWithSize,
   zipWithSize3,
   zipWithSize4,
{-
   pad,
   cancelNullVector,
-}
   ) where

import Numeric.NonNegative.Class ((-|))
import qualified Numeric.NonNegative.Chunky as LS
import qualified Data.StorableVector.Lazy as LSV
import qualified Data.StorableVector as V

import Data.StorableVector.Lazy (Vector(SV), ChunkSize(ChunkSize))

import Data.StorableVector.Lazy (
   chunkSize, defaultChunkSize,
   empty, singleton, unpack, unpackWith, cycle,
   null, cons, append, concat, map, reverse,
   foldl, foldl', any, all, maximum, minimum,
   viewL, viewR, switchL, switchR,
   scanl, mapAccumL, mapAccumR, crochetL,
   dropMarginRem, dropMargin,
   dropWhile, takeWhile, span, filter, 
   zipWith, zipWith3, zipWith4, 
   )

import qualified Data.List as List

import qualified Data.List.HT as ListHT
import Data.Tuple.HT (mapPair, mapFst, forcePair, swap, )

import Control.Monad (liftM2, liftM3, liftM4, guard, )

import Foreign.Storable (Storable)

import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (fst, snd, curry, uncurry)
import Prelude (Int, (.), ($), (<=), flip, return, fmap, not)


type LazySize = LS.T ChunkSize

-- * Introducing and eliminating 'Vector's

{-
Actually, this is lazy enough:

> LSV.unpack $ pack (LS.fromChunks [10,15]) (['a'..'y'] List.++ Prelude.undefined)
"abcdefghijklmnopqrstuvwxy"
-}
pack :: (Storable a) => LazySize -> [a] -> Vector a
pack size =
   fst . unfoldrN size ListHT.viewL


{-# INLINE packWith #-}
packWith :: (Storable b) => LazySize -> (a -> b) -> [a] -> Vector b
packWith size f =
   fst . unfoldrN size (fmap (mapFst f) . ListHT.viewL)


{-
{-# INLINE unfoldrNAlt #-}
unfoldrNAlt :: (Storable b) =>
      LazySize
   -> (a -> Maybe (b,a))
   -> a
   -> (Vector b, Maybe a)
unfoldrNAlt (LS.Cons size) f x =
   let go sz y =
          case sz of
             [] -> ([], y)
             (ChunkSize s : ss) ->
                maybe
                   ([], Nothing)
                   ((\(c,a1) -> mapFst (c:) $ go ss a1) .
                    V.unfoldrN s (fmap (mapSnd f)))
                   (f y)
   in  mapFst SV $ go size (Just x)
-}

{-# INLINE unfoldrN #-}
unfoldrN :: (Storable b) =>
      LazySize
   -> (a -> Maybe (b,a))
   -> a
   -> (Vector b, Maybe a)
unfoldrN size f =
   let go sz y =
          forcePair $
          case sz of
             [] -> ([], y)
             (ChunkSize s : ss) ->
                let m =
                       do a0 <- y
                          let p = V.unfoldrN s f a0
                          guard (not (V.null (fst p)))
                          return p
                in  case m of
                       Nothing -> ([], Nothing)
                       Just (c,a1) -> mapFst (c:) $ go ss a1
   in  mapFst SV . go (LS.toChunks size) . Just


{-# INLINE iterateN #-}
iterateN :: Storable a => LazySize -> (a -> a) -> a -> Vector a
iterateN size f =
   fst . unfoldrN size (\x -> Just (x, f x))

{-
Tries to be time and memory efficient
by reusing subvectors of a chunk
until a larger chunk is needed.
However, it can be a memory leak
if a huge chunk is followed by many little ones.
-}
replicate :: Storable a => LazySize -> a -> Vector a
replicate size x =
   SV $ snd $
   List.mapAccumL
      (\v (ChunkSize m) ->
         if m <= V.length v
           then (v, V.take m v)
           else let v1 = V.replicate m x
                in  (v1,v1))
      V.empty $
   LS.toChunks size

{-
replicate :: Storable a => LazySize -> a -> Vector a
replicate size x =
   SV $ List.map (\(ChunkSize m) -> V.replicate m x) (LS.toChunks size)
-}


-- * Basic interface

length :: Vector a -> LazySize
length = LS.fromChunks . List.map chunkLength . LSV.chunks

chunkLength :: V.Vector a -> ChunkSize
chunkLength = ChunkSize . V.length

decrementLimit :: V.Vector a -> LazySize -> LazySize
decrementLimit x y =
   y -| LS.fromNumber (chunkLength x)

intFromChunkSize :: ChunkSize -> Int
intFromChunkSize (ChunkSize x) = x

intFromLazySize :: LazySize -> Int
intFromLazySize =
   List.sum . List.map intFromChunkSize . LS.toChunks



-- * sub-vectors

{- |
Generates laziness breaks
wherever either the lazy length number or the vector has a chunk boundary.
-}
{-# INLINE take #-}
take :: (Storable a) => LazySize -> Vector a -> Vector a
take n = fst . splitAt n

{- |
Preserves the chunk pattern of the lazy vector.
-}
{-# INLINE takeVectorPattern #-}
takeVectorPattern :: (Storable a) => LazySize -> Vector a -> Vector a
takeVectorPattern _ (SV []) = empty
takeVectorPattern n (SV (x:xs)) =
   if List.null (LS.toChunks n)
     then empty
     else
       let remain = decrementLimit x n
       in  SV $ uncurry (:) $
           if LS.isNull remain
             then (V.take (intFromLazySize n) x, [])
             else
               (x, LSV.chunks $ take remain $ LSV.fromChunks xs)

{-# INLINE drop #-}
drop :: (Storable a) => LazySize -> Vector a -> Vector a
drop size xs =
   List.foldl (flip (LSV.drop . intFromChunkSize)) xs (LS.toChunks size)

{-# INLINE splitAt #-}
splitAt ::
   (Storable a) => LazySize -> Vector a -> (Vector a, Vector a)
splitAt size xs =
   mapFst LSV.concat $ swap $
   List.mapAccumL
      (\xs0 n ->
         swap $ LSV.splitAt (intFromChunkSize n) xs0)
      xs (LS.toChunks size)

{-# INLINE splitAtVectorPattern #-}
splitAtVectorPattern ::
   (Storable a) => LazySize -> Vector a -> (Vector a, Vector a)
splitAtVectorPattern n0 =
   forcePair .
   if List.null (LS.toChunks n0)
     then (,) empty
     else
       let recourse n xt =
              forcePair $
              case xt of
                 [] -> ([], [])
                 (x:xs) ->
                    let remain = decrementLimit x n
                    in  if LS.isNull remain
                          then mapPair ((:[]), (:xs)) $
                               V.splitAt (intFromLazySize n) x
                          else mapFst (x:) $ recourse remain xs
       in  mapPair (SV, SV) . recourse n0 . LSV.chunks


{-# INLINE [0] zipWithSize #-}
zipWithSize :: (Storable a, Storable b, Storable c) =>
      LazySize
   -> (a -> b -> c)
   -> Vector a
   -> Vector b
   -> Vector c
zipWithSize size f =
   curry (fst . unfoldrN size (\(xt,yt) ->
      liftM2
         (\(x,xs) (y,ys) -> (f x y, (xs,ys)))
         (viewL xt)
         (viewL yt)))

{-# INLINE zipWithSize3 #-}
zipWithSize3 ::
   (Storable a, Storable b, Storable c, Storable d) =>
   LazySize -> (a -> b -> c -> d) ->
   (Vector a -> Vector b -> Vector c -> Vector d)
zipWithSize3 size f s0 s1 s2 =
   fst $ unfoldrN size (\(xt,yt,zt) ->
      liftM3
         (\(x,xs) (y,ys) (z,zs) ->
             (f x y z, (xs,ys,zs)))
         (viewL xt)
         (viewL yt)
         (viewL zt))
      (s0,s1,s2)

{-# INLINE zipWithSize4 #-}
zipWithSize4 ::
   (Storable a, Storable b, Storable c, Storable d, Storable e) =>
   LazySize -> (a -> b -> c -> d -> e) ->
   (Vector a -> Vector b -> Vector c -> Vector d -> Vector e)
zipWithSize4 size f s0 s1 s2 s3 =
   fst $ unfoldrN size (\(xt,yt,zt,wt) ->
      liftM4
         (\(x,xs) (y,ys) (z,zs) (w,ws) ->
             (f x y z w, (xs,ys,zs,ws)))
         (viewL xt)
         (viewL yt)
         (viewL zt)
         (viewL wt))
      (s0,s1,s2,s3)

{-
{- |
Ensure a minimal length of the list by appending pad values.
-}
{-# ONLINE pad #-}
pad :: (Storable a) => ChunkSize -> a -> Int -> Vector a -> Vector a
pad size y n0 =
   let recourse n xt =
          if n<=0
            then xt
            else
              case xt of
                 [] -> chunks $ replicate size n y
                 x:xs -> x : recourse (n - V.length x) xs
   in  SV . recourse n0 . chunks

padAlt :: (Storable a) => ChunkSize -> a -> Int -> Vector a -> Vector a
padAlt size x n xs =
   append xs
      (let m = length xs
       in  if n>m
             then replicate size (n-m) x
             else empty)
-}
