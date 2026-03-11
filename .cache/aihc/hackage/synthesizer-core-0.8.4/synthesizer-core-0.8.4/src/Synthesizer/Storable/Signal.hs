{-# OPTIONS_GHC -fenable-rewrite-rules #-}
{- |
Chunky signal stream build on StorableVector.

Hints for fusion:
 - Higher order functions should always be inlined in the end
   in order to turn them into machine loops
   instead of calling a function in an inner loop.
-}
module Synthesizer.Storable.Signal (
   T,
   Vector.hPut,
   ChunkSize, Vector.chunkSize, defaultChunkSize,
   -- for Storable.Oscillator
   scanL,
   Vector.map,
   Vector.iterate,
   Vector.zipWith,
   -- for State.Signal
   Vector.append,
   Vector.concat,
   Vector.span,
   Vector.splitAt,
   Vector.viewL,
   Vector.viewR,
   Vector.switchL,
   Vector.unfoldr,
   Vector.reverse,
   Vector.crochetL,
   -- for Dimensional.File
   Vector.writeFile,
   -- for Storable.Cut
   mix, mixSndPattern, mixSize,
   splitAtPad,
   Vector.null,
   Vector.fromChunks,
   Vector.foldr,
   -- for Storable.Filter.Comb
   delay,
   delayLoop,
   delayLoopOverlap,
   -- for FusionList.Storable
   Vector.empty,
   Vector.cons,
   Vector.replicate,
   Vector.repeat,
   Vector.drop,
   Vector.take,
   takeCrochet,
   fromList,
   -- for Generic.Signal
   zipWithRest,
   zipWithAppend,
   -- for Storable.ALSA.MIDI
   Vector.switchR,
   -- for Test.Filter
   toList,
   -- for Storable.Filter.NonRecursive
   Vector.chunks,

   -- just for fun
   genericLength,
   ) where

import qualified Data.List as List
import qualified Data.StorableVector.Lazy.Pointer as Pointer
import qualified Data.StorableVector.Lazy as Vector
import qualified Data.StorableVector as V
import Data.StorableVector.Lazy (ChunkSize(..))

import Foreign.Storable (Storable, )
import Foreign.Storable.Tuple ()

import qualified Synthesizer.Frame.Stereo as Stereo

import qualified Data.List.HT as ListHT
import Data.Maybe.HT (toMaybe, )
import Data.Tuple.HT (mapFst, mapSnd, mapPair, forcePair, )

import qualified Algebra.Ring      as Ring
import qualified Algebra.Additive  as Additive
import qualified Algebra.ToInteger as ToInteger

import qualified Number.NonNegativeChunky as Chunky
import qualified Number.NonNegative       as NonNeg

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


-- this form is needed for Storable signal embed in amplitude signal
type T = Vector.Vector


defaultChunkSize :: ChunkSize
defaultChunkSize = ChunkSize 1024


{-# INLINE fromList #-}
fromList :: (Storable a) => ChunkSize -> [a] -> T a
fromList = Vector.pack

{-# INLINE toList #-}
toList :: (Storable a) => T a -> [a]
toList = Vector.unpack


{-# INLINE scanL #-}
scanL :: (Storable a, Storable b) =>
   (a -> b -> a) -> a -> T b -> T a
scanL = Vector.scanl


{- |
This implementation generates laziness breaks
whereever one of the original sequences has laziness breaks.
It should be commutative in this respect.

It is more efficient than 'mixSize'
since it appends the rest of the longer signal without copying.
-}
{-# SPECIALISE mix :: T Double -> T Double -> T Double #-}
{-# SPECIALISE mix :: T Float -> T Float -> T Float #-}
{-# SPECIALISE mix :: T (Double,Double) -> T (Double,Double) -> T (Double,Double) #-}
{-# SPECIALISE mix :: T (Float,Float) -> T (Float,Float) -> T (Float,Float) #-}
{-# SPECIALISE mix :: T (Stereo.T Double) -> T (Stereo.T Double) -> T (Stereo.T Double) #-}
{-# SPECIALISE mix :: T (Stereo.T Float) -> T (Stereo.T Float) -> T (Stereo.T Float) #-}
{-# INLINE mix #-}
mix :: (Additive.C x, Storable x) =>
   T x ->
   T x ->
   T x
mix = zipWithAppend (+)
{-
List.map V.unpack $ Vector.chunks $ mix (fromList defaultChunkSize [1,2,3,4,5::P.Double]) (fromList defaultChunkSize [1,2,3,4])
-}

{- |
Mix while maintaining the pattern of the second operand.
This is closer to the behavior of Vector.zipWithLastPattern.
-}
{-# INLINE mixSndPattern #-}
mixSndPattern :: (Additive.C x, Storable x) =>
   T x ->
   T x ->
   T x
mixSndPattern xs0 ys0 =
   let recourse xs (y:ys) =
              snd (V.mapAccumL
                 (\p0 yi ->
                    Pointer.switchL (p0,yi)
                       (\xi p1 -> (p1,xi+yi)) p0)
                 (Pointer.cons xs) y)
              :
              recourse (Vector.drop (V.length y) xs) ys
       recourse xs [] = Vector.chunks xs
   in  Vector.fromChunks $
       recourse xs0 (Vector.chunks ys0)


{-# INLINE zipWithAppend #-}
zipWithAppend ::
   (Storable x) =>
   (x -> x -> x) ->
   T x -> T x -> T x
zipWithAppend f xs0 ys0 =
   let recourse xt@(x:_) yt@(y:_) =
          let z = V.zipWith f x y
              n = V.length z
          in  z : recourse
                     (Vector.chunks $ Vector.drop n $ Vector.fromChunks xt)
                     (Vector.chunks $ Vector.drop n $ Vector.fromChunks yt)
       recourse xs [] = xs
       recourse [] ys = ys
   in  Vector.fromChunks $
       recourse (Vector.chunks xs0) (Vector.chunks ys0)

{- |
It also preserves the chunk structure of the second signal,
which is essential if you want to limit look-ahead.

This implementation seems to have a memory leak!
-}
{-# INLINE _zipWithAppendRest #-}
_zipWithAppendRest ::
   (Storable x) =>
   (x -> x -> x) ->
   T x -> T x -> T x
_zipWithAppendRest f xs ys =
   uncurry Vector.append $ mapSnd snd $ zipWithRest f xs ys

{-# INLINE zipWithRest #-}
zipWithRest ::
   (Storable c, Storable x) =>
   (x -> x -> c) ->
   T x ->
   T x ->
   (Vector.Vector c, (Bool, T x))
zipWithRest f xs ys =
   let len = min (lazyLength xs) (lazyLength ys) :: Chunky.T NonNeg.Int
       (prefixX,suffixX) = genericSplitAt len xs
       (prefixY,suffixY) = genericSplitAt len ys
       second = Vector.null suffixX
   in  (Vector.zipWithLastPattern f prefixX prefixY,
        (second, if second then suffixY else suffixX))

{-
We should move that to StorableVector package,
but we cannot, since that's Haskell 98.
-}
genericSplitAt ::
   (Additive.C i, Ord i, ToInteger.C i, Storable x) =>
   i -> T x -> (T x, T x)
genericSplitAt n0 =
   let recourse n xs0 =
          forcePair $
          ListHT.switchL
             ([], [])
             (\x xs ->
                if isZero n
                  then ([], xs0)
                  else
                    let m = fromIntegral $ V.length x
                    in  if m<=n
                          then mapFst (x:) $ recourse (n-m) xs
                          else mapPair ((:[]), (:xs)) $
                               V.splitAt (fromInteger $ toInteger n) x)
             xs0
   in  mapPair (Vector.fromChunks, Vector.fromChunks) .
       recourse n0 . Vector.chunks


-- cf. Data.StorableVector.Lazy.Pattern.length
lazyLength :: (Ring.C i) =>
   T x -> i
lazyLength =
   List.foldr (+) zero . List.map (fromIntegral . V.length) . Vector.chunks

genericLength :: (Ring.C i) =>
   T x -> i
genericLength =
   sum . List.map (fromIntegral . V.length) . Vector.chunks


splitAtPad ::
   (Additive.C x, Storable x) =>
   ChunkSize -> Int -> T x -> (T x, T x)
splitAtPad size n =
   mapFst (Vector.pad size Additive.zero n) . Vector.splitAt n


{- disabled SPECIALISE mixSize :: ChunkSize -> T Double -> T Double -> T Double -}
{- disabled SPECIALISE mixSize :: ChunkSize -> T Float -> T Float -> T Float -}
{- disabled SPECIALISE mixSize :: ChunkSize -> T (Double,Double) -> T (Double,Double) -> T (Double,Double) -}
{- disabled SPECIALISE mixSize :: ChunkSize -> T (Float,Float) -> T (Float,Float) -> T (Float,Float) -}
{-# INLINE mixSize #-}
mixSize :: (Additive.C x, Storable x) =>
      ChunkSize
   -> T x
   -> T x
   -> T x
mixSize size xs ys =
   Vector.unfoldr size mixStep
      (Pointer.cons xs, Pointer.cons ys)


{-# INLINE mixStep #-}
mixStep :: (Additive.C x, Storable x) =>
   (Pointer.Pointer x, Pointer.Pointer x) ->
   Maybe (x, (Pointer.Pointer x, Pointer.Pointer x))
mixStep (xt,yt) =
   case (Pointer.viewL xt, Pointer.viewL yt) of
      (Just (x,xs), Just (y,ys)) -> Just (x+y, (xs,ys))
      (Nothing,     Just (y,ys)) -> Just (y,   (xt,ys))
      (Just (x,xs), Nothing)     -> Just (x,   (xs,yt))
      (Nothing,     Nothing)     -> Nothing



{-# INLINE delay #-}
delay :: (Storable y) =>
   ChunkSize -> y -> Int -> T y -> T y
delay size z n = Vector.append (Vector.replicate size n z)

{-# INLINE delayLoop #-}
delayLoop ::
   (Storable y) =>
      (T y -> T y)
            -- ^ processor that shall be run in a feedback loop
   -> T y   -- ^ prefix of the output, its length determines the delay
   -> T y
delayLoop proc prefix =
   let ys = Vector.append prefix (proc ys)
   in  ys


{-# INLINE delayLoopOverlap #-}
delayLoopOverlap ::
   (Additive.C y, Storable y) =>
      Int
   -> (T y -> T y)
            {- ^ Processor that shall be run in a feedback loop.
                 It's absolutely necessary that this function preserves the chunk structure
                 and that it does not look a chunk ahead.
                 That's guaranteed for processes that do not look ahead at all,
                 like 'Vector.map', 'Vector.crochetL' and
                 all of type @Causal.Process@. -}
   -> T y   -- ^ input
   -> T y   -- ^ output has the same length as the input
delayLoopOverlap time proc xs =
   let ys = Vector.zipWith (Additive.+) xs
               (delay (Vector.chunkSize time) Additive.zero time (proc ys))
   in  ys



{-# INLINE takeCrochet #-}
takeCrochet :: Storable a => Int -> T a -> T a
takeCrochet = Vector.crochetL (\x n -> toMaybe (n>0) (x, pred n))
