{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Storable.Filter.NonRecursive (
   delay,
   delayPad,
   delayPos,
   delayNeg,

   downsample2,
   sumsDownsample2,
   convolveDownsample2,
   inverseFrequencyModulationFloor,
   sumsPosModulatedPyramid,
   accumulatePosModulatedPyramid,
   accumulateBinPosModulatedPyramid,
   movingAverageModulatedPyramid,
   movingAccumulateModulatedPyramid,

   -- for testing
   sumsDownsample2Alt,
   pyramid,
   ) where

import qualified Synthesizer.Storable.Signal as SigSt
import qualified Data.StorableVector as V
import qualified Data.StorableVector.Pointer as VPtr
import qualified Data.StorableVector.Lazy as VL
import qualified Data.StorableVector.Lazy.Pattern as VP

import qualified Synthesizer.Basic.Filter.NonRecursive as Filt
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as SigS
import qualified Synthesizer.Plain.Signal as Sig

import qualified Algebra.Module         as Module
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import Foreign.Storable (Storable, )
import Foreign.Storable.Tuple ()

import Control.Monad (mplus, )

import qualified Data.List as List
import Data.Tuple.HT (mapFst, mapSnd, mapPair, swap, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (fromMaybe, )

import qualified Numeric.NonNegative.Chunky as NonNegChunky

import NumericPrelude.Numeric
import NumericPrelude.Base as NP



{-# INLINE delay #-}
delay :: (Additive.C y, Storable y) => Int -> SigSt.T y -> SigSt.T y
delay = delayPad zero

{-# INLINE delayPad #-}
delayPad :: (Storable y) => y -> Int -> SigSt.T y -> SigSt.T y
delayPad z n =
   if n<0
     then delayNeg (Additive.negate n)
     else delayPosPad z n

{-# INLINE delayPos #-}
delayPos :: (Additive.C y, Storable y) => Int -> SigSt.T y -> SigSt.T y
delayPos = delayPosPad zero

{-# INLINE delayPosPad #-}
delayPosPad :: (Storable v) => v -> Int -> SigSt.T v -> SigSt.T v
delayPosPad z n = SigSt.append (SigSt.replicate SigSt.defaultChunkSize n z)

{-# INLINE delayNeg #-}
delayNeg :: (Storable y) => Int -> SigSt.T y -> SigSt.T y
delayNeg = SigSt.drop


{- |
The Maybe type carries an unpaired value from one block to the next one.
-}
accumulateDownsample2Strict ::
   (Storable v) =>
   (v -> v -> v) ->
   Maybe v -> V.Vector v -> (Maybe v, V.Vector v)
accumulateDownsample2Strict acc carry ys =
   mapFst (\v -> fmap fst $ V.viewL . snd =<< v) $ swap $
   V.unfoldrN (div (V.length ys + maybe 0 (const 1) carry) 2) (\(carry0,xs0) ->
      do (x0,xs1) <- mplus (fmap (\c -> (c, xs0)) carry0) (V.viewL xs0)
         (x1,xs2) <- V.viewL xs1
         return (acc x0 x1, (Nothing, xs2)))
      (carry, ys)

accumulateDownsample2 ::
   (Storable v) =>
   (v -> v -> v) ->
   SigSt.T v -> SigSt.T v
accumulateDownsample2 acc =
   SigSt.fromChunks .
   filter (not . V.null) .
   (\(carry, chunks) ->
      chunks ++ maybe [] (\cr -> [V.singleton cr]) carry) .
   List.mapAccumL (accumulateDownsample2Strict acc) Nothing .
   SigSt.chunks

sumsDownsample2 ::
   (Additive.C v, Storable v) =>
   SigSt.T v -> SigSt.T v
sumsDownsample2 =
   accumulateDownsample2 (+)

sumsDownsample2Alt ::
   (Additive.C v, Storable v) =>
   SigSt.T v -> SigSt.T v
sumsDownsample2Alt ys =
   fst .
   VP.unfoldrN (halfLazySize $ VP.length ys) (\xs ->
      flip fmap (SigS.viewL xs) $ \xxs0@(x0,xs0) ->
         SigS.switchL xxs0 {- xs0 is empty -}
            (\ x1 xs1 -> (x0+x1, xs1))
            xs0)
    . SigS.fromStorableSignal $ ys

{-
! - downsample

x ! 2
(y*x)    ! 2 = y    ! 2 * x ! 2  +  y->1 ! 2 * x<-1 ! 2
(y*x<-1) ! 2 = y<-1 ! 2 * x ! 2  +  y    ! 2 * x<-1 ! 2

(y^n*x) ! 2 can be implemented by matrix power and multiplication
where multiplication is convolution.
-}

convolveDownsample2 ::
   (Module.C a v, Storable a, Storable v) =>
   SigSt.T a -> SigSt.T v -> SigSt.T v
convolveDownsample2 ms ys =
   let mac =
          SigS.sum . SigS.zipWith (*>)
             (SigS.fromStorableSignal ms)
   in  fst .
       VP.unfoldrN (halfLazySize $ VP.length ys) (\xs ->
          toMaybe (not $ SigSt.null xs)
             (mac (SigS.fromStorableSignal xs),
              SigSt.drop 2 xs))
        $ ys


halfLazySize :: NonNegChunky.T VP.ChunkSize -> NonNegChunky.T VP.ChunkSize
halfLazySize =
   NonNegChunky.fromChunks .
   filter (VL.ChunkSize zero /=) .
   (\(c,ls) -> ls ++ [VL.ChunkSize c]) .
   List.mapAccumL (\c (VL.ChunkSize l) ->
      mapSnd VL.ChunkSize $ swap $ divMod (c+l) 2) zero .
   NonNegChunky.toChunks


{- |
offset must be zero or one.
-}
downsample2Strict ::
   (Storable v) =>
   Int -> V.Vector v -> V.Vector v
downsample2Strict offset ys =
   fst $
   V.unfoldrN (- div (offset - V.length ys) 2)
      (fmap (mapSnd laxTailStrict) . V.viewL) $
   if offset == 0
     then ys
     else laxTailStrict ys

laxTailStrict ::
   (Storable v) =>
   V.Vector v -> V.Vector v
laxTailStrict ys =
   V.switchL ys (flip const) ys

downsample2 ::
   (Storable v) =>
   SigSt.T v -> SigSt.T v
downsample2 =
   SigSt.fromChunks .
   filter (not . V.null) .
   snd .
   List.mapAccumL
      (\k c ->
         (mod (k + V.length c) 2, downsample2Strict k c)) zero .
   SigSt.chunks


{-
The laziness granularity of the input signal is maintained.
-}
pyramid ::
   (Storable v) =>
   (v -> v -> v) ->
   Int -> SigSt.T v -> [SigSt.T v]
pyramid acc height =
   take (1+height) . iterate (accumulateDownsample2 acc)



{- |
Moving average, where window bounds must be always non-negative.

The laziness granularity is @2^height@.

This function is only slightly more efficient
than its counterpart from Generic.Filter,
since it generates strict blocks
and not one-block chunky signals.
-}
accumulatePosModulatedPyramid ::
   (Storable v) =>
   ([SigSt.T v] -> (Int,Int) -> v) ->
   ([Int], [SigSt.T v]) ->
   SigSt.T (Int,Int) -> SigSt.T v
accumulatePosModulatedPyramid accumulate (sizes,pyr0) ctrl =
   let blockSize = head sizes
       pyrStarts = iterate (zipWith SigSt.drop sizes) pyr0
       ctrlBlocks = SigS.toList $ SigG.sliceVertical blockSize ctrl
   in  SigSt.fromChunks $
       zipWith
          (\pyr ->
              SigS.toStrictStorableSignal blockSize .
              SigS.map (accumulate pyr) .
              SigS.zipWith (\d -> mapPair ((d+), (d+))) (SigS.iterate (1+) 0) .
              SigS.fromStorableSignal)
          pyrStarts ctrlBlocks

sumsPosModulatedPyramid ::
   (Additive.C v, Storable v) =>
   Int -> SigSt.T (Int,Int) -> SigSt.T v -> SigSt.T v
sumsPosModulatedPyramid height ctrl xs =
   accumulatePosModulatedPyramid
      FiltG.sumRangeFromPyramid
      (addSizes $ pyramid (+) height xs)
      ctrl

accumulateBinPosModulatedPyramid ::
   (Storable v) =>
   (v -> v -> v) ->
   Int -> SigSt.T (Int,Int) -> SigSt.T v -> SigSt.T v
accumulateBinPosModulatedPyramid acc height ctrl xs =
   accumulatePosModulatedPyramid
      (\pyr ->
         fromMaybe (error "accumulateBinPosModulatedPyramid: empty window") .
         FiltG.maybeAccumulateRangeFromPyramid acc pyr)
      (addSizes $ pyramid acc height xs)
      ctrl

addSizes :: [signal] -> ([Int], [signal])
addSizes pyr = (Filt.unitSizesFromPyramid pyr, pyr)


{- |
The first argument is the amplification.
The main reason to introduce it,
was to have only a Module constraint instead of Field.
This way we can also filter stereo signals.
-}
movingAverageModulatedPyramid ::
   (Field.C a, Module.C a v, Storable Int, Storable v) =>
   a -> Int -> Int -> SigSt.T Int -> SigSt.T v -> SigSt.T v
movingAverageModulatedPyramid amp height maxC ctrl0 =
   withPaddedInput zero
      (\ctrl xs ->
         SigSt.zipWith (\c x -> (amp / fromIntegral (2*c+1)) *> x) ctrl0 $
         sumsPosModulatedPyramid height ctrl xs)
      maxC ctrl0


movingAccumulateModulatedPyramid ::
   (Storable v) =>
   (v -> v -> v) ->
   v -> Int -> Int -> SigSt.T Int -> SigSt.T v -> SigSt.T v
movingAccumulateModulatedPyramid acc pad height =
   withPaddedInput pad $
   accumulateBinPosModulatedPyramid acc height


withPaddedInput ::
   (Storable y) =>
   y -> (SigSt.T (Int, Int) -> SigSt.T y -> v) ->
   Int -> SigSt.T Int -> SigSt.T y -> v
withPaddedInput pad proc maxC ctrl xs =
   proc
      (SigSt.map (\c -> (maxC - c, maxC + c + 1)) ctrl)
      (delayPad pad maxC xs)


{- |
The function is like that of
'Synthesizer.State.Filter.NonRecursive.inverseFrequencyModulationFloor',
but this function preserves in a sense the chunk structure.

The result will have laziness breaks at least
at the chunk boundaries that correspond to the breaks in the input signal.
However we insert more breaks,
such that a maximum chunk size can be warrented.
(Since control and input signal are aligned in time,
we might as well use the control chunk structure.
Currently I do not know what is better.
For the above example it doesn't matter.
We might implement a variant in Causal.Filter.NonRecursive.)

This function cannot be written using generic functions,
since we have to inspect the chunks individually.
-}
{-# INLINE inverseFrequencyModulationFloor #-}
inverseFrequencyModulationFloor ::
   (Storable v, SigG.Read sig t, Ring.C t, Ord t) =>
   SigSt.ChunkSize ->
   sig t -> SigSt.T v -> SigSt.T v
inverseFrequencyModulationFloor chunkSize ctrl =
   SigG.runViewL ctrl (\nextC cst0 ->
      SigSt.concat .
      Sig.crochetL
         (\chunk ms -> flip fmap ms $ \ts ->
            inverseFrequencyModulationChunk chunkSize
               nextC ts chunk)
         (Just (0,cst0)) .
      SigSt.chunks)

{-# INLINE inverseFrequencyModulationChunk #-}
inverseFrequencyModulationChunk ::
   (Storable v, Ring.C t, Ord t) =>
   SigSt.ChunkSize ->
   (s -> Maybe (t,s)) -> (t,s) -> V.Vector v -> (SigSt.T v, Maybe (t,s))
inverseFrequencyModulationChunk chunkSize nextC (phase,cst0) chunk =
   let {-# INLINE switch #-}
       {-
       switch ::
          (Maybe (t, s) -> r) ->
          ((t, v) -> (s, VPtr.Pointer v) -> r) ->
          t ->
          (s, VPtr.Pointer v) -> r
       -}
       {-
       This is a combination of two switches,
       that simulate a switch on (zip ctrl xs).
       -}
       switch l r t (cp0,xp0) =
          maybe
             (l Nothing)
             (\(c1,cp1) ->
                VPtr.switchL
                   (l (Just (t,cp0)))
                   (\x1 xp1 -> r (t+c1,x1) (cp1,xp1))
                   xp0)
             (nextC cp0)

       {-# INLINE go #-}
       {-
       go ::
          (t,v) -> (s, VPtr.Pointer v) ->
          Either (Maybe (t,s)) (v, ((t,v), (s, VPtr.Pointer v)))
       -}
       go (c,x) cxp =
          if c<1
            then switch Left go c cxp
            else Right (x, ((c-1,x),cxp))

   in  switch ((,) SigSt.empty)
          (curry $ VL.unfoldrResult chunkSize (uncurry go))
          phase (cst0, VPtr.cons chunk)
