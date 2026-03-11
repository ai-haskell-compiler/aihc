{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Storable.Repair (removeClicks, ) where

import qualified Data.StorableVector.Lazy.Pattern as SVP
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV

import Control.Monad (guard, )

import qualified Data.List as List
import Data.Tuple.HT (mapFst, mapSnd, )
import Data.Maybe.HT (toMaybe, )
import Data.Word (Word8, )

import Foreign.Storable (Storable, )

import qualified Algebra.RealField as RealField
import qualified Algebra.Field     as Field
import qualified Algebra.Additive  as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


{-
could use Generic.Control.line
-}
ramp ::
   (Storable a, Field.C a) =>
   Int -> (a, a) -> SVL.Vector a
ramp n (y0, y1) =
   SVL.sampleN SVL.defaultChunkSize n $ \k ->
      (y0 * fromIntegral (n-k) + y1 * fromIntegral k)
         / fromIntegral n

svHead :: (Storable a) => SVL.Vector a -> a
svHead =
   SVL.switchL (error "head: empty storable vector") const

{- |
@less-than@ comparison where 'Nothing' means \"unbounded\".
-}
maybeLT :: Ord a => Maybe a -> a -> Bool
maybeLT mx y =
   maybe False (<y) mx


type Jump = Word8

up, down, smooth :: Jump
up = 1
down = -1
smooth = 0

{- |
both @up@ and @down@ threshold must be positive numbers.
-}
splitAtJump ::
   (Storable a, Additive.C a, Ord a) =>
   Int ->
   Maybe a -> Maybe a ->
   SVL.Vector a -> (SVL.Vector a, (Jump, SVL.Vector a))
splitAtJump slopeDistance thresholdUp thresholdDown xs =
   (\(beforeJump, afterJump) ->
      mapSnd ((,) (SVL.switchL up (\d _ -> d) afterJump)) $
      SVP.splitAt (SVP.length beforeJump) xs) $
   SVL.span (smooth==) $
   SVL.zipWith
      (\x0 x1 ->
         case (maybeLT thresholdUp (x1-x0),
               maybeLT thresholdDown (x0-x1)) of
            (True, _) -> up
            (_, True) -> down
            _ -> smooth) xs $
   SVL.switchR SVL.empty
      (\ys y ->
         SVL.append ys $
         SVL.fromChunks [SV.replicate slopeDistance y]) $
   SVL.drop slopeDistance xs

chopAtJumps ::
   (Storable a, Additive.C a, Ord a) =>
   Int -> Int ->
   Maybe a -> Maybe a ->
   SVL.Vector a -> (SVL.Vector a, [(Jump, SVP.Vector a)])
chopAtJumps
      slopeDistance minSpikeDistance
      thresholdUp thresholdDown =
   mapSnd
      (List.unfoldr
          (\(dir,rest) ->
             toMaybe (not $ SVL.null rest)
                (let (ys,zs) = SVL.splitAt minSpikeDistance rest
                 in  mapFst ((,) dir . SVL.append ys) $
                     splitAtJump slopeDistance thresholdUp thresholdDown zs)))
   .
   splitAtJump slopeDistance thresholdUp thresholdDown


{- |
Usage:
   @removeClicks
      slopeDistance maxSpikeWidth minSpikeDistance
      thresholdUp thresholdDown@

@slopeDistance@ is the distance of samples in which we analyse differences.
The smoother the spike slope the larger @slopeDistance@ must be.

@slopeDistance@ should be smaller than the _minimal_ spike width.
@maxSpikeWidth@ should be smaller than the minimal spike distance.
Spike distance is measured from one spike beginning to the next one.

@thresholdUp@ is the minimal difference of two samples at @slopeDistance@
that are to be considered an upward jump.
@thresholdDown@ is for downward jumps.
If a threshold is 'Nothing' then jumps in this direction are ignored.
You should only use this if you are very sure
that spikes with the according sign do not occur.
Otherwise the algorithm will be confused
by the jump in reverse direction at the end of the spike.

Example: @removeClicks 1 5 20 (Just 0.1) (Just 0.1)@.

The algorithm works as follows:
Chop the signal at jumps.
Then begin at a certain distance behind the jump
and search backwards for the matching jump at the end of the spike.
If the borders of a spike are found this way,
then they are connected by a linear ramp.
-}
removeClicks ::
   (Storable a, RealField.C a) =>
   Int -> Int -> Int ->
   Maybe a -> Maybe a ->
   SVL.Vector a -> SVL.Vector a
removeClicks
      slopeDistance maxSpikeWidth minSpikeDistance
      thresholdUp thresholdDown =
   SVL.concat
   .
   uncurry (:)
   .
   mapSnd
      (map
          (\(dir, chunk) ->
             uncurry SVL.append
             .
             mapFst
                ((\(ys, ~(_dir,click)) ->
                    SVL.append
                       (ramp (SVL.length click)
                           (svHead chunk, svHead click))
                       (SVL.reverse ys))
                 .
                 splitAtJump slopeDistance
                    (guard (dir==up)   >> thresholdUp)
                    (guard (dir==down) >> thresholdDown)
                 .
                 SVL.reverse)
             .
             SVL.splitAt maxSpikeWidth
             $
             chunk))
   .
   chopAtJumps
      slopeDistance minSpikeDistance
      thresholdUp thresholdDown
