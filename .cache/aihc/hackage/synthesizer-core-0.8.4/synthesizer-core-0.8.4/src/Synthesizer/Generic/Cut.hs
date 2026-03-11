{- |
This module allows abstraction of operations
that operate on the time axis
and do also work on signal types without sample values.
The most distinctive instances are certainly
Dirac signals and chunky time values.
-}
module Synthesizer.Generic.Cut where

import qualified Synthesizer.Plain.Signal as Sig
import qualified Synthesizer.State.Signal as SigS
import qualified Data.StorableVector as SV
import qualified Data.StorableVector.Lazy as SVL

import qualified Algebra.ToInteger as ToInteger
import qualified Algebra.Ring as Ring

import qualified Data.EventList.Relative.BodyTime as EventList
import qualified Data.EventList.Relative.TimeTime as EventListTT
import qualified Data.EventList.Relative.MixedTime as EventListMT

import qualified Algebra.NonNegative as NonNeg
import qualified Number.NonNegativeChunky as Chunky

import qualified Numeric.NonNegative.Class as NonNeg98
import qualified Numeric.NonNegative.Chunky as Chunky98

import Foreign.Storable (Storable, )
import Control.DeepSeq (NFData, rnf, )

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import Data.Function (fix, )
import Data.Tuple.HT (mapPair, mapFst, mapSnd, )
import Data.Monoid (Monoid, mempty, )

import qualified Prelude as P
import NumericPrelude.Numeric
import Prelude
   (Bool, String, (++), error,
    pred, (==), (<=), (>=), (<),
    (.), ($), const, snd,
    not, (||), (&&), min, max, )


class Read sig where
   null :: sig -> Bool
   length :: sig -> Int

class (Read sig) => NormalForm sig where
   {- |
   Evaluating the first value of the signal
   is necessary for avoiding a space leaks
   if you repeatedly drop a prefix from the signal
   and do not consume something from it.
   -}
   evaluateHead :: sig -> ()

class (Read sig, Monoid sig) => Transform sig where
   {- Monoid functions
   {-
   In our categorization 'empty' would belong to the Write class,
   but since an empty signal contains no data,
   the maximum packet size is irrelevant.
   This makes e.g. the definition of mixMulti more general.
   -}
   empty :: sig
   cycle :: sig -> sig
   append :: sig -> sig -> sig
   concat :: [sig] -> sig
   -}
   take :: Int -> sig -> sig
   drop :: Int -> sig -> sig
   -- can occur in an inner loop in Interpolation
   dropMarginRem :: Int -> Int -> sig -> (Int, sig)
   splitAt :: Int -> sig -> (sig, sig)
   reverse :: sig -> sig


instance Storable y => Read (SV.Vector y) where
   {-# INLINE null #-}
   null = SV.null
   {-# INLINE length #-}
   length = SV.length

instance (Storable y) => NormalForm (SV.Vector y) where
   {-# INLINE evaluateHead #-}
   evaluateHead x =
      if SV.null x then () else ()

instance Storable y => Transform (SV.Vector y) where
   {-# INLINE take #-}
   take = SV.take
   {-# INLINE drop #-}
   drop = SV.drop
   {-# INLINE splitAt #-}
   splitAt = SV.splitAt
   {-# INLINE dropMarginRem #-}
   dropMarginRem n m xs =
      let d = min m $ max 0 $ SV.length xs - n
      in  (m-d, SV.drop d xs)
   {-# INLINE reverse #-}
   reverse = SV.reverse


-- instance Storable y => Read SigSt.T y where
instance Storable y => Read (SVL.Vector y) where
   {-# INLINE null #-}
   null = SVL.null
   {-# INLINE length #-}
   length = SVL.length

instance (Storable y) => NormalForm (SVL.Vector y) where
   {-# INLINE evaluateHead #-}
   evaluateHead =
      ListHT.switchL () (\x _ -> evaluateHead x) . SVL.chunks
--      ListHT.switchL () (\x _ -> rnf x) . SVL.chunks
--   evaluateHead x =
--      if SVL.null x then () else ()

{-
instance (Storable y, NFData y) => NormalForm (SVL.Vector y) where
   {-# INLINE evaluateHead #-}
   evaluateHead x = SVL.switchL () (\x _ -> rnf x)
-}

instance Storable y => Transform (SVL.Vector y) where
   {-
   {-# INLINE empty #-}
   empty = SVL.empty
   {-# INLINE cycle #-}
   cycle = SVL.cycle
   {-# INLINE append #-}
   append = SVL.append
   {-# INLINE concat #-}
   concat = SVL.concat
   -}
   {-# INLINE take #-}
   take = SVL.take
   {-# INLINE drop #-}
   drop = SVL.drop
   {-# INLINE splitAt #-}
   splitAt = SVL.splitAt
   {-# INLINE dropMarginRem #-}
   dropMarginRem = SVL.dropMarginRem
   {-# INLINE reverse #-}
   reverse = SVL.reverse


instance Read ([] y) where
   {-# INLINE null #-}
   null = List.null
   {-# INLINE length #-}
   length = List.length

instance (NFData y) => NormalForm ([] y) where
   {-# INLINE evaluateHead #-}
   evaluateHead = ListHT.switchL () (\x _ -> rnf x)

instance Transform ([] y) where
   {-
   {-# INLINE empty #-}
   empty = []
   {-# INLINE cycle #-}
   cycle = List.cycle
   {-# INLINE append #-}
   append = (List.++)
   {-# INLINE concat #-}
   concat = List.concat
   -}
   {-# INLINE take #-}
   take = List.take
   {-# INLINE drop #-}
   drop = List.drop
   {-# INLINE dropMarginRem #-}
   dropMarginRem = Sig.dropMarginRem
   {-# INLINE splitAt #-}
   splitAt = List.splitAt
   {-# INLINE reverse #-}
   reverse = List.reverse


instance Read (SigS.T y) where
   {-# INLINE null #-}
   null = SigS.null
   {-# INLINE length #-}
   length = SigS.length

instance (NFData y) => NormalForm (SigS.T y) where
   {-
   Evaluating the first element of a generator might look silly,
   since it is not stored in a data structure.
   However, the generator depends on an internal state,
   which might be in turn a list or a storable vector,
   which is evaluated then.
   -}
   {-# INLINE evaluateHead #-}
   evaluateHead = SigS.switchL () (\x _ -> rnf x)

instance Transform (SigS.T y) where
   {-
   {-# INLINE empty #-}
   empty = SigS.empty
   {-# INLINE cycle #-}
   cycle = SigS.cycle
   {-# INLINE append #-}
   append = SigS.append
   {-# INLINE concat #-}
   concat = SigS.concat
   -}

   {-# INLINE take #-}
   take = SigS.take
   {-# INLINE drop #-}
   drop = SigS.drop
   {-# INLINE dropMarginRem #-}
   dropMarginRem = SigS.dropMarginRem
   {-# INLINE splitAt #-}
   splitAt n =
      -- This implementation is slow. Better leave it unimplemented?
      mapPair (SigS.fromList, SigS.fromList) .
      List.splitAt n . SigS.toList
   {-# INLINE reverse #-}
   reverse = SigS.reverse


{- |
We abuse event lists for efficient representation of piecewise constant signals.
-}
instance (P.Integral t) => Read (EventList.T t y) where
   null = EventList.null
   length = fromIntegral . P.toInteger . P.sum . EventList.getTimes

instance (P.Integral t, NFData y) => NormalForm (EventList.T t y) where
   evaluateHead = EventList.switchL () (\x _ _ -> rnf x)


{-
needed for chunks of MIDI events as input to CausalIO processes
-}
instance (P.Integral t) => Read (EventListTT.T t y) where
   null = EventListMT.switchTimeL (\t xs -> t==0 && EventList.null xs)
   length = fromIntegral . P.toInteger . P.sum . EventListTT.getTimes

instance (P.Integral t, NonNeg98.C t) => Transform (EventListTT.T t y) where
   take = EventListTT.takeTime . P.fromIntegral
   drop = EventListTT.dropTime . P.fromIntegral
   dropMarginRem =
      dropMarginRemChunky (P.map P.fromIntegral . EventListTT.getTimes)
   splitAt = EventListTT.splitAtTime . P.fromIntegral
   reverse = EventListTT.reverse


-- cf. StorableVector.Lazy.dropMarginRem
dropMarginRemChunky ::
   (Transform sig) =>
   (sig -> [Int]) -> Int -> Int -> sig -> (Int, sig)
dropMarginRemChunky getTimes n m xs =
   List.foldl'
      (\(mi,xsi) k -> (mi-k, drop k xsi))
      (m, xs)
      (getTimes $ take m $ drop n xs)

{- |
The function defined here are based on the interpretation
of event lists as piecewise constant signals.
They do not fit to the interpretation of atomic events.
Because e.g. it makes no sense to split an atomic event into two instances by splitAt,
and it is also not clear, whether dropping the first chunk
shall leave a chunk of length zero
or remove that chunk completely.

However, sometimes we also need lists of events.
In this case the 'reverse' method would be different.
For an event-oriented instance of EventList.TimeTime
see NoteOffList in synthesizer-alsa package.
-}
instance (P.Integral t, NonNeg98.C t) => Transform (EventList.T t y) where
   take n xs =
      EventList.foldrPair
         (\b t go remain ->
            if remain <= NonNeg98.zero
              then EventList.empty
              else
                let (m, ~(le,d)) = NonNeg98.split t remain
                in  EventList.cons b m $
                    go (if le then d else NonNeg98.zero))
         (const EventList.empty) xs
         (P.fromIntegral n)

   drop =
      let recourse n =
             EventList.switchL EventList.empty $ \b t xs ->
             let (le,d) = snd $ NonNeg98.split t n
             in  if le
                   then recourse d xs
                   else EventList.cons b d xs
      in  recourse . P.fromIntegral

   dropMarginRem =
      dropMarginRemChunky (P.map P.fromIntegral . EventList.getTimes)

   -- cf. StorableVector.Lazy.splitAt
   splitAt =
      let recourse 0 = (,) EventList.empty
          recourse n =
             EventList.switchL (EventList.empty, EventList.empty) $ \b t xs ->
             let (m, ~(le,d)) = NonNeg98.split t n
             in  mapFst (EventList.cons b m) $
                 if le
                   then recourse d xs
                   else (EventList.empty, EventList.cons b d xs)
      in  recourse . P.fromIntegral

   reverse =
      EventList.fromPairList . List.reverse . EventList.toPairList


{-
useful for application of non-negative chunky numbers as gate signals
-}
instance (ToInteger.C a, NonNeg.C a) => Read (Chunky.T a) where
   {-# INLINE null #-}
   null = List.null . Chunky.toChunks
   {-# INLINE length #-}
   length = sum . List.map (fromIntegral . toInteger) . Chunky.toChunks

instance (ToInteger.C a, NonNeg.C a, NFData a) => NormalForm (Chunky.T a) where
   {-# INLINE evaluateHead #-}
   evaluateHead = ListHT.switchL () (\x _ -> rnf x) . Chunky.toChunks


intToChunky :: (Ring.C a, NonNeg.C a) => String -> Int -> Chunky.T a
intToChunky name =
   Chunky.fromNumber .
-- the non-negative type is not necessarily a wrapper
--   NonNegW.fromNumberMsg ("Generic.Cut."++name) .
   fromIntegral .
   (\x ->
      if x<zero
        then error ("Generic.Cut.NonNeg.Chunky."++name++": negative argument")
        else x)


instance (ToInteger.C a, NonNeg.C a) => Transform (Chunky.T a) where
   {-# INLINE take #-}
   take n = P.min (intToChunky "take" n)
   {-# INLINE drop #-}
   drop n x = x NonNeg.-| intToChunky "drop" n
   {-# INLINE dropMarginRem #-}
   dropMarginRem n m x =
      let (z,~(b,d)) =
             Chunky.minMaxDiff
                (intToChunky "dropMargin/n" m)
                (x NonNeg.-| intToChunky "dropMargin/m" n)
      in  (if b then 0 else fromIntegral (Chunky.toNumber d),
           x NonNeg.-| z)
   {-# INLINE splitAt #-}
   splitAt n x =
      mapSnd
         (\ ~(b,d) -> if b then d else mempty)
         (Chunky.minMaxDiff (intToChunky "splitAt" n) x)
   {-# INLINE reverse #-}
   reverse = Chunky.fromChunks . List.reverse . Chunky.toChunks



instance (P.Integral a) => Read (Chunky98.T a) where
   {-# INLINE null #-}
   null = List.null . Chunky98.toChunks
   {-# INLINE length #-}
   length = sum . List.map (P.fromIntegral . P.toInteger) . Chunky98.toChunks

instance (P.Integral a, NonNeg.C a, NFData a) =>
      NormalForm (Chunky98.T a) where
   {-# INLINE evaluateHead #-}
   evaluateHead = ListHT.switchL () (\x _ -> rnf x) . Chunky98.toChunks


intToChunky98 :: (P.Num a, NonNeg98.C a) => String -> Int -> Chunky98.T a
intToChunky98 name =
   Chunky98.fromNumber .
--   NonNegW.fromNumberMsg ("Generic.Cut."++name) .
   P.fromIntegral .
   (\x ->
      if x<0
        then error ("Generic.Cut.NonNeg.Chunky98."++name++": negative argument")
        else x)

instance (P.Integral a, NonNeg98.C a) => Transform (Chunky98.T a) where
   {-# INLINE take #-}
   take n = P.min (intToChunky98 "take" n)
   {-# INLINE drop #-}
   drop n x = x NonNeg98.-| intToChunky98 "drop" n
   {-# INLINE dropMarginRem #-}
   dropMarginRem n m x =
      let (z,~(b,d)) =
             NonNeg98.split
                (intToChunky98 "dropMargin/n" m)
                (x NonNeg98.-| intToChunky98 "dropMargin/m" n)
      in  (if b then 0 else P.fromIntegral (Chunky98.toNumber d),
           x NonNeg98.-| z)
   {-# INLINE splitAt #-}
   splitAt n x =
      mapSnd
         (\ ~(b,d) -> if b then d else Chunky98.zero)
         (NonNeg98.split (intToChunky98 "splitAt" n) x)
   {-# INLINE reverse #-}
   reverse = Chunky98.fromChunks . List.reverse . Chunky98.toChunks


{-# INLINE empty #-}
empty :: (Monoid sig) => sig
empty = Monoid.mempty

{-# INLINE cycle #-}
cycle :: (Monoid sig) => sig -> sig
cycle x = fix (append x)

{-# INLINE append #-}
append :: (Monoid sig) => sig -> sig -> sig
append = Monoid.mappend

{-# INLINE concat #-}
concat :: (Monoid sig) => [sig] -> sig
concat = Monoid.mconcat


{- |
Like @lengthAtLeast n xs  =  length xs >= n@,
but is more efficient, because it is more lazy.
-}
{-# INLINE lengthAtLeast #-}
lengthAtLeast :: (Transform sig) =>
   Int -> sig -> Bool
lengthAtLeast n xs =
   n<=0 || not (null (drop (pred n) xs))

{-# INLINE lengthAtMost #-}
lengthAtMost :: (Transform sig) =>
   Int -> sig -> Bool
lengthAtMost n xs =
   n>=0 && null (drop n xs)

{-# INLINE sliceVertical #-}
sliceVertical :: (Transform sig) =>
   Int -> sig -> SigS.T sig
sliceVertical n =
   SigS.map (take n) .
   SigS.takeWhile (not . null) .
   SigS.iterate (drop n)
