{- |
Functions for cutting signals with respect to lazy chunky time measures.
This is essential for realtime applications.
-}
module Synthesizer.ChunkySize.Cut where

import qualified Synthesizer.ChunkySize as ChunkySize
import qualified Synthesizer.Generic.Cut as Cut
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.State.Signal as SigS

import qualified Data.StorableVector.Lazy.Pattern as SigStV
import qualified Data.StorableVector.Lazy as Vector
import Foreign.Storable (Storable)

import qualified Number.NonNegativeChunky as Chunky

import qualified Data.List.Match as Match
import qualified Data.List as List
import Data.Tuple.HT (mapPair, )
import Data.Monoid (Monoid, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding (splitAt, Read, )
import Prelude ()


class Cut.Read sig => Read sig where
   length :: sig -> ChunkySize.T

class (Read sig, Monoid sig) => Transform sig where
   take :: ChunkySize.T -> sig -> sig
   drop :: ChunkySize.T -> sig -> sig
   splitAt :: ChunkySize.T -> sig -> (sig, sig)


-- instance Storable y => Read SigSt.T y where
instance Storable y => Read (Vector.Vector y) where
   {-# INLINE length #-}
   length = ChunkySize.fromStorableVectorSize . SigStV.length

instance Storable y => Transform (Vector.Vector y) where
   {-# INLINE take #-}
   take = SigStV.take . ChunkySize.toStorableVectorSize
   {-# INLINE drop #-}
   drop = SigStV.drop . ChunkySize.toStorableVectorSize
   {-# INLINE splitAt #-}
   splitAt = SigStV.splitAt . ChunkySize.toStorableVectorSize


instance Read ([] y) where
   {-# INLINE length #-}
   length xs =
      Chunky.fromChunks $ Match.replicate xs $ SigG.LazySize one

instance Transform ([] y) where
   {-# INLINE take #-}
   take ns =
      Match.take (ChunkySize.toNullList ns)
   {-# INLINE drop #-}
   drop ns xs =
      -- 'drop' cannot make much use of laziness, thus 'foldl' is ok
      List.foldl
         (\x (SigG.LazySize n) -> List.drop n x)
         xs (Chunky.toChunks ns)
   {-# INLINE splitAt #-}
   splitAt ns =
      Match.splitAt (ChunkySize.toNullList ns)

{-
instance Read (SigFL.T y) where
   {-# INLINE length #-}
   length = SigFL.length

instance Transform (SigFL.T y) where
   {-# INLINE take #-}
   take = SigFL.take
   {-# INLINE drop #-}
   drop = SigFL.drop
   {-# INLINE splitAt #-}
   splitAt = SigFL.splitAt
-}

instance Read (SigS.T y) where
   {-# INLINE length #-}
   length =
      Chunky.fromChunks . SigS.toList .
      SigS.map (const (SigG.LazySize one))

instance Transform (SigS.T y) where
   {-# INLINE take #-}
   take size0 =
      SigS.crochetL
         (\x (n,ns) ->
            if n>zero
              then Just (x, (pred n, ns))
              else
                case ns of
                  SigG.LazySize m : ms -> Just (x, (pred m, ms))
                  [] -> Nothing)
         (zero, Chunky.toChunks $ Chunky.normalize size0)
   {-# INLINE drop #-}
   drop ns xs =
      List.foldl
         (\x (SigG.LazySize n) -> SigS.drop n x)
         xs (Chunky.toChunks ns)
   {-# INLINE splitAt #-}
   splitAt n =
      -- This implementation is slow. Better leave it unimplemented?
      mapPair (SigS.fromList, SigS.fromList) .
      splitAt n . SigS.toList


{-
{-
useful for application of non-negative chunky numbers as gate signals
-}
instance (ToInteger.C a, NonNeg.C a) => Read (Chunky.T a) where
   {-# INLINE length #-}
   length = sum . List.map (fromIntegral . toInteger) . Chunky.toChunks


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
      let (z,d,b) =
             Chunky.minMaxDiff
                (intToChunky "dropMargin/n" n)
                (x NonNeg.-| intToChunky "dropMargin/m" m)
      in  (if b then 0 else fromIntegral (Chunky.toNumber d),
           x NonNeg.-| z)
   {-# INLINE splitAt #-}
   splitAt n x =
      let (z,d,b) = Chunky.minMaxDiff (intToChunky "splitAt" n) x
      in  (z, if b then d else mempty)
   {-# INLINE reverse #-}
   reverse = Chunky.fromChunks . List.reverse . Chunky.toChunks



instance (P.Integral a) => Read (Chunky98.T a) where
   {-# INLINE null #-}
   null = List.null . Chunky98.toChunks
   {-# INLINE length #-}
   length = sum . List.map (P.fromIntegral . P.toInteger) . Chunky98.toChunks


intToChunky98 :: (NonNeg98.C a) => String -> Int -> Chunky98.T a
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
      let (z,d,b) =
             Chunky98.minMaxDiff
                (intToChunky98 "dropMargin/n" n)
                (x NonNeg98.-| intToChunky98 "dropMargin/m" m)
      in  (if b then 0 else P.fromIntegral (Chunky98.toNumber d),
           x NonNeg98.-| z)
   {-# INLINE splitAt #-}
   splitAt n x =
      let (z,d,b) = Chunky98.minMaxDiff (intToChunky98 "splitAt" n) x
      in  (z, if b then d else Chunky98.zero)
   {-# INLINE reverse #-}
   reverse = Chunky98.fromChunks . List.reverse . Chunky98.toChunks


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
-}
