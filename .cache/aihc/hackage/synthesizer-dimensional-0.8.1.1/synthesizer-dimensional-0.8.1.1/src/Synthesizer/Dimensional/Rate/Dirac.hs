{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.Dimensional.Rate.Dirac where

import qualified Synthesizer.Generic.Cut as Cut

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Dimensional.Process as Proc

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Rate as Rate

import qualified Data.Monoid as Mn
import qualified Data.Semigroup as Sg

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import qualified Algebra.Field              as Field
import qualified Algebra.Ring               as Ring

import Data.Tuple.HT (mapPair, mapSnd, )

import NumericPrelude.Numeric (zero, one, )


{- |
We want to represent streams of discrete events
in a manner that is more safe than plain @[Bool]@.
Each peak can be imagined as a Dirac impulse.

A @[Bool]@ could be used accidentally for 'Synthesizer.Dimensional.Amplitude.Cut.selectBool',
where @selectBool@ is intended for piecewise constant control curves.

You may think that a type like @Peak = Peak Bool@ as sample type
in @T s Peak@ would also do the job.
Actually, this wouldn't be a good idea
since you can apply constant interpolation on it,
which obviously fools the idea of a peak.

This type is so level that it could be moved to Synthesizer.Generic.Dirac.
-}
newtype T s sig = Cons {decons :: sig Bool}

instance Sg.Semigroup (sig Bool) => Sg.Semigroup (T s sig) where
   Cons x <> Cons y = Cons (x Sg.<> y)

instance Mn.Monoid (sig Bool) => Mn.Monoid (T s sig) where
   mempty = Cons Mn.mempty
   mappend (Cons x) (Cons y) = Cons (Mn.mappend x y)

instance Cut.Read (sig Bool) => Cut.Read (T s sig) where
   {-# INLINE null #-}
   null = Cut.null . decons
   {-# INLINE length #-}
   length = Cut.length . decons

instance Cut.Transform (sig Bool) => Cut.Transform (T s sig) where
   {-# INLINE take #-}
   take n = Cons . Cut.take n . decons
   {-# INLINE drop #-}
   drop n = Cons . Cut.drop n . decons
   {-# INLINE splitAt #-}
   splitAt n = mapPair (Cons, Cons) . Cut.splitAt n . decons
   {-# INLINE dropMarginRem #-}
   dropMarginRem n m = mapSnd Cons . Cut.dropMarginRem n m . decons
   {-# INLINE reverse #-}
   reverse = Cons . Cut.reverse . decons

{- |
This is the most frequently needed transformation
of a stream of peaks, if not the only one.
It converts to a signal of peaks with area 1.
This convention is especially useful for smoothing filters
that produce frequency progress curves from zero crossings.
-}
{-# INLINE toAmplitudeSignal #-}
toAmplitudeSignal ::
   (Ring.C q, Dim.C u, Functor sig) =>
   Proc.T s u q
      (T s sig ->
       SigA.T (Rate.Phantom s) (Amp.Numeric (DN.T (Dim.Recip u) q)) (sig q))
toAmplitudeSignal =
   flip fmap Proc.getSampleRate $ \rate ->
      SigA.Cons Rate.Phantom (Amp.Numeric rate) .
      fmap (\c -> if c then one else zero) .
      decons
