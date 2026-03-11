{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Rate.Cut (
   splitAt, take, drop,
   concat, append, ) where

import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import qualified Synthesizer.Generic.Cut as CutG

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import qualified Number.NonNegative     as NonNeg

import qualified Algebra.RealRing      as RealRing
-- import qualified Algebra.Field          as Field

import Data.Monoid (Monoid, mappend, mconcat, )


-- import NumericPrelude.Numeric hiding (negate)
-- import NumericPrelude.Base as P
import Prelude hiding (splitAt, take, drop, concat, )


type Signal s amp sig =
   SigA.T (Rate.Phantom s) amp sig

{- |
To avoid recomputation,
don't use this directly on State signals
but only after buffering.
-}
{-# INLINE splitAt #-}
splitAt :: (CutG.Transform sig, RealRing.C t, Dim.C u) =>
   DN.T u t ->
   Proc.T s u t
      (Signal s amp sig ->
       (Signal s amp sig, Signal s amp sig))
splitAt t' =
   flip fmap (Proc.toTimeScalar t') $
   \t x ->
      let (y,z) = CutG.splitAt (RealRing.round t) $ SigA.body x
      in  (SigA.replaceBody y x,
           SigA.replaceBody z x)

{-# INLINE take #-}
take :: (CutG.Transform sig, RealRing.C t, Dim.C u) =>
   DN.T u t ->
   Proc.T s u t
      (Signal s amp sig ->
       Signal s amp sig)
take t' =
   flip fmap (Proc.toTimeScalar t') $
   \t -> SigA.processBody (CutG.take (RealRing.round t))

{-# INLINE drop #-}
drop :: (CutG.Transform sig, RealRing.C t, Dim.C u) =>
   DN.T u t ->
   Proc.T s u t
      (Signal s amp sig ->
       Signal s amp sig)
drop t' =
   flip fmap (Proc.toTimeScalar t') $
   \t -> SigA.processBody (CutG.drop (RealRing.round t))


{-# INLINE concat #-}
concat ::
   (Amp.Primitive amp, Monoid sig, Dim.C u) =>
   Proc.T s u t (
      [Signal s amp sig] ->
      Signal s amp sig)
concat =
   Proc.pure $
   SigA.Cons Rate.Phantom Amp.primitive . mconcat . map SigA.body

{-# INLINE append #-}
append ::
   (Amp.Primitive amp, Monoid sig, Dim.C u) =>
   Proc.T s u t (
      Signal s amp sig ->
      Signal s amp sig ->
      Signal s amp sig)
append =
   Proc.pure $
   \x -> SigA.processBody (mappend (SigA.body x))
