{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

A class that allows unified handling of
@Amplitude.Flat@ and @Amplitude.Dimensional Dim.Scalar@
which is often used for control curves.
However, I'm thinking about whether this is more abuse than use.
So this class may disappear in future.
Amplitude.Flat might become a synonym for @DN.scalar one@.
Sometimes, using Flat instead of DN.Scalar has the advantage
of internally saving a multiplication with one,
but I think the compiler should optimize that away.
The optimization however is more complicated
if a whole StorableVector is multiplied element-wise by one.
E.g. the concatenation of flat (storable) signals
can be done without copying the entire data.
-}
module Synthesizer.Dimensional.Amplitude.Flat
   (C, amplifySample, canonicalize, toSamples, ) where

import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
import qualified Synthesizer.Generic.Signal as SigG

-- import qualified Synthesizer.State.Signal as Sig

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

{-
import qualified Algebra.Module         as Module
import qualified Algebra.Field          as Field
-}
import qualified Algebra.Ring           as Ring

-- import Number.DimensionTerm ((&/&))


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{-
we could use OccasionallyScalar class,
but this would flood user code with OccScalar.C y y constraints
-}
class Amp.C amp => C y amp | amp -> y where
   toScalar :: amp -> y
   amplifySample :: amp -> y -> y
   amplify :: (SigG.Transform sig y) =>
      amp -> sig y -> sig y

instance Ring.C y => C y (Amp.Flat y) where
   toScalar = const Ring.one
   amplifySample _ = id
   amplify _ = id

instance (Dim.IsScalar v, Ring.C y) => C y (Amp.Numeric (DN.T v y)) where
   toScalar (Amp.Numeric amp) =
      DN.toNumber .
      DN.rewriteDimension Dim.toScalar $
      amp
   amplifySample amp y = toScalar amp * y
   amplify amp = FiltG.amplify (toScalar amp)


{- DEPRECATED toSamples "this function drops the sample rate, better use canonicalize" -}
{-# INLINE toSamples #-}
toSamples ::
   (C y flat, SigG.Transform sig y) =>
   SigA.T rate flat (sig y) -> sig y
toSamples sig =
   amplify (SigA.amplitude sig) (SigA.body sig)

{-# INLINE canonicalize #-}
canonicalize ::
   (C y flat, SigG.Transform sig y) =>
   SigA.T rate flat (sig y) -> SigA.T rate (Amp.Flat y) (sig y)
canonicalize sig =
   SigA.Cons (SigA.sampleRate sig) Amp.Flat (toSamples sig)
