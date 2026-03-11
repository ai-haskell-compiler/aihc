{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.ChunkySize.Cut (
   splitAt, take, drop,
   ) where

-- import qualified Synthesizer.Dimensional.Process as Proc
import qualified Synthesizer.Dimensional.Rate as Rate
import qualified Synthesizer.Dimensional.Amplitude as Amp
import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.ChunkySize as ChunkySize
import qualified Synthesizer.ChunkySize.Cut as CutC

-- import qualified Number.DimensionTerm        as DN
-- import qualified Algebra.DimensionTerm       as Dim

-- import qualified Number.NonNegative     as NonNeg

-- import qualified Algebra.RealRing      as RealRing
-- import qualified Algebra.Field          as Field


-- import NumericPrelude.Numeric hiding (negate)
-- import NumericPrelude.Base as P
import Prelude hiding (splitAt, take, drop, length, )


type Signal s amp sig =
   SigA.T (Rate.Phantom s) amp sig

type Size s =
   SigA.T (Rate.Phantom s) Amp.Abstract ChunkySize.T

{- |
To avoid recomputation,
don't use this directly on State signals
but only after buffering.
-}
{-# INLINE splitAt #-}
splitAt :: (CutC.Transform sig) =>
   Size s ->
   Signal s amp sig ->
   (Signal s amp sig, Signal s amp sig)
splitAt =
   \t x ->
      let (y,z) = CutC.splitAt (SigA.body t) $ SigA.body x
      in  (SigA.replaceBody y x,
           SigA.replaceBody z x)

{-# INLINE take #-}
take :: (CutC.Transform sig) =>
   Size s ->
   Signal s amp sig ->
   Signal s amp sig
take =
   \t -> SigA.processBody (CutC.take (SigA.body t))

{-# INLINE drop #-}
drop :: (CutC.Transform sig) =>
   Size s ->
   Signal s amp sig ->
   Signal s amp sig
drop =
   \t -> SigA.processBody (CutC.drop (SigA.body t))
