{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes


Control curves which can be used
as envelopes, for controlling filter parameters and so on.
-}
module Synthesizer.Dimensional.Amplitude.Control (
   -- * Primitives
   constant, constantVector,

   -- * Piecewise constant
   piecewiseConstantGeneric,
   piecewiseConstantStorable,
   ) where

import qualified Synthesizer.Dimensional.Signal.Private as SigA

import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.PiecewiseConstant.Generic as PCG
import qualified Synthesizer.PiecewiseConstant.Storable as PCSt

import qualified Synthesizer.State.Control as Ctrl

import qualified Synthesizer.Generic.Signal as SigG

import qualified Synthesizer.Storable.Signal as SigSt
import Foreign.Storable (Storable)

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

-- import qualified Algebra.Module             as Module
import qualified Algebra.Absolute               as Absolute
-- import qualified Algebra.Ring               as Ring
-- import qualified Algebra.Additive           as Additive

-- import NumericPrelude.Numeric
import NumericPrelude.Base as P
import Prelude ()


{-# INLINE constant #-}
constant :: (Absolute.C y, Dim.C u) =>
      DN.T u y {-^ value -}
   -> SigA.R s u y y
constant =
   uncurry constantVector .
   DN.absSignum

{- |
The amplitude must be positive!
This is not checked.
-}
{-# INLINE constantVector #-}
constantVector :: -- (Field.C y', Absolute.C y', OccScalar.C y y') =>
      DN.T u y {-^ amplitude -}
   -> yv       {-^ value -}
   -> SigA.R s u y yv
constantVector y yv =
   SigA.fromBody y (Ctrl.constant yv)


{-# INLINE piecewiseConstantGeneric #-}
piecewiseConstantGeneric ::
   (SigG.Write sig y) =>
   SigA.T rate amp (PC.T y) ->
   SigA.T rate amp (sig y)
piecewiseConstantGeneric =
   SigA.processBody PCG.toSignal

{-# INLINE piecewiseConstantStorable #-}
piecewiseConstantStorable ::
   (Storable y) =>
   SigA.T rate amp (PC.T y) ->
   SigA.T rate amp (SigSt.T y)
piecewiseConstantStorable =
   SigA.processBody PCSt.toSignal
