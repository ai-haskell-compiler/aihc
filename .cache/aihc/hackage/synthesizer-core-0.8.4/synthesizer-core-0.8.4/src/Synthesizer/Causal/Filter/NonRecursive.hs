{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Causal.Filter.NonRecursive where

import qualified Synthesizer.Causal.Process as Causal
import Control.Arrow ((>>>), )

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Basic.Filter.NonRecursive as Filt
import qualified Synthesizer.State.Control as CtrlS
import qualified Synthesizer.State.Signal as SigS
import Synthesizer.Utility (affineComb, )

import qualified Algebra.Module         as Module
import qualified Algebra.Field          as Field
import qualified Algebra.Ring           as Ring
import qualified Algebra.Additive       as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base as NP


{-# INLINE amplify #-}
amplify :: (Ring.C a) => a -> Causal.T a a
amplify v = Causal.map (v*)

{-# INLINE amplifyVector #-}
amplifyVector :: (Module.C a v) => a -> Causal.T v v
amplifyVector v = Causal.map (v*>)


{-# INLINE envelope #-}
envelope :: (Ring.C a) =>
   Causal.T (a,a) a
envelope = Causal.map (uncurry (*))

{-# INLINE envelopeVector #-}
envelopeVector :: (Module.C a v) =>
   Causal.T (a,v) v
envelopeVector = Causal.map (uncurry (*>))


{-# INLINE crossfade #-}
crossfade :: (Field.C a, Module.C a a) => Int -> Causal.T (a,a) a
crossfade len =
   let affineCombMono :: (Module.C a a) => a -> (a,a) -> a
       affineCombMono = affineComb
   in  Causal.applyFst
          (Causal.map (uncurry affineCombMono))
          (CtrlS.line len (0, 1))


{-# INLINE accumulatePosModulatedFromPyramid #-}
accumulatePosModulatedFromPyramid ::
   (SigG.Transform sig v) =>
   ([sig v] -> (Int,Int) -> v) ->
   [sig v] -> Causal.T (Int,Int) v
accumulatePosModulatedFromPyramid summer pyr0 =
   let sizes = Filt.unitSizesFromPyramid pyr0
       pyrStarts =
          SigS.iterate (zipWith SigG.drop sizes) pyr0
       offsets =
          SigS.take (head sizes) (SigS.iterate (1+) 0)
   in  Causal.feedFst (SigS.liftA2 (,) pyrStarts offsets) >>>
       Causal.map (\((pyr,offset), (lo,hi)) ->
          summer pyr (offset+lo, offset+hi))

{-# INLINE sumsPosModulatedFromPyramid #-}
sumsPosModulatedFromPyramid ::
   (Additive.C v, SigG.Transform sig v) =>
   [sig v] -> Causal.T (Int,Int) v
sumsPosModulatedFromPyramid =
   accumulatePosModulatedFromPyramid FiltG.sumRangeFromPyramid
