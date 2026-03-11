{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

All recursive filters with real coefficients
can be decomposed into first order and second order filters with real coefficients.
This follows from the Fundamental theorem of algebra.

This implements a cascade of second order filters
using StorableVectors for state and filter parameters.
-}
module Synthesizer.Plain.Filter.Recursive.SecondOrderCascade (
   Parameter (Parameter),
   State,
   step,
   modifierInit,
   modifier,
   causal,
   ) where

import qualified Synthesizer.Plain.Filter.Recursive.SecondOrder as Filt2
import qualified Synthesizer.Plain.Signal   as Sig
import qualified Synthesizer.Plain.Modifier as Modifier
import qualified Synthesizer.Interpolation.Class as Interpol

import qualified Synthesizer.Causal.Process as Causal

import qualified Algebra.Module                as Module
import qualified Algebra.Ring                  as Ring

import qualified Control.Monad.Trans.State as MS

import qualified Data.StorableVector as SV
import Foreign.Storable (Storable(..))

import NumericPrelude.Numeric
import NumericPrelude.Base


{-
Maybe there is no need to make the parameter vector
a StorableVector or an Array.
We could also make Paramter a State.Signal,
which reads from a StorableVector or Array buffer.
This way we would not need to create many StorableVectors
when interpolating filter parameters.
-}
newtype Parameter a =
   Parameter (SV.Vector (Filt2.Parameter a))

{-
If Causal.Process would support ST operations,
then we could use a writeable storable vector for the status.
This would save us many allocations.
-}
type State a =
   SV.Vector (Filt2.State a)


{-# INLINE checkSizes #-}
checkSizes :: String -> SV.Vector a -> SV.Vector b -> c -> c
checkSizes opName x y act =
   if SV.length x == SV.length y
     then act
     else error $ opName ++ ": incompatible sizes of cascades of second order filters"

{-# INLINE withSizeCheck #-}
withSizeCheck ::
   String ->
   (SV.Vector a -> SV.Vector b -> c) ->
   (SV.Vector a -> SV.Vector b -> c)
withSizeCheck opName f x y =
   checkSizes opName x y (f x y)


instance (Interpol.C a v, Storable v) => Interpol.C a (Parameter v) where
   {-# INLINE scaleAndAccumulate #-}
   scaleAndAccumulate (a, Parameter x) =
      (Parameter $ SV.map (curry Interpol.scale a) x,
       \ (Parameter y) ->
          Parameter $ withSizeCheck "mac"
             (SV.zipWith (curry Interpol.scaleAccumulate a)) x y)


{-# INLINE step #-}
step ::
   (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Parameter a -> v -> MS.State (State v) v
step (Parameter p) =
   Modifier.stackStatesStorableVaryL Filt2.step p

{-# INLINE modifierInit #-}
modifierInit ::
   (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Modifier.Initialized (State v) (State v) (Parameter a) v v
modifierInit =
   Modifier.Initialized id step


{-# INLINE modifier #-}
modifier ::
   (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Modifier.Simple (State v) (Parameter a) v v
modifier order =
   Sig.modifierInitialize modifierInit
      (SV.replicate order Filt2.zeroState)

{-# INLINE causal #-}
causal :: (Ring.C a, Module.C a v, Storable a, Storable v) =>
   Int ->
   Causal.T (Parameter a, v) v
causal order =
   Causal.fromSimpleModifier (modifier order)

