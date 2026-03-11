{- |
Copyright   :  (c) Henning Thielemann 2008-2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Dimensional.Amplitude.Displacement (
   mix, mixVolume,
   mixMulti, mixMultiVolume,
   raise, raiseVector, distort,
   map, mapLinear, mapExponential, mapLinearDimension,
   inflateGeneric, inflate,
   ) where

import qualified Synthesizer.Dimensional.Signal.Private as SigA
import Synthesizer.Dimensional.Signal.Private (toAmplitudeScalar)

import qualified Synthesizer.Dimensional.Amplitude as Amp

import qualified Synthesizer.Dimensional.Amplitude.Flat as Flat

import qualified Number.DimensionTerm        as DN
import qualified Algebra.DimensionTerm       as Dim

import Number.DimensionTerm ((&*&))

import qualified Synthesizer.Generic.Signal  as SigG

import qualified Synthesizer.State.Displacement as Disp
import qualified Synthesizer.State.Signal  as Sig

import qualified Algebra.Transcendental as Trans
import qualified Algebra.Module         as Module
import qualified Algebra.Field          as Field
import qualified Algebra.Absolute       as Absolute
import qualified Algebra.Ring           as Ring

import qualified Data.List as List

import NumericPrelude.Base hiding (map, )
import NumericPrelude.Numeric
import Prelude ()


{- * Mixing -}

{- |
Mix two signals.
In contrast to 'zipWith' the result has the length of the longer signal.
-}
{-# INLINE mix #-}
mix ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C u) =>
      SigA.R s u y yv
   -> SigA.R s u y yv
   -> SigA.R s u y yv
mix x y =
   mixVolume
      (DN.abs (SigA.actualAmplitude x) + DN.abs (SigA.actualAmplitude y))
      x y

{-# INLINE mixVolume #-}
mixVolume ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C u) =>
      DN.T u y
   -> SigA.R s u y yv
   -> SigA.R s u y yv
   -> SigA.R s u y yv
mixVolume v x y =
   let z = SigA.fromBody v
              (SigA.vectorSamples (toAmplitudeScalar z) x +
               SigA.vectorSamples (toAmplitudeScalar z) y)
   in  z

{- |
Mix one or more signals.
-}
{-# INLINE mixMulti #-}
mixMulti ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C u) =>
      [SigA.R s u y yv]
   ->  SigA.R s u y yv
mixMulti x =
   mixMultiVolume (sum (List.map (DN.abs . SigA.actualAmplitude) x)) x

{-# INLINE mixMultiVolume #-}
mixMultiVolume ::
   (Absolute.C y, Field.C y, Module.C y yv, Dim.C u) =>
      DN.T u y
   -> [SigA.R s u y yv]
   ->  SigA.R s u y yv
mixMultiVolume v x =
   let z = SigA.fromBody v
              (foldr (\y -> (SigA.vectorSamples (toAmplitudeScalar z) y +)) Sig.empty x)
   in  z

{- |
Add a number to all of the signal values.
This is useful for adjusting the center of a modulation.
-}
{-# INLINE raise #-}
raise :: (Field.C y, Dim.C u) =>
      DN.T u y
   -> SigA.T rate (Amp.Dimensional u y) (Sig.T y)
   -> SigA.T rate (Amp.Dimensional u y) (Sig.T y)
raise y' x =
   SigA.processBody
      (Disp.raise (toAmplitudeScalar x y')) x

{-# INLINE raiseVector #-}
raiseVector :: (Field.C y, Module.C y yv, Dim.C u) =>
      DN.T u y
   -> yv
   -> SigA.T rate (Amp.Dimensional u y) (Sig.T yv)
   -> SigA.T rate (Amp.Dimensional u y) (Sig.T yv)
raiseVector y' yv x =
   SigA.processBody
      (Disp.raise (toAmplitudeScalar x y' *> yv)) x

{- |
Distort the signal using a flat function.
The first signal gives the scaling of the function.
If the scaling is c and the input sample is y,
then @c * f(y/c)@ is output.
This way we can use an (efficient) flat function
and have a simple, yet dimension conform, way of controlling the distortion.
E.g. if the distortion function is @tanh@
then the value @c@ controls the saturation level.
-}
{-# INLINE distort #-}
distort :: (Field.C y, Module.C y yv, Dim.C u) =>
      (yv -> yv)
   -> SigA.R s u y y
   -> SigA.R s u y yv
   -> SigA.R s u y yv
distort f cs xs =
   SigA.processBody
      (Sig.zipWith
          (\c y -> c *> f (recip c *> y))
          (SigA.scalarSamples (toAmplitudeScalar xs) cs)) xs



{-# INLINE map #-}
map ::
   (Amp.Primitive amp) =>
   (y0 -> y1) ->
   SigA.T rate amp (Sig.T y0) ->
   SigA.T rate amp (Sig.T y1)
map f =
   SigA.processBody (Sig.map f)


{-
This signature is too general.
It will cause strange type errors
if u is Scalar and further process want to use the Flat instance.
The Flat instance cannot be found, if q cannot be determined.

mapLinear :: (Flat.C y flat, Ring.C y, Dim.C u) =>
    y ->
    DN.T u q ->
    SigA.T rate flat (Sig.T y) ->
    SigA.T rate (Amp.Dimensional u q) (Sig.T y)
-}

{- |
Map a control curve without amplitude unit
by a linear (affine) function with a unit.
This is a combination of 'raise' and 'amplify'.
-}
{-# INLINE mapLinear #-}
mapLinear :: (Flat.C y flat, Ring.C y, Dim.C u) =>
   y ->
   DN.T u y ->
   SigA.T rate flat (Sig.T y) ->
   SigA.T rate (Amp.Dimensional u y) (Sig.T y)
mapLinear depth center =
   mapAux center (Sig.map (\x -> one+x*depth) . Flat.toSamples)

{-# INLINE mapExponential #-}
mapExponential :: (Flat.C y flat, Trans.C y, Dim.C u) =>
   y ->
   DN.T u q ->
   SigA.T rate flat (Sig.T y) ->
   SigA.T rate (Amp.Dimensional u q) (Sig.T y)
mapExponential depth center =
   -- mapAux center (Sig.map (depth**) . Flat.toSamples)
   -- should be faster
   mapAux center
      (let logDepth = log depth in Sig.map (exp . (logDepth*)) .
       Flat.toSamples)

{-# INLINE mapLinearDimension #-}
mapLinearDimension ::
   (Field.C y, Absolute.C y, Dim.C u, Dim.C v) =>
      DN.T v y               {- ^ range: one is mapped to @center + range * ampX@ -}
   -> DN.T (Dim.Mul v u) y  {- ^ center: zero is mapped to @center@ -}
   -> SigA.T rate (Amp.Dimensional u y) (Sig.T y)
   -> SigA.T rate (Amp.Dimensional (Dim.Mul v u) y) (Sig.T y)
mapLinearDimension range center x =
   let absRange  = DN.abs range &*& SigA.actualAmplitude x
       absCenter = DN.abs center
       rng = toAmplitudeScalar z absRange
       cnt = toAmplitudeScalar z absCenter
       z =
          mapAux (absRange + absCenter)
             (Sig.map (\y -> cnt + rng*y) . SigA.body)
             x
   in  z

mapAux ::
   amp ->
   (SigA.T rate amplitude body0 -> body1) ->
   SigA.T rate amplitude body0 ->
   SigA.T rate (Amp.Numeric amp) body1
mapAux amp f xs =
   SigA.Cons (SigA.sampleRate xs) (Amp.Numeric amp) .
   f $ xs



{- |
I suspect that this function will most oftenly not the right choice.
When the amplitude is Flat, better use 'inflate'.
When the amplitude is Numeric, better use @Filter.amplifyScalarDimension@
since this will not modify signal values
but only the global amplitude.
This is both more efficient and ensures boundedness of signal values.
-}
{-# INLINE inflateGeneric #-}
inflateGeneric ::
   (Flat.C y flat, SigG.Transform sig y) =>
   amp ->
   SigA.T rate flat (sig y) ->
   SigA.T rate (Amp.Numeric amp) (sig y)
inflateGeneric v =
   \x ->
      SigA.Cons (SigA.sampleRate x) (Amp.Numeric v)
         (Flat.toSamples x)

{-# INLINE inflate #-}
inflate ::
   amp ->
   SigA.T rate (Amp.Flat y) sig ->
   SigA.T rate (Amp.Numeric amp) sig
inflate v =
   \x ->
      SigA.Cons (SigA.sampleRate x) (Amp.Numeric v)
         (SigA.body x)
