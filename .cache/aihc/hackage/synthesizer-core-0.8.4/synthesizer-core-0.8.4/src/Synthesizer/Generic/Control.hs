{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Synthesizer.Generic.Control (
   constant,
   linear,
   linearMultiscale,
   linearMultiscaleNeutral,
   line,
   exponential, exponentialMultiscale,
   exponentialMultiscaleNeutral,
   exponential2, exponential2Multiscale,
   exponential2MultiscaleNeutral,
   vectorExponential,
   vectorExponential2,
   cosine, cosineMultiscaleLinear,
   cosineMultiscale,
   Ctrl.cosineWithSlope,
   cubicHermite,
   ) where

import qualified Synthesizer.Plain.Control as Ctrl

import qualified Synthesizer.Generic.Signal as SigG

import qualified Algebra.Module                as Module
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Field                 as Field
import qualified Algebra.Additive              as Additive

import qualified Number.Complex as Complex
import Number.Complex (cis,real)

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Control curve generation -}

constant :: (SigG.Write sig y) =>
   SigG.LazySize -> y -> sig y
constant = SigG.repeat


linear :: (Additive.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y   {-^ steepness -}
   -> y   {-^ initial value -}
   -> sig y
          {-^ linear progression -}
linear size d y0 = SigG.iterate size (d+) y0

{- |
Minimize rounding errors by reducing number of operations per element
to a logarithmuc number.
-}
linearMultiscale ::
   (Additive.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y
   -> y
   -> sig y
linearMultiscale size =
   curveMultiscale size (+)

{- |
Linear curve starting at zero.
-}
linearMultiscaleNeutral :: (Additive.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y
   -> sig y
linearMultiscaleNeutral size slope =
   curveMultiscaleNeutral size (+) slope zero

{- |
Linear curve of a fixed length.
The final value is not actually reached,
instead we stop one step before.
This way we can concatenate several lines
without duplicate adjacent values.
-}
line :: (Field.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> Int   {-^ length -}
   -> (y,y) {-^ initial and final value -}
   -> sig y
            {-^ linear progression -}
line size n (y0,y1) =
   SigG.take n $ linear size ((y1-y0) / fromIntegral n) y0


exponential, exponentialMultiscale ::
   (Trans.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y   {-^ time where the function reaches 1\/e of the initial value -}
   -> y   {-^ initial value -}
   -> sig y
          {-^ exponential decay -}
exponential size time =
   SigG.iterate size (* exp (- recip time))
exponentialMultiscale size time =
   curveMultiscale size (*) (exp (- recip time))

exponentialMultiscaleNeutral :: (Trans.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y   {-^ time where the function reaches 1\/e of the initial value -}
   -> sig y
          {-^ exponential decay -}
exponentialMultiscaleNeutral size time =
   curveMultiscaleNeutral size (*) (exp (- recip time)) one

exponential2, exponential2Multiscale :: (Trans.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y   {-^ half life -}
   -> y   {-^ initial value -}
   -> sig y
          {-^ exponential decay -}
exponential2 size halfLife =
   SigG.iterate size (*  0.5 ** recip halfLife)
exponential2Multiscale size halfLife =
   curveMultiscale size (*) (0.5 ** recip halfLife)

exponential2MultiscaleNeutral :: (Trans.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> y   {-^ half life -}
   -> sig y
          {-^ exponential decay -}
exponential2MultiscaleNeutral size halfLife =
   curveMultiscaleNeutral size (*) (0.5 ** recip halfLife) one




{-| This is an extension of 'exponential' to vectors
    which is straight-forward but requires more explicit signatures.
    But since it is needed rarely I setup a separate function. -}
vectorExponential ::
   (Trans.C y, Module.C y v, SigG.Write sig v) =>
      SigG.LazySize
   ->  y  {-^ time where the function reaches 1\/e of the initial value -}
   ->  v  {-^ initial value -}
   -> sig v
          {-^ exponential decay -}
vectorExponential size time y0 =
   SigG.iterate size (exp (-1/time) *>) y0

vectorExponential2 ::
   (Trans.C y, Module.C y v, SigG.Write sig v) =>
      SigG.LazySize
   ->  y  {-^ half life -}
   ->  v  {-^ initial value -}
   -> sig v
          {-^ exponential decay -}
vectorExponential2 size halfLife y0 =
   SigG.iterate size (0.5**(1/halfLife) *>) y0



cosine, cosineMultiscaleLinear :: (Trans.C y, SigG.Write sig y) =>
      SigG.LazySize
   ->  y  {-^ time t0 where  1 is approached -}
   ->  y  {-^ time t1 where -1 is approached -}
   -> sig y
          {-^ a cosine wave where one half wave is between t0 and t1 -}
cosine size = Ctrl.cosineWithSlope $
   \d x -> SigG.map cos (linear size d x)

cosineMultiscaleLinear size = Ctrl.cosineWithSlope $
   \d x -> SigG.map cos (linearMultiscale size d x)

cosineMultiscale ::
   (Trans.C y, SigG.Write sig (Complex.T y),
    SigG.Transform sig (Complex.T y), SigG.Transform sig y) =>
      SigG.LazySize
   ->  y  {-^ time t0 where  1 is approached -}
   ->  y  {-^ time t1 where -1 is approached -}
   -> sig y
          {-^ a cosine wave where one half wave is between t0 and t1 -}
cosineMultiscale size = Ctrl.cosineWithSlope $
   \d x -> SigG.map real (curveMultiscale size (*) (cis d) (cis x))


cubicHermite :: (Field.C y, SigG.Write sig y) =>
      SigG.LazySize
   -> (y, (y,y)) -> (y, (y,y)) -> sig y
cubicHermite size node0 node1 =
   SigG.map (Ctrl.cubicFunc node0 node1) $ linear size 1 0


{- * Auxiliary functions -}


curveMultiscale :: (SigG.Write sig y) =>
   SigG.LazySize -> (y -> y -> y) -> y -> y -> sig y
curveMultiscale size op d y0 =
   SigG.cons y0 . SigG.map (op y0) $ SigG.iterateAssociative size op d


curveMultiscaleNeutral :: (SigG.Write sig y) =>
   SigG.LazySize -> (y -> y -> y) -> y -> y -> sig y
curveMultiscaleNeutral size op d neutral =
   SigG.cons neutral $ SigG.iterateAssociative size op d
