{-# LANGUAGE NoImplicitPrelude #-}
{- |
<http://en.wikipedia.org/wiki/Particle_displacement>
-}
module Synthesizer.Generic.Displacement where

import qualified Synthesizer.Generic.Signal as SigG

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


-- * Mixing

{-| Mix two signals.
    In opposition to 'zipWith' the result has the length of the longer signal. -}
mix :: (Additive.C v, SigG.Transform sig v) =>
   sig v -> sig v -> sig v
mix = SigG.mix

{- relict from Prelude98's Num
mixMono :: Ring.C a => [a] -> [a] -> [a]
mixMono [] x  = x
mixMono x  [] = x
mixMono (x:xs) (y:ys) = x+y : mixMono xs ys
-}

{-| Mix one or more signals. -}
mixMulti :: (Additive.C v, SigG.Transform sig v) =>
   [sig v] -> sig v
mixMulti = foldl mix SigG.empty


{-| Add a number to all of the signal values.
    This is useful for adjusting the center of a modulation. -}
raise :: (Additive.C v, SigG.Transform sig v) =>
   v -> sig v -> sig v
raise x = SigG.map ((+) x)


-- * Distortion

{- |
In "Synthesizer.Basic.Distortion" you find a collection
of appropriate distortion functions.
-}
distort :: (SigG.Read sig c, SigG.Transform sig v) =>
   (c -> v -> v) -> sig c -> sig v -> sig v
distort = SigG.zipWith


-- * Preprocessing of control curves

{-# INLINE mapLinear #-}
mapLinear :: (Ring.C a, SigG.Transform sig a) =>
   a ->
   a ->
   sig a ->
   sig a
mapLinear depth center =
   SigG.map (\x -> center*(one+x*depth))

{-# INLINE mapExponential #-}
mapExponential :: (Trans.C a, SigG.Transform sig a) =>
   a ->
   a ->
   sig a ->
   sig a
mapExponential depth center =
   -- SigG.map ((center*) . (depth**))
   -- should be faster
   let logDepth = log depth
   in  SigG.map ((center*) . exp . (logDepth*))
