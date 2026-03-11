{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.State.Displacement where

import qualified Synthesizer.State.Signal as Sig

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


-- * Mixing

{-|
Mix two signals.
In opposition to 'zipWith' the result has the length of the longer signal.
-}
{-# INLINE mix #-}
mix :: (Additive.C v) => Sig.T v -> Sig.T v -> Sig.T v
mix = Sig.mix

{-| Mix an arbitrary number of signals. -}
{-# INLINE mixMulti #-}
mixMulti :: (Additive.C v) => [Sig.T v] -> Sig.T v
mixMulti = foldl mix Sig.empty


{-|
Add a number to all of the signal values.
This is useful for adjusting the center of a modulation.
-}
{-# INLINE raise #-}
raise :: (Additive.C v) => v -> Sig.T v -> Sig.T v
raise x = Sig.map ((+) x)


-- * Distortion

{-|
In "Synthesizer.Basic.Distortion" you find a collection
of appropriate distortion functions.
-}
{-# INLINE distort #-}
distort :: (c -> a -> a) -> Sig.T c -> Sig.T a -> Sig.T a
distort = Sig.zipWith


-- * Preprocessing of control curves

{-# INLINE mapLinear #-}
mapLinear :: (Ring.C a) =>
   a ->
   a ->
   Sig.T a ->
   Sig.T a
mapLinear depth center =
   Sig.map (\x -> center*(one+x*depth))

{-# INLINE mapExponential #-}
mapExponential :: (Trans.C a) =>
   a ->
   a ->
   Sig.T a ->
   Sig.T a
mapExponential depth center =
   -- Sig.map ((center*) . (depth**))
   -- should be faster
   let logDepth = log depth
   in  Sig.map ((center*) . exp . (logDepth*))
