{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Causal.Displacement where

import qualified Control.Arrow as A

import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base


-- * Mixing

{-|
Mix two signals.
Unfortunately we have to use 'zipWith' semantic here,
that is the result is as long as the shorter of both inputs.
-}
{-# INLINE mix #-}
mix :: (Additive.C v, A.Arrow arrow) => arrow (v,v) v
mix = A.arr (uncurry (+))


{-|
Add a number to all of the signal values.
This is useful for adjusting the center of a modulation.
-}
{-# INLINE raise #-}
raise :: (Additive.C v, A.Arrow arrow) => v -> arrow v v
raise x = A.arr (x+)


-- * Distortion
{-|
In "Synthesizer.Basic.Distortion" you find a collection
of appropriate distortion functions.
-}
{-# INLINE distort #-}
distort :: (A.Arrow arrow) => (c -> a -> a) -> arrow (c,a) a
distort f = A.arr (uncurry f)

-- * Preprocessing of control curves

{-# INLINE mapLinear #-}
mapLinear :: (Ring.C a, A.Arrow arrow) =>
   a ->
   a ->
   arrow a a
mapLinear depth center =
   A.arr (\x -> center*(one+x*depth))

{-# INLINE mapExponential #-}
mapExponential :: (Trans.C a, A.Arrow arrow) =>
   a ->
   a ->
   arrow a a
mapExponential depth center =
   -- Sig.map ((center*) . (depth**))
   -- should be faster
   let logDepth = log depth
   in  A.arr ((center*) . exp . (logDepth*))
