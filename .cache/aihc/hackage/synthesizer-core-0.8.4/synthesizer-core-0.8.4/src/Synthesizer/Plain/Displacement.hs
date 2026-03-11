{-# LANGUAGE NoImplicitPrelude #-}
{- |
<http://en.wikipedia.org/wiki/Particle_displacement>
-}
module Synthesizer.Plain.Displacement where

import qualified Algebra.Additive              as Additive

import qualified Synthesizer.Plain.Signal as Sig

import NumericPrelude.Numeric
import NumericPrelude.Base


{- * Mixing -}

{-| Mix two signals.
    In opposition to 'zipWith' the result has the length of the longer signal. -}
mix :: (Additive.C v) => Sig.T v -> Sig.T v -> Sig.T v
mix = (+)

{- relict from Prelude98's Num
mixMono :: Ring.C a => [a] -> [a] -> [a]
mixMono [] x  = x
mixMono x  [] = x
mixMono (x:xs) (y:ys) = x+y : mixMono xs ys
-}

{-| Mix an arbitrary number of signals. -}
mixMulti :: (Additive.C v) => [Sig.T v] -> Sig.T v
mixMulti = foldl mix zero


{-| Add a number to all of the signal values.
    This is useful for adjusting the center of a modulation. -}
raise :: (Additive.C v) => v -> Sig.T v -> Sig.T v
raise x = map ((+) x)


{- * Distortion -}
{- |
In "Synthesizer.Basic.Distortion" you find a collection
of appropriate distortion functions.
-}
distort :: (c -> a -> a) -> Sig.T c -> Sig.T a -> Sig.T a
distort = zipWith
