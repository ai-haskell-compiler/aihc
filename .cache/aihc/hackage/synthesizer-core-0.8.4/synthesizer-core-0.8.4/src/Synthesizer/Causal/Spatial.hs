{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Causal.Spatial where

import Control.Arrow (Arrow, arr, )

import qualified Algebra.NormedSpace.Euclidean as Euc
import qualified Algebra.Field                 as Field

import NumericPrelude.Numeric
import NumericPrelude.Base


{-|
simulate an moving sounding object

convert the way of the object through 2D or 3D space
into a delay and attenuation information,
sonicDelay is the reciprocal of the sonic velocity
-}
moveAround ::
   (Field.C a, Euc.C a v, Arrow arrow) =>
   a -> a -> v -> arrow v (a,a)
moveAround att sonicDelay ear =
   arr ((\dist -> (sonicDelay*dist, 1/(att+dist)^2)) . Euc.norm . subtract ear)
