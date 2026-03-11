{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.LorenzAttractor where

import qualified Algebra.Module as Module
import qualified Algebra.Ring   as Ring

import NumericPrelude.Numeric
import NumericPrelude.Base


computeDerivatives :: (Ring.C y) =>
   (y, y, y) -> (y, y, y) -> (y, y, y)
computeDerivatives (a,b,c) (x,y,z) =
   let x' = a*(y-x)
       y' = x*(b-z) - y
       z' = x*y -c*z
   in  (x',y',z')

explicitEuler :: (Module.C a v) =>
   a -> (v -> v) -> v -> [v]
explicitEuler h phi s =
   let ys = s : map (\y -> y + h *> phi y) ys
   in  ys


equilibrium :: (Double, Double, Double)
equilibrium = (sqrt 72, sqrt 72, 27.001)

example0 :: [(Double, Double, Double)]
example0 =
   explicitEuler (0.01::Double)
      (computeDerivatives (10, 28, 8/3)) equilibrium

example :: [(Double, Double, Double)]
example =
   explicitEuler (0.01::Double)
      (computeDerivatives (10, 28, 8/3)) (8.5, 8.6, 27)
