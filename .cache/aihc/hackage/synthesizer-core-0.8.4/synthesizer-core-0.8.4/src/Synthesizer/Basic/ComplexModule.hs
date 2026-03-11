{-
Maybe this module should be moved to NumericPrelude.
-}
module Synthesizer.Basic.ComplexModule where

import qualified Number.Complex as Complex
import qualified Algebra.Module as Module
import Number.Complex ((+:), )

import NumericPrelude.Numeric
import Prelude ()


{-# INLINE scale #-}
scale :: (Module.C a v) =>
   Complex.T a -> v -> Complex.T v
scale s x =
   Complex.real s *> x  +:  Complex.imag s *> x

{-# INLINE mul #-}
mul :: (Module.C a v) =>
   Complex.T a -> Complex.T v -> Complex.T v
mul x y =
   (Complex.real x *> Complex.real y - Complex.imag x *> Complex.imag y)
   +:
   (Complex.real x *> Complex.imag y + Complex.imag x *> Complex.real y)

{-# INLINE project #-}
project :: (Module.C a v) =>
   Complex.T a -> Complex.T v -> v
project x y =
   Complex.real x *> Complex.real y - Complex.imag x *> Complex.imag y
