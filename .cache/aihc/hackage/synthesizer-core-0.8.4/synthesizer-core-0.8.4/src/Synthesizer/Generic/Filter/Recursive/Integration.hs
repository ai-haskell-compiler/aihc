{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Filter operators from calculus
-}
module Synthesizer.Generic.Filter.Recursive.Integration where

import qualified Synthesizer.Generic.Signal as SigG

import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Integrate with initial value zero.
However the first emitted value is the value of the input signal.
It maintains the length of the signal.
-}
{-# INLINE run #-}
run :: (Additive.C v, SigG.Transform sig v) =>
   sig v -> sig v
run =
   SigG.crochetL (\x acc -> let y = x+acc in Just (y,y)) zero
   -- scanl1 (+)

{- |
Integrate with initial condition.
First emitted value is the initial condition.
The signal become one element longer.
-}
{-# INLINE runInit #-}
runInit :: (Additive.C v, SigG.Transform sig v) =>
   v -> sig v -> sig v
runInit = SigG.scanL (+)

{- other quadrature methods may follow -}
