{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Filter operators from calculus
-}
module Synthesizer.Plain.Filter.Recursive.Integration where

import qualified Synthesizer.Plain.Signal   as Sig

import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Integrate with initial value zero.
However the first emitted value is the value of the input signal.
It maintains the length of the signal.
-}
{-# INLINE run #-}
run :: Additive.C v => Sig.T v -> Sig.T v
run = scanl1 (+)

{- |
Integrate with initial condition.
First emitted value is the initial condition.
The signal becomes one element longer.
-}
{-# INLINE runInit #-}
runInit :: Additive.C v => v -> Sig.T v -> Sig.T v
runInit = scanl (+)

{- other quadrature methods may follow -}
