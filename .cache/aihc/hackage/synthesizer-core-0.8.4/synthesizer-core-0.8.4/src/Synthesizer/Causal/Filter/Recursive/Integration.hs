{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2009
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Filter operators from calculus
-}
module Synthesizer.Causal.Filter.Recursive.Integration where

import qualified Synthesizer.Causal.Process as Causal
import qualified Control.Monad.Trans.State as State

import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base



{- |
Integrate with initial value zero.
However the first emitted value is the value of the input signal.
It maintains the length of the signal.
-}
{-# INLINE run #-}
run :: Additive.C v => Causal.T v v
run = Causal.fromState (\x -> State.modify (x+) >> State.get) zero

{- |
Integrate with initial condition.
First emitted value is the initial condition.
The signal becomes one element longer.
-}
{-# INLINE runInit #-}
runInit :: Additive.C v => v -> Causal.T v v
runInit = Causal.fromState (\x -> State.state (\s -> (s, s+x)))

{- other quadrature methods may follow -}
