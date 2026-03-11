{-# LANGUAGE NoImplicitPrelude #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Filter operators from calculus
-}
module Synthesizer.State.Filter.Recursive.Integration where

import qualified Synthesizer.State.Signal  as Sig
import qualified Synthesizer.Causal.Process as Causal

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
run =
   Sig.crochetL (\x acc -> let y = x+acc in Just (y,y)) zero
   -- scanl1 (+)

{- |
Integrate with initial condition.
First emitted value is the initial condition.
The signal become one element longer.
-}
{-# INLINE runInit #-}
runInit :: Additive.C v => v -> Sig.T v -> Sig.T v
runInit = Sig.scanL (+)


{-# INLINE causal #-}
causal :: Additive.C v => Causal.T v v
causal = Causal.scanL1 (+)

{- |
Integrate with initial condition.
First emitted value is the initial condition.
The signal become one element longer.
-}
{-# INLINE causalInit #-}
causalInit :: Additive.C v => v -> Causal.T v v
causalInit = Causal.scanL (+)

{- other quadrature methods may follow -}
