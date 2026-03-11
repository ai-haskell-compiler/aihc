{-# OPTIONS_HADDOCK show-extensions #-}

-- | Module    :  InfBackprop
-- Copyright   :  (C) 2023 Alexey Tochin
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Alexey Tochin <Alexey.Tochin@gmail.com>
--
-- Automatic differentiation and backpropagation.
-- See 'InfBackprop.Tutorial' for details.
module InfBackprop
  ( -- * Base

    -- ** Types
    Backprop (MkBackprop),
    BackpropFunc,
    -- Manipulations
    call,
    forward,
    backward,
    derivative,
    derivativeN,

    -- ** Categorical Bifunctor
    (***),
    first,
    second,

    -- * Differentiable functions

    -- ** Elementary functions
    const,
    linear,
    (+),
    (-),
    negate,
    (*),
    (/),

    -- ** Tuple manipulations
    dup,
    setFirst,
    setSecond,
    forget,
    forgetFirst,
    forgetSecond,

    -- ** Exponential family functions
    log,
    logBase,
    exp,
    (**),
    pow,

    -- ** Trigonometric functions
    cos,
    sin,
    tan,
    asin,
    acos,
    atan,
    atan2,
    sinh,
    cosh,
    tanh,
    asinh,
    acosh,
    atanh,

    -- * Monadic differentiable functions
    pureBackprop,
    backpropExpr,
    loggingBackpropExpr,

    -- * Tools
    pureKleisli,
    simpleDifferentiable,
  )
where

import Control.CatBifunctor (first, second, (***))
import Debug.LoggingBackprop (backpropExpr, loggingBackpropExpr, pureKleisli)
import InfBackprop.Common
  ( Backprop (MkBackprop),
    BackpropFunc,
    backward,
    call,
    const,
    derivative,
    derivativeN,
    forward,
    pureBackprop,
  )
import Prelude.InfBackprop
  ( acos,
    acosh,
    asin,
    asinh,
    atan,
    atan2,
    atanh,
    cos,
    cosh,
    dup,
    exp,
    forget,
    forgetFirst,
    forgetSecond,
    linear,
    log,
    logBase,
    negate,
    pow,
    setFirst,
    setSecond,
    simpleDifferentiable,
    sin,
    sinh,
    tan,
    tanh,
    (*),
    (**),
    (+),
    (-),
    (/),
  )
