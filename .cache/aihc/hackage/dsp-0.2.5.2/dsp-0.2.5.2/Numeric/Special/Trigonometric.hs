module Numeric.Special.Trigonometric (csc,   sec,   cot,
				      acsc,  asec,  acot,
				      csch,  sech,  coth,
				      acsch, asech, acoth
				     ) where

import Data.Complex

-- Circular functions

csc :: Floating a => a -> a
csc z = 1 / sin z

sec :: Floating a => a -> a
sec z = 1 / cos z

cot :: Floating a => a -> a
cot z = 1 / tan z

-- Inverse circular functions

acsc :: Floating a => a -> a
acsc z = asin $ 1 / z

asec :: Floating a => a -> a
asec z = acos $ 1 / z

acot :: Floating a => a -> a
acot z = atan $ 1 / z

-- Hyperbolic functions

csch :: Floating a => a -> a
csch z = 1 / sinh z

sech :: Floating a => a -> a
sech z = 1 / cosh z

coth :: Floating a => a -> a
coth z = 1 / tanh z

-- Inverse hyperbolic functions

acsch :: Floating a => a -> a
acsch z = asinh $ 1 / z

asech :: Floating a => a -> a
asech z = acosh $ 1 / z

acoth :: Floating a => a -> a
acoth z = atanh $ 1 / z

-- Specialization pragmas

{-# specialize csc :: Double         -> Double         #-}
{-# specialize csc :: Complex Double -> Complex Double #-}
{-# specialize sec :: Double         -> Double         #-}
{-# specialize sec :: Complex Double -> Complex Double #-}
{-# specialize cot :: Double         -> Double         #-}
{-# specialize cot :: Complex Double -> Complex Double #-}

{-# specialize acsc :: Double         -> Double         #-}
{-# specialize acsc :: Complex Double -> Complex Double #-}
{-# specialize asec :: Double         -> Double         #-}
{-# specialize asec :: Complex Double -> Complex Double #-}
{-# specialize acot :: Double         -> Double         #-}
{-# specialize acot :: Complex Double -> Complex Double #-}

{-# specialize csch :: Double         -> Double         #-}
{-# specialize csch :: Complex Double -> Complex Double #-}
{-# specialize sech :: Double         -> Double         #-}
{-# specialize sech :: Complex Double -> Complex Double #-}
{-# specialize coth :: Double         -> Double         #-}
{-# specialize coth :: Complex Double -> Complex Double #-}

{-# specialize acsch :: Double         -> Double         #-}
{-# specialize acsch :: Complex Double -> Complex Double #-}
{-# specialize asech :: Double         -> Double         #-}
{-# specialize asech :: Complex Double -> Complex Double #-}
{-# specialize acoth :: Double         -> Double         #-}
{-# specialize acoth :: Complex Double -> Complex Double #-}
