{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Maintainer  :  numericprelude@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes

Abstraction of bases of finite dimensional modules
-}

module Algebra.ModuleBasis where

import qualified Number.Ratio as Ratio

import qualified Algebra.PrincipalIdealDomain as PID
import qualified Algebra.Module   as Module
import Algebra.Ring     (one, fromInteger)
import Algebra.Additive ((+), zero)

import Data.List (map, length, (++))

import Prelude(Eq, (==), Bool, Int, Integer, Float, Double, asTypeOf, )

{- |
It must hold:

>   Module.linearComb (flatten v `asTypeOf` [a]) (basis a) == v
>   dimension a v == length (flatten v `asTypeOf` [a])
-}
class (Module.C a v) => C a v where
    {- | basis of the module with respect to the scalar type,
         the result must be independent of argument, 'Prelude.undefined' should suffice. -}
    basis :: a -> [v]
    -- | scale a vector by a scalar
    flatten :: v -> [a]
    {- | the size of the basis, should also work for undefined argument,
         the result must be independent of argument, 'Prelude.undefined' should suffice. -}
    dimension :: a -> v -> Int

{-* Instances for atomic types -}

instance C Float Float where
   basis _ = [one]
   flatten = (:[])
   dimension _ _ = 1

instance C Double Double where
   basis _ = [one]
   flatten = (:[])
   dimension _ _ = 1

instance C Int Int where
   basis _ = [one]
   flatten = (:[])
   dimension _ _ = 1

instance C Integer Integer where
   basis _ = [one]
   flatten = (:[])
   dimension _ _ = 1

instance (PID.C a) => C (Ratio.T a) (Ratio.T a) where
   basis _ = [one]
   flatten = (:[])
   dimension _ _ = 1



{-* Instances for composed types -}

instance (C a v0, C a v1) => C a (v0, v1) where
   basis s = map (\v -> (v,zero)) (basis s) ++
             map (\v -> (zero,v)) (basis s)
   flatten (x0,x1) = flatten x0 ++ flatten x1
   dimension s ~(x0,x1) = dimension s x0 + dimension s x1

instance (C a v0, C a v1, C a v2) => C a (v0, v1, v2) where
   basis s = map (\v -> (v,zero,zero)) (basis s) ++
             map (\v -> (zero,v,zero)) (basis s) ++
             map (\v -> (zero,zero,v)) (basis s)
   flatten (x0,x1,x2) = flatten x0 ++ flatten x1 ++ flatten x2
   dimension s ~(x0,x1,x2) = dimension s x0 + dimension s x1 + dimension s x2



{- * Properties -}

propFlatten :: (Eq v, C a v) => a -> v -> Bool
propFlatten a v  =  Module.linearComb (flatten v `asTypeOf` [a]) (basis a) == v

propDimension :: (C a v) => a -> v -> Bool
propDimension a v  =  dimension a v == length (flatten v `asTypeOf` [a])
