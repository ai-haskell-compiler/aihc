{-# LANGUAGE NoImplicitPrelude #-}
{- |
An implementation of a Delay using a classical circular buffer
running in the State Thread monad.
-}
module Synthesizer.Plain.Filter.Delay.ST (modulated) where

import qualified Synthesizer.Plain.Interpolation as Interpolation

import qualified Algebra.RealField as RealField
import qualified Algebra.Additive  as Additive

import Control.Monad.ST.Lazy(runST,strictToLazyST,ST)
import Data.Array.ST

import NumericPrelude.Numeric
import NumericPrelude.Base


{-
I had no success in hiding ST in the 'modulatedST' function.
The explicit type signature is crucial.
-}
modulatedAction :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> [a] -> [v] -> ST s [v]
modulatedAction ip size ts xs =
   let ipNum  = Interpolation.number ip
       ipFunc = Interpolation.func   ip
   in  do arr <- strictToLazyST (newArray (0,2*size-1) zero)
                    :: Additive.C v => ST s (STArray s Int v)
          mapM (\(n,t,x) -> strictToLazyST $
                  do writeArray arr n x
                     writeArray arr (n+size) x
                     let (ti,tf) = splitFraction t
                     y <- mapM (readArray arr) (take ipNum [(n+ti) ..])
                     return (if ti<0
                               then error "negative delay"
                               else
                                 if size < ti+ipNum
                                   then error "too much delay"
                                   else ipFunc tf y))
               (zip3 (cycle [(size-1),(size-2)..0]) ts xs)

modulated :: (RealField.C a, Additive.C v) =>
   Interpolation.T a v -> Int -> [a] -> [v] -> [v]
modulated ip maxDelay ts xs =
   let offset = Interpolation.offset ip
   in  drop offset
          (runST
             (modulatedAction
                ip (maxDelay + Interpolation.number ip)
                (replicate offset zero ++ ts) xs))


