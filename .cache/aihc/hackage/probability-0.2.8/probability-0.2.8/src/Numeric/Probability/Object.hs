{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{- |
Portability: Multi-parameter type class with functional dependency

Abstract interface to probabilistic objects
like random generators and probability distributions.
It allows to use the same code
both for computing complete distributions
and for generating random values according to the distribution.
The latter one is of course more efficient
and may be used for approximation of the distribution by simulation.

Maybe a better name is @Experiment@.
-}
module Numeric.Probability.Object where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Random as Rnd
import qualified Numeric.Probability.Shape as Shape

import qualified Data.List as List

import qualified Control.Monad.Trans.Except as ME
import Control.Monad (liftM, )


class Monad obj => C prob obj | obj -> prob where
   fromFrequencies :: [(a,prob)] -> obj a


instance C Double Rnd.T where
   fromFrequencies = Rnd.pick . Dist.fromFreqs

instance Fractional prob => C prob (Dist.T prob) where
   fromFrequencies = Dist.fromFreqs

instance C prob obj => C prob (ME.ExceptT b obj) where
   fromFrequencies =
      ME.ExceptT . liftM Right . fromFrequencies



type Spread obj a = [a] -> obj a

shape :: (C prob obj, Fractional prob) =>
   (prob -> prob) -> Spread obj a
shape _ [] = error "Probability.shape: empty list"
shape f xs =
   let incr = 1 / fromIntegral (length xs - 1)
       ps = List.map f (iterate (+incr) 0)
   in  fromFrequencies (zip xs ps)

linear :: (C prob obj, Fractional prob) => Spread obj a
linear = shape Shape.linear

uniform :: (C prob obj, Fractional prob) => Spread obj a
uniform = shape Shape.uniform

negExp :: (C prob obj, Floating prob) => Spread obj a
negExp = shape Shape.negExp

normal :: (C prob obj, Floating prob) => Spread obj a
normal = shape Shape.normal

enum :: (C prob obj, Floating prob) => [Int] -> Spread obj a
enum  =  relative . List.map fromIntegral

{- |
Give a list of frequencies, they do not need to sum up to 1.
-}
relative :: (C prob obj, Floating prob) => [prob] -> Spread obj a
relative ns = fromFrequencies . flip zip ns
