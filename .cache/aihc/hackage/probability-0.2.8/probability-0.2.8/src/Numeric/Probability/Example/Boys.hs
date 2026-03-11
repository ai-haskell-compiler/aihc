{- |
Consider a family of two children.  Given that there is a boy in the family,
what is the probability that there are two boys in the family?
-}

module Numeric.Probability.Example.Boys where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), (?=<<), )

import Control.Monad (liftM2, )


type Probability = Rational
type Dist a = Dist.T Probability a

data Child = Boy | Girl
             deriving (Eq,Ord,Show)

type Family = (Child, Child)

birth :: Dist Child
birth = Dist.uniform [Boy, Girl]

family :: Dist Family
family = liftM2 (,) birth birth

allBoys :: Dist.Event Family
allBoys (c0, c1) = (c0 == Boy && c1 == Boy)

existsBoy :: Dist.Event Family
existsBoy (c0, c1) = (c0 == Boy || c1 == Boy)

familyWithBoy :: Dist Family
familyWithBoy = existsBoy ?=<< family
{-
familyWithBoy =
   do f <- family
      guard (existsBoy f)
      return f
-}

twoBoys :: Probability
twoBoys = allBoys ?? familyWithBoy


countBoy :: Child -> Int
countBoy Boy = 1
countBoy Girl = 0

countBoys :: Family -> Int
countBoys (c0,c1) = countBoy c0 + countBoy c1

numBoys :: Dist Int
numBoys = Dist.map countBoys familyWithBoy
