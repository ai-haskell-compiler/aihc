{- |
Ceneralization of "Numeric.Probability.Example.Boys"

Consider a family of n children.  Given that there are k boys in the family,
what is the probability that there are m boys in the family?
-}

module Numeric.Probability.Example.NBoys where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution (Event, (??), (?=<<), )

import Numeric.Probability.Example.Boys
   (Dist, Probability, Child(Boy), birth, )

import Control.Monad (replicateM)


type Family = [Child]

family :: Int -> Dist Family
family n = replicateM n birth

countBoys :: Family -> Int
countBoys = length . filter (==Boy)

boys :: Int -> Event Family
boys k f = countBoys f >= k

nBoys :: Int -> Int -> Int -> Probability
nBoys n k m =  boys m ?? boys k ?=<< family n

numBoys :: Int -> Int -> Dist Int
numBoys n k = Dist.map countBoys (boys k ?=<< family n)


-- * Special cases

-- | only boys in a family that has one boy
onlyBoys1 :: Int -> Probability
onlyBoys1 n = nBoys n 1 n
