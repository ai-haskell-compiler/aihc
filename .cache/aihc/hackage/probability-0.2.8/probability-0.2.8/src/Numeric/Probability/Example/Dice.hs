module Numeric.Probability.Example.Dice where

import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), )
import Control.Monad (liftM2, replicateM)


type Die = Int

type Probability = Rational
type Dist = Dist.T Probability

die :: Dist Die
die = Dist.uniform [1..6]

-- | product of independent distributions
twoDice :: Dist (Die,Die)
twoDice = liftM2 (,) die die

dice :: Int -> Dist [Die]
dice = flip replicateM die


twoSixes :: Probability
twoSixes = (==(6,6)) ?? twoDice

{- |
@sixes p n@ computes the probability of getting
p sixes (@>1@, @==2@, ...) when rolling n dice
-}
sixes :: (Int -> Bool) -> Int -> Probability
sixes p n = (p . length . filter (==6)) ?? dice n

droll :: Dist Die
droll =
   liftM2 (+) (Dist.uniform [0,1]) die

g3 :: Probability
g3 = (>3) ?? die

addTwo :: Dist Die
addTwo =
   liftM2 (+) die die
