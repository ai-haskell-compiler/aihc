{- |

Approach: model a node with k predecessors as a function with k
          parameters

-}
module Numeric.Probability.Example.Bayesian where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition as Trans
import qualified Numeric.Probability.Monad as MonadExt
import Numeric.Probability.Distribution ((??), (?=<<), )



-- * Abbreviations, smart constructors

type Probability = Rational
type Dist a = Dist.T Probability a

type State  a = [a]
type PState a = Dist (State a)
type STrans a = State a -> PState a
type SPred  a = a -> State a -> Bool

event :: Probability -> a -> STrans a
event p e0 = Trans.maybe p (e0:)

happens :: Eq a => SPred a
happens = elem

network :: [STrans a] -> PState a
network = flip MonadExt.compose []


source :: Probability -> a -> STrans a
source = event

bin :: Eq a =>
   a -> a -> Probability -> Probability -> Probability -> Probability ->
   a -> STrans a
bin x y a b c d z s | elem x s && elem y s = event a z s
                    | elem x s             = event b z s
                    | elem y s             = event c z s
                    | otherwise            = event d z s


-- | Two possible causes for one effect

data Nodes = A | B | E deriving (Eq,Ord,Show)

g :: PState Nodes
g = network [source 0.1 A,
             source 0.2 B,
             bin A B 1 1 0.5 0 E]

-- * queries

e, aE, bE :: Probability
e  = happens E ??                g
aE = happens A ?? happens E ?=<< g
bE = happens B ?? happens E ?=<< g


{-
data State = State {causeA :: Bool, causeB :: Bool, effect :: Bool}
             deriving (Eq,Ord,Show)

nCauseA s = s{causeA=True}
-}

--
-- Wet grass example
--
-- cloudy = true 0.5
--
-- sprinkler c = dep c 0.1 0.5
--
-- rain c = dep c 0.8 0.2
--
-- wetGrass s r = bin s r 0.99 0.9 0.9 0
--
-- c = cloudy
-- s = sprinkler cloudy
-- r = rain cloudy
-- w = wetGrass s r


-- alarm :: Prob -> Prob -> Prob
-- alarm b e = cond b (pTrue 0.8)
--                    (cond e (pTrue 0.1) (pTrue 0.01))
--
-- john :: Prob -> Prob
-- john a = cond a (pTrue 0.7) (pTrue 0.1)
--
-- mary :: Prob -> Prob
-- mary a = cond a (pTrue 0.6) (pTrue 0.2)
--
--
-- maryWhenJohn = mary a ?? john a
--                where a = alarm (pTrue 0.5) (pTrue 0.1)
