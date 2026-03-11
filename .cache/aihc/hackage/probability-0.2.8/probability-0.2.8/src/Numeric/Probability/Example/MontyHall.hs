module Numeric.Probability.Example.MontyHall where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition as Trans
import Numeric.Probability.Simulation ((~.), )

import Numeric.Probability.Percentage
    (Dist, RDist, Trans, )

import qualified Numeric.Probability.Monad as MonadExt

import Data.List ( (\\) )


{- no Random instance for Rational
type Probability = Rational
type Dist a  = Dist.T  Probability a
type RDist a = Rnd.Distribution Probability a
type Trans a = Transition    Probability a
-}

data Door = A | B | C
            deriving (Eq,Ord,Show)

doors :: [Door]
doors = [A,B,C]

data State = Doors {prize :: Door, chosen :: Door, opened :: Door}
             deriving (Eq,Ord,Show)


-- | initial configuration of the game status
start :: State
start = Doors {prize=u,chosen=u,opened=u} where u=undefined


{- |
Steps of the game:

 (1) hide the prize

 (2) choose a door

 (3) open a non-open door, not revealing the prize

 (4) apply strategy: switch or stay
-}
hide :: Trans State
hide s = Dist.uniform [s {prize = d} | d <- doors]

choose :: Trans State
choose s = Dist.uniform [s {chosen = d} | d <- doors]

open :: Trans State
open s = Dist.uniform [s {opened = d} | d <- doors \\ [prize s,chosen s]]

type Strategy = Trans State

switch :: Strategy
switch s = Dist.uniform [s {chosen = d} | d <- doors \\ [chosen s,opened s]]

stay :: Strategy
stay = Trans.id

game :: Strategy -> Trans State
game s = MonadExt.compose [hide,choose,open,s]


-- * Playing the game

data Outcome = Win | Lose
               deriving (Eq,Ord,Show)

result :: State -> Outcome
result s = if chosen s==prize s then Win else Lose

eval :: Strategy -> Dist Outcome
eval s = Dist.map result (game s start)

simEval :: Int -> Strategy -> RDist Outcome
simEval k s = Dist.map result `fmap` (k ~. game s) start


-- * Alternative modeling

firstChoice :: Dist Outcome
firstChoice = Dist.uniform [Win,Lose,Lose]

switch' :: Trans Outcome
switch' Win  = Dist.certainly Lose
switch' Lose = Dist.certainly Win


-- * Play the game the monadic way

type StrategyM = Door -> Door -> Door

stayM :: StrategyM
stayM chosenDoor _openedDoor = chosenDoor

switchM :: StrategyM
switchM chosenDoor openedDoor =
   let [finalDoor] = doors \\ [chosenDoor, openedDoor]
   in  finalDoor

evalM :: StrategyM -> Dist Outcome
evalM chooseFinalDoor =
   do prizeDoor  <- Dist.uniform doors
      chosenDoor <- Dist.uniform doors
      openedDoor <- Dist.uniform (doors \\ [prizeDoor, chosenDoor])
      return (if chooseFinalDoor chosenDoor openedDoor == prizeDoor
                then Win else Lose)
