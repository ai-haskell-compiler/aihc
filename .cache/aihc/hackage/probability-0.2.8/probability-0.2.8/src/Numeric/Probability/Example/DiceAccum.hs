{- |
We play the following game:
We roll a die until we stop or we get three spots.
In the first case we own all spots obtained so far,
in the latter case we own nothing.

What is the strategy for maximizing the expected score?
-}
module Numeric.Probability.Example.DiceAccum where

import qualified Numeric.Probability.Random as Rnd
import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition as Trans
import qualified Numeric.Probability.Monad as MonadExt
import Numeric.Probability.Trace (Trace)

import Numeric.Probability.Example.Dice (Die, )


type Score = Int


die :: Fractional prob => Dist.T prob Die
die = Dist.uniform [1..6]

roll :: Fractional prob => Trans.T prob (Maybe Score)
roll =
   maybe
     (return Nothing)
     (\score -> flip fmap die $
         \spots ->
            -- where is my beloved 'toMaybe' ?
            if spots == 3
              then Nothing
              else Just (score + spots))

continue :: Score -> Bool
continue scoreInt =
   let score = fromIntegral scoreInt :: Rational
   in  Dist.expected
          (Dist.uniform (0 : map (score+) [1,2,4,5,6])) > score

-- | optimal strategy
strategy :: Fractional prob => Trans.T prob (Maybe Score)
strategy s0 =
   maybe
     (return Nothing)
     (\score ->
         if continue score
           then roll s0
           else return s0) s0

-- | distribution of the scores that are achieved with the optimal strategy
game :: Fractional prob => Dist.T prob (Maybe Score)
game =
   Trans.compose (replicate 18 strategy) (Just 0)
   -- MonadExt.compose (replicate 8 turn) (Just 0)


{- too inefficient
game :: Fractional prob => Dist.T prob Score
game =
   let turn score =
          if continue score
            then roll score >>= \s -> if s==0 then return 0 else turn s
            else return score
   in  turn 0
-}


walk :: Int -> IO (Trace (Maybe Score))
walk n =
   Rnd.run $
   MonadExt.walk n
      (Rnd.change (roll :: Trans.T Double (Maybe Score)))
      (Just 0)
