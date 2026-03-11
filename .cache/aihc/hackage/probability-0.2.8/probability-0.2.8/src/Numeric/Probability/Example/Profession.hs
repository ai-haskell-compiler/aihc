{- |
The newspaper "Sueddeutsche" asked their readers
what professions 16 persons have,
by only showing the photographies of them and three choices.

Their statistics was:
22% readers had  0 to  5 correct answers   (category 0)
75% readers had  6 to 11 correct answers   (category 1)
 3% readers had 12 to 16 correct answers   (category 2)

Can this statistics be explained with random guessing,
or is there some information in the photographies
that the readers could utilize?

I got 6 correct answers.
-}

module Numeric.Probability.Example.Profession where

import qualified Numeric.Probability.Distribution as Dist


-- type Probability = Rational
type Probability = Double
type Dist a = Dist.T Probability a

correctAnswers :: Dist Int
correctAnswers = sum $ replicate 16 $ Dist.fromFreqs [(0,2), (1,1)]

categories :: Dist Int
categories =
   Dist.map (\n -> if n<=5 then 0 else if n<=11 then 1 else 2) correctAnswers
