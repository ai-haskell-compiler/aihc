module Numeric.Probability.Example.Collection where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Random as Rnd
import Numeric.Probability.Distribution ((??), )
import Numeric.Probability.Simulation ((~.), )

import Numeric.Probability.Percentage (Dist)

import Numeric.Probability.Monad (doWhile, )
import Control.Monad.Trans.State (StateT(StateT, runStateT), evalStateT, )
import Control.Monad (liftM2, replicateM, )

import qualified Data.List.HT as ListHT
import System.Random (Random)



type Collection a = [a]

type Probability = Rational


selectOne :: (Fractional prob) =>
   StateT (Collection a) (Dist.T prob) a
selectOne =
   StateT $ Dist.uniform . ListHT.removeEach

select1 :: (Fractional prob) => Collection a -> Dist.T prob a
select1 = evalStateT selectOne

select2 :: (Fractional prob) => Collection a -> Dist.T prob (a,a)
select2 = evalStateT (liftM2 (,) selectOne selectOne)

select :: (Fractional prob) => Int -> Collection a -> Dist.T prob [a]
select n = evalStateT (replicateM n selectOne)


-- * Example collections

-- ** marbles

data Marble = R | G | B deriving (Eq,Ord,Show)

bucket :: Collection Marble
bucket = [R,R,R,R,R, G,G,G, B,B]

jar :: Collection Marble
jar = [R,R,G,G,B]

-- pRGB = prob (just [R,G,B]) (select 3 bucket)
pRGB :: Probability
pRGB = Dist.just [R,G,B] ?? select 3 jar
pRG :: Probability
pRG  = Dist.oneOf [[R,G],[G,R]] ?? select 2 jar

-- ** cards

data Suit = Club | Spade | Heart | Diamond
            deriving (Eq,Ord,Show,Enum)

data Rank = Plain Int | Jack | Queen | King | Ace
            deriving (Eq,Ord,Show)

type Card = (Rank,Suit)

plains :: [Rank]
plains = map Plain [2..10]

faces :: [Rank]
faces = [Jack,Queen,King,Ace]

isFace :: Card -> Bool
isFace (r,_) = r `elem` faces
-- isFace = (`elem` faces) . fst

isPlain :: Card -> Bool
isPlain (r,_) = r `elem` plains

ranks :: [Rank]
ranks = plains ++ faces

suits :: [Suit]
suits = [Club,Spade,Heart,Diamond]

deck :: Collection Card
deck = liftM2 (,) ranks suits


-- * Example

{- | mini-blackjack:
draw 2 cards, and if value is less than 14, continue drawing
until value equals or exceeds 14.  if values exceeds 21,
you lose, otherwise you win.
-}

value :: Card -> Int
value ((Plain n),_) = n
value (Ace,_) = 11
value _ = 10

totalValue :: Collection Card -> Int
totalValue cards = sum (map value cards)

-- this can be made with StateT, too, I think
draw :: (Fractional prob) =>
   ([Card], Collection Card) -> Dist.T prob ([Card], Collection Card)
draw (cards,cl) =
   runStateT (fmap (:cards) selectOne) cl

drawF :: ([Card], Collection Card) -> Dist ([Card], Collection Card)
drawF = draw


drawTo16 :: Rnd.T ([Card], Collection Card)
drawTo16 =
   doWhile
      (\(cards,_) -> totalValue cards < 16)
      (Rnd.change drawF) ([], deck)

win :: ([Card], b) -> Bool
win (cards,_) = totalValue cards <= 21

chanceWin :: (Fractional prob, Ord prob, Random prob) =>
   Rnd.T (Dist.T prob Bool)
chanceWin = fmap (Dist.map win) ((100 ~. const drawTo16) undefined)
