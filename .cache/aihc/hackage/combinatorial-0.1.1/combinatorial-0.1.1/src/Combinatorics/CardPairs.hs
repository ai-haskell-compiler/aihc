{- |
Compute how often it happens
that a Queen and a King are adjacent in a randomly ordered card set.
-}
module Combinatorics.CardPairs (
   -- * general
   Card(..), CardCount(..),
   charFromCard,
   allPossibilities,
   numberOfAllPossibilities,
   possibilitiesCardsNaive,
   possibilitiesCardsDynamic,
   possibilitiesCardsBorderNaive,
   possibilitiesCardsBorderDynamic,
   possibilitiesCardsBorder2Dynamic,
   -- * examples
   cardSetSizeSkat, numberOfPossibilitiesSkat, probabilitySkat,
   cardSetSizeRummy, numberOfPossibilitiesRummy, probabilityRummy,
   cardSetSizeRummyJK, numberOfPossibilitiesRummyJK, probabilityRummyJK,
   -- * tests
   exampleOutput,
   adjacentCouplesSmall,
   allPossibilitiesSmall,
   allPossibilitiesMedium,
   allPossibilitiesSkat,
   ) where

import qualified Combinatorics as Comb

import Data.Array (Array, (!), array, )
import Data.Ix (Ix, )
import qualified Data.List.HT as ListHT

import qualified Control.Monad.Trans.State as State
import Control.Monad (liftM, liftM2, liftM3, replicateM, )

import Data.Ratio ((%), )


{- $setup
>>> import qualified Combinatorics.CardPairs as CardPairs
>>> import Combinatorics.CardPairs (CardCount(CardCount))
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Control.Applicative (liftA3)
>>> import Data.List.HT (allEqual)
>>> import Data.Array ((!))
>>>
>>> genCardCount :: QC.Gen (CardPairs.CardCount Int)
>>> genCardCount =
>>>    liftA3 CardPairs.CardCount
>>>       (QC.choose (0,5)) (QC.choose (0,5)) (QC.choose (0,5))
-}


type CardSet a = [(a, Int)]

data Card = Other | Queen | King
   deriving (Eq, Ord, Enum, Show)

charFromCard :: Card -> Char
charFromCard card =
   case card of
      Other -> ' '
      Queen -> 'q'
      King  -> 'k'

removeEach :: State.StateT (CardSet a) [] a
removeEach =
   State.StateT $
   map (\(pre,(x,n),post) ->
          (x, pre ++
              let m = pred n
              in (if m>0 then ((x,m):) else id)
              post)) .
   ListHT.splitEverywhere

normalizeSet :: CardSet a -> CardSet a
normalizeSet = filter ((>0) . snd)

allPossibilities :: CardSet a -> [[a]]
allPossibilities set =
   State.evalStateT
      (replicateM (sum (map snd set)) removeEach)
      (normalizeSet set)

allPossibilitiesSmall :: [[Card]]
allPossibilitiesSmall =
   allPossibilities [(Other, 4), (Queen, 2), (King, 2)]

allPossibilitiesMedium :: [[Card]]
allPossibilitiesMedium =
   allPossibilities [(Other, 4), (Queen, 4), (King, 4)]

allPossibilitiesSkat :: [[Card]]
allPossibilitiesSkat =
   allPossibilities [(Other, 24), (Queen, 4), (King, 4)]


adjacentCouple :: [Card] -> Bool
adjacentCouple =
   or .
   ListHT.mapAdjacent
      (\x y -> (x==Queen && y==King) || (x==King && y==Queen))

adjacentCouplesSmall :: [[Card]]
adjacentCouplesSmall =
   filter adjacentCouple $
   allPossibilities [(Other, 4), (Queen, 2), (King, 2)]

exampleOutput :: IO ()
exampleOutput =
   mapM_ (print . map charFromCard) allPossibilitiesSmall


{- |
Candidate for utility-ht:
-}
sample :: (a -> b) -> [a] -> [(a,b)]
sample f = map (\x -> (x, f x))


data CardCount i =
   CardCount {otherCount, queenCount, kingCount :: i}
      deriving (Eq, Ord, Ix, Show)


possibilitiesCardsNaive ::
   CardCount Int -> Integer
possibilitiesCardsNaive (CardCount no nq nk) =
   fromIntegral $ length $
   filter adjacentCouple $
   allPossibilities [(Other,no), (Queen,nq), (King,nk)]

possibilitiesCardsDynamic ::
   CardCount Int -> Array (CardCount Int) Integer
possibilitiesCardsDynamic (CardCount mo mq mk) =
   let border =
          liftM3 CardCount [0,1]   [0..mq] [0..mk] ++
          liftM3 CardCount [0..mo] [0,1]   [0..mk] ++
          liftM3 CardCount [0..mo] [0..mq] [0,1]
       p =
          array (CardCount 0 0 0, CardCount mo mq mk) $
             sample possibilitiesCardsNaive border ++
             sample
                (\(CardCount no nq nk) ->
                   -- " ******"
                   p!(CardCount (no-1) nq nk) +
                   -- "q *****"
                   p!(CardCount (no-1) (nq-1) nk) +
                   -- "k *****"
                   p!(CardCount (no-1) nq (nk-1)) +
                   -- The following case is not handled correctly,
                   -- because the second 'q' can be part of a "qk".
                   -- "qq*****"
                   p!(CardCount no (nq-2) nk) +
                   -- "kk*****"
                   p!(CardCount no nq (nk-2)) +
                   -- "kq*****"
                   -- "qk*****"
                   2 * Comb.multinomial [fromIntegral no, fromIntegral nq-1, fromIntegral nk-1])
                (liftM3 CardCount [2..mo] [2..mq] [2..mk])
   in  p


sumCard :: Num i => CardCount i -> i
sumCard (CardCount x y z) = x+y+z

{-
Candidate for utility-ht: slice

http://hackage.haskell.org/packages/archive/event-list/0.1/doc/html/Data-EventList-Relative-TimeBody.html#v:slice
could be rewritten for plain lists.
-}

{- |
Count the number of card set orderings
with adjacent queen and king.
We return a triple where the elements count with respect to an additional condition:
(card set starts with an ordinary card ' ',
 start with queen 'q',
 start with king 'k')

prop> allEqual [CardPairs.possibilitiesCardsBorderNaive (CardCount 2 3 5), CardPairs.possibilitiesCardsBorderDynamic (CardCount 5 5 5) ! (CardCount 2 3 5), CardPairs.possibilitiesCardsBorder2Dynamic (CardCount 5 5 5) ! (CardCount 2 3 5)]
prop> QC.forAll genCardCount $ \cc -> allEqual [CardPairs.possibilitiesCardsBorderNaive cc, CardPairs.possibilitiesCardsBorderDynamic cc ! cc, CardPairs.possibilitiesCardsBorder2Dynamic cc ! cc]
-}
possibilitiesCardsBorderNaive ::
   CardCount Int -> CardCount Integer
possibilitiesCardsBorderNaive (CardCount no nq nk) =
   foldl (\n (card:_) ->
      case card of
         Other -> n{otherCount = 1 + otherCount n}
         Queen -> n{queenCount = 1 + queenCount n}
         King  -> n{kingCount  = 1 + kingCount n})
      (CardCount 0 0 0) $
   filter adjacentCouple $
   allPossibilities [(Other,no), (Queen,nq), (King,nk)]

possibilitiesCardsBorderDynamic ::
   CardCount Int -> Array (CardCount Int) (CardCount Integer)
possibilitiesCardsBorderDynamic (CardCount mo mq mk) =
   let p =
          array (CardCount 0 0 0, CardCount mo mq mk) $
             liftM  (\ nq -> (CardCount 0 nq 0, CardCount 0 0 0)) [1..mq] ++
             liftM  (\ nk -> (CardCount 0 0 nk, CardCount 0 0 0)) [1..mk] ++
             liftM2 (\ nq nk -> ((CardCount 0 nq nk),
                       let s = fromIntegral $ nq+nk-1
                       in  CardCount 0
                              (Comb.binomial s (fromIntegral nk))
                              (Comb.binomial s (fromIntegral nq))))
                [1..mq] [1..mk] ++
             -- (CardCount 0 0 0) is redundant in the list,
             -- its number is not needed anyway
             liftM2 (\ no nk -> (CardCount no 0 nk, CardCount 0 0 0)) [0..mo] [0..mk] ++
             liftM2 (\ no nq -> (CardCount no nq 0, CardCount 0 0 0)) [0..mo] [0..mq] ++
             sample
                (\(CardCount no nq nk) ->
                   let allP = Comb.multinomial [fromIntegral no, fromIntegral nq-1, fromIntegral nk-1]
                   in  CardCount
                          (-- " ******"
                           sumCard (p ! CardCount (no-1) nq nk))
                          (-- "q *****"
                           otherCount (p ! CardCount no (nq-1) nk) +
                           -- "qq*****"
                           queenCount (p ! CardCount no (nq-1) nk) +
                           -- "qk*****"
                           allP)
                          (-- "k *****"
                           otherCount (p ! CardCount no nq (nk-1)) +
                           -- "kk*****"
                           kingCount  (p ! CardCount no nq (nk-1)) +
                           -- "kq*****"
                           allP))
                (liftM3 CardCount [1..mo] [1..mq] [1..mk])
   in  p

possibilitiesCardsBorder2Dynamic ::
   CardCount Int -> Array (CardCount Int) (CardCount Integer)
possibilitiesCardsBorder2Dynamic (CardCount mo mq mk) =
   let p =
          array (CardCount 0 0 0, CardCount mo mq mk) $
          flip sample (liftM3 CardCount [0..mo] [0..mq] [0..mk]) $
          \(CardCount no nq nk) ->
             let allP = Comb.multinomial [fromIntegral no, fromIntegral nq-1, fromIntegral nk-1]
                 test0 n f g =
                    if n==0
                      then 0
                      else g $ p ! f (n-1)
             in  CardCount
                    (test0 no (\io -> CardCount io nq nk) $
                       -- " ******"
                       sumCard)
                    (test0 nq (\iq -> CardCount no iq nk) $ \pc ->
                       -- "q *****"
                       otherCount pc +
                       -- "qq*****"
                       queenCount pc +
                       -- "qk*****"
                       allP)
                    (test0 nk (\ik -> CardCount no nq ik) $ \pc ->
                       -- "k *****"
                       otherCount pc +
                       -- "kk*****"
                       kingCount  pc +
                       -- "kq*****"
                       allP)
   in  p

{-
for \{o,q,k\} \subset \{1,2,\dots\}
O_{o,q,k} = O_{o-1,q,k} + Q_{o-1,q,k} + K_{o-1,q,k}
Q_{o,q,k} = O_{o,q-1,k} + Q_{o,q-1,k} + M(o,q-1,k-1)
K_{o,q,k} = O_{o,q,k-1} + K_{o,q,k-1} + M(o,q-1,k-1)

O = (O+Q+K)->(1,0,0)
Q = (O+Q)->(0,1,0) + M->(0,1,1)
K = (O+K)->(0,0,1) + M->(0,1,1)

O = (O+Q+K)·x
Q = (O+Q)·y + y·z/(1-x-y-z)
K = (O+K)·z + y·z/(1-x-y-z)

Q·(1-y) = O·y + y·z/(1-x-y-z)
K·(1-z) = O·z + y·z/(1-x-y-z)

O = (O + (O·y + y·z/(1-x-y-z))/(1-y) + (O·z + y·z/(1-x-y-z))/(1-z))·x
O·(1-x-y-z)·(1-x)
   = ((O·y·(1-x-y-z) + y·z)/(1-y) + (O·z·(1-x-y-z) + y·z)/(1-z))·x
O·(1-x-y-z)·(1-x)·(1-y)·(1-z)
   = ((O·(1-x-y-z) + z)·y·(1-z) + (O·(1-x-y-z) + y)·z·(1-y))·x
O·(1-x-y-z + (1+x)·y·z)·(1-x-y-z) = x·y·z·(2-y-z)

O+Q+K = O/x
  = y·z·(2-y-z) / (1-x-y-z + (1+x)·y·z) / (1-x-y-z)
-}


{-
Pascalsches Dreieck als Potenzreihe von 1/(1-x-y)
ausgerechnet mit Matrizen.

/n_{0,2}\   /n_{0,1}\
|n_{1,1}| = |n_{1,0}|
\n_{1,2}/   \n_{1,1}/

/n_{1,1}\   /n_{0,1}\
|n_{2,0}| = |n_{1,0}|
\n_{2,1}/   \n_{1,1}/
-}


numberOfAllPossibilities :: CardCount Int -> Integer
numberOfAllPossibilities (CardCount no nq nk) =
   Comb.multinomial [fromIntegral no, fromIntegral nq, fromIntegral nk]


cardSetSizeSkat :: CardCount Int
cardSetSizeSkat = CardCount 24 4 4

numberOfPossibilitiesSkat :: Integer
numberOfPossibilitiesSkat =
   sumCard $ possibilitiesCardsBorder2Dynamic cardSetSizeSkat ! cardSetSizeSkat

probabilitySkat :: Double
probabilitySkat =
   fromRational $
   numberOfPossibilitiesSkat % numberOfAllPossibilities cardSetSizeSkat


cardSetSizeRummy :: CardCount Int
cardSetSizeRummy = CardCount 44 4 4

numberOfPossibilitiesRummy :: Integer
numberOfPossibilitiesRummy =
   sumCard $ possibilitiesCardsBorder2Dynamic cardSetSizeRummy ! cardSetSizeRummy

probabilityRummy :: Double
probabilityRummy =
   fromRational $
   numberOfPossibilitiesRummy % numberOfAllPossibilities cardSetSizeRummy


{- |
Allow both Jack and King adjacent to Queen.
-}
cardSetSizeRummyJK :: CardCount Int
cardSetSizeRummyJK = CardCount 40 4 8

numberOfPossibilitiesRummyJK :: Integer
numberOfPossibilitiesRummyJK =
   sumCard $ possibilitiesCardsBorder2Dynamic cardSetSizeRummyJK ! cardSetSizeRummyJK

probabilityRummyJK :: Double
probabilityRummyJK =
   fromRational $
   numberOfPossibilitiesRummyJK % numberOfAllPossibilities cardSetSizeRummyJK
