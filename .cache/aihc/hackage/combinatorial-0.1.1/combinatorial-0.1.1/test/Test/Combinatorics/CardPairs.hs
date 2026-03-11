-- Do not edit! Automatically created with doctest-extract from src/Combinatorics/CardPairs.hs
{-# LINE 40 "src/Combinatorics/CardPairs.hs" #-}

module Test.Combinatorics.CardPairs where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 41 "src/Combinatorics/CardPairs.hs" #-}
import     qualified Combinatorics.CardPairs as CardPairs
import     Combinatorics.CardPairs (CardCount(CardCount))

import     qualified Test.QuickCheck as QC
import     Control.Applicative (liftA3)
import     Data.List.HT (allEqual)
import     Data.Array ((!))

genCardCount     :: QC.Gen (CardPairs.CardCount Int)
genCardCount     =
       liftA3 CardPairs.CardCount
          (QC.choose (0,5)) (QC.choose (0,5)) (QC.choose (0,5))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics.CardPairs:184: "
{-# LINE 184 "src/Combinatorics/CardPairs.hs" #-}
 DocTest.property
{-# LINE 184 "src/Combinatorics/CardPairs.hs" #-}
     (allEqual [CardPairs.possibilitiesCardsBorderNaive (CardCount 2 3 5), CardPairs.possibilitiesCardsBorderDynamic (CardCount 5 5 5) ! (CardCount 2 3 5), CardPairs.possibilitiesCardsBorder2Dynamic (CardCount 5 5 5) ! (CardCount 2 3 5)])
 DocTest.printPrefix "Combinatorics.CardPairs:185: "
{-# LINE 185 "src/Combinatorics/CardPairs.hs" #-}
 DocTest.property
{-# LINE 185 "src/Combinatorics/CardPairs.hs" #-}
     (QC.forAll genCardCount $ \cc -> allEqual [CardPairs.possibilitiesCardsBorderNaive cc, CardPairs.possibilitiesCardsBorderDynamic cc ! cc, CardPairs.possibilitiesCardsBorder2Dynamic cc ! cc])
