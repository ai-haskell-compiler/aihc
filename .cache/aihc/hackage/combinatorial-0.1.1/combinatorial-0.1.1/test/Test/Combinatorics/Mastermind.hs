-- Do not edit! Automatically created with doctest-extract from src/Combinatorics/Mastermind.hs
{-# LINE 22 "src/Combinatorics/Mastermind.hs" #-}

module Test.Combinatorics.Mastermind where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 23 "src/Combinatorics/Mastermind.hs" #-}
import     qualified Combinatorics.Mastermind as Mastermind
import     qualified Combinatorics as Comb
import     qualified Test.QuickCheck as QC
import     Control.Monad (replicateM)
import     Data.List (genericLength)

genMastermindDistinct     :: QC.Gen (Int, Int, Int, Int)
genMastermindDistinct     = do
       n <- QC.choose (0,12)
       k <- QC.choose (0, min 5 n)
       b <- QC.choose (0,k)
       w <- QC.choose (0,k-b)
       return (n,k,b,w)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics.Mastermind:48: "
{-# LINE 48 "src/Combinatorics/Mastermind.hs" #-}
 DocTest.example
{-# LINE 48 "src/Combinatorics/Mastermind.hs" #-}
   (filter ((Mastermind.Eval 2 0 ==) . Mastermind.evaluate "aabbb") $ replicateM 5 ['a'..'c'])
  [ExpectedLine [LineChunk "[\"aaaaa\",\"aaaac\",\"aaaca\",\"aaacc\",\"aacaa\",\"aacac\",\"aacca\",\"aaccc\",\"acbcc\",\"accbc\",\"acccb\",\"cabcc\",\"cacbc\",\"caccb\",\"ccbbc\",\"ccbcb\",\"cccbb\"]"]]
 DocTest.printPrefix "Combinatorics.Mastermind:86: "
{-# LINE 86 "src/Combinatorics/Mastermind.hs" #-}
 DocTest.property
{-# LINE 86 "src/Combinatorics/Mastermind.hs" #-}
     (QC.forAll genMastermindDistinct $ \(n,k,b,w) -> let alphabet = take n ['a'..]; code = take k alphabet in Mastermind.numberDistinct n k b w == (genericLength $ filter ((Mastermind.Eval b w ==) . Mastermind.evaluate code) $ Comb.variate k alphabet))
 DocTest.printPrefix "Combinatorics.Mastermind:95: "
{-# LINE 95 "src/Combinatorics/Mastermind.hs" #-}
 DocTest.property
{-# LINE 95 "src/Combinatorics/Mastermind.hs" #-}
     (QC.forAll genMastermindDistinct $ \(n,k,_b,w) -> Mastermind.numberDistinctWhite n k w == Mastermind.numberDistinct n k 0 w)
