-- Do not edit! Automatically created with doctest-extract from src/Combinatorics/Private.hs
{-# LINE 18 "src/Combinatorics/Private.hs" #-}

module Test.Combinatorics.Private where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 19 "src/Combinatorics/Private.hs" #-}
import     qualified Combinatorics.Private as CombPriv
import     Test.Combinatorics (genPermuteRep, genVariate, genChooseIndex)

import     qualified Test.QuickCheck as QC
import     Control.Applicative ((<$>))
import     Data.List.HT (allEqual)
import     Data.Eq.HT (equating)

genChoose     :: QC.Gen (Int, Int)
genChoose     = do
       n <- QC.choose (0,15)
       k <- QC.choose (-2,n)
       return (n,k)

genTuples     :: QC.Gen (Int, [Char])
genTuples     = do
       xs <- take 16 <$> QC.arbitrary
       n <- QC.choose (-1, length xs + 1)
       return (n,xs)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics.Private:46: "
{-# LINE 46 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 46 "src/Combinatorics/Private.hs" #-}
     (QC.forAll (take 6 <$> QC.arbitrary) $ \xs -> CombPriv.permuteRec xs == CombPriv.permuteMSL (xs::[Int]))
 DocTest.printPrefix "Combinatorics.Private:67: "
{-# LINE 67 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 67 "src/Combinatorics/Private.hs" #-}
     (QC.forAll (genPermuteRep 10) $ \xs -> CombPriv.permuteRep xs == CombPriv.permuteRepM xs)
 DocTest.printPrefix "Combinatorics.Private:98: "
{-# LINE 98 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 98 "src/Combinatorics/Private.hs" #-}
     (QC.forAll genChoose $ \(n,k) -> allEqual $ CombPriv.chooseRec n k : CombPriv.chooseMSL n k : CombPriv.chooseMSL0 n k : [])
 DocTest.printPrefix "Combinatorics.Private:132: "
{-# LINE 132 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 132 "src/Combinatorics/Private.hs" #-}
     (QC.forAll (QC.choose (-1,7)) $ \n -> QC.forAll genVariate $ \xs -> CombPriv.variateRep n xs == CombPriv.variateRepM n xs)
 DocTest.printPrefix "Combinatorics.Private:143: "
{-# LINE 143 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 143 "src/Combinatorics/Private.hs" #-}
     (QC.forAll (QC.choose (-1,7)) $ \n -> QC.forAll genVariate $ \xs -> CombPriv.variateRec n xs == CombPriv.variateMSL n xs)
 DocTest.printPrefix "Combinatorics.Private:160: "
{-# LINE 160 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 160 "src/Combinatorics/Private.hs" #-}
     (QC.forAll genTuples $ \(n,xs) -> allEqual $ CombPriv.tuplesRec n xs : CombPriv.tuplesRec0 n xs : CombPriv.tuplesMSL n xs : CombPriv.tuplesMSL0 n xs : [])
 DocTest.printPrefix "Combinatorics.Private:199: "
{-# LINE 199 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 199 "src/Combinatorics/Private.hs" #-}
     (QC.forAll genChooseIndex $ \(n,k,i) -> CombPriv.chooseUnrankRec n k i  ==  CombPriv.chooseUnrankList n k i)
 DocTest.printPrefix "Combinatorics.Private:264: "
{-# LINE 264 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 264 "src/Combinatorics/Private.hs" #-}
     (allEqual $ map (take 1000) (CombPriv.derangementNumbersPS0 : CombPriv.derangementNumbersPS1 : CombPriv.derangementNumbersInclExcl : [] :: [[Integer]]))
 DocTest.printPrefix "Combinatorics.Private:295: "
{-# LINE 295 "src/Combinatorics/Private.hs" #-}
 DocTest.property
{-# LINE 295 "src/Combinatorics/Private.hs" #-}
     (equating (take 20) CombPriv.surjectiveMappingNumbersPS (CombPriv.surjectiveMappingNumbersStirling :: [[Integer]]))
