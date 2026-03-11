-- Do not edit! Automatically created with doctest-extract from src/Combinatorics/Partitions.hs
{-# LINE 18 "src/Combinatorics/Partitions.hs" #-}

module Test.Combinatorics.Partitions where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 19 "src/Combinatorics/Partitions.hs" #-}
import     qualified Combinatorics.Partitions as Parts
import     qualified Test.QuickCheck as QC
import     Data.List (genericLength)
import     Data.Eq.HT (equating)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Combinatorics.Partitions:59: "
{-# LINE 59 "src/Combinatorics/Partitions.hs" #-}
 DocTest.property
{-# LINE 59 "src/Combinatorics/Partitions.hs" #-}
     (QC.forAll (QC.choose (0,100)) Parts.propInfProdLinearFactors)
 DocTest.printPrefix "Combinatorics.Partitions:84: "
{-# LINE 84 "src/Combinatorics/Partitions.hs" #-}
 DocTest.property
{-# LINE 84 "src/Combinatorics/Partitions.hs" #-}
     (Parts.propPentagonalsDifP 10000)
 DocTest.printPrefix "Combinatorics.Partitions:92: "
{-# LINE 92 "src/Combinatorics/Partitions.hs" #-}
 DocTest.property
{-# LINE 92 "src/Combinatorics/Partitions.hs" #-}
     (Parts.propPentagonalsDifN 10000)
 DocTest.printPrefix "Combinatorics.Partitions:137: "
{-# LINE 137 "src/Combinatorics/Partitions.hs" #-}
 DocTest.property
{-# LINE 137 "src/Combinatorics/Partitions.hs" #-}
     (Parts.propPentagonalPowerSeries 1000)
 DocTest.printPrefix "Combinatorics.Partitions:170: "
{-# LINE 170 "src/Combinatorics/Partitions.hs" #-}
 DocTest.property
{-# LINE 170 "src/Combinatorics/Partitions.hs" #-}
     (QC.forAll (QC.choose (1,10)) $ \k -> QC.forAll (QC.choose (0,50)) $ \n -> Parts.partitionsInc k n == Parts.allPartitionsInc !! k !! n)
 DocTest.printPrefix "Combinatorics.Partitions:171: "
{-# LINE 171 "src/Combinatorics/Partitions.hs" #-}
 DocTest.property
{-# LINE 171 "src/Combinatorics/Partitions.hs" #-}
     (equating (take 30) (map genericLength (Parts.allPartitionsInc !! 1)) Parts.numPartitions)
