module Test.Knead where

import Test.Utility
         (InflatedString, setAssigns, equivalentSolutions,
          forAllShrinkSmall, genInflatedWords)

import qualified Math.SetCover.Exact.Knead.Saturated as ESC_KneadSat
import qualified Math.SetCover.Exact.Knead as ESC_Knead
import qualified Math.SetCover.Exact as ESC

import qualified Test.QuickCheck as QC


partitionKnead :: [InflatedString] -> Bool
partitionKnead xs =
   let asns = setAssigns xs
   in ESC.partitions asns
      ==
      ESC_Knead.partitions asns

partitionKneadVector :: [InflatedString] -> Bool
partitionKneadVector xs =
   let asns = setAssigns xs
   in  equivalentSolutions
         (ESC.partitions asns)
         (ESC_KneadSat.partitions asns)


tests :: [(String, (Int, QC.Property))]
tests =
   ("partitionKnead",
      forAllShrinkSmall genInflatedWords partitionKnead) :
   ("partitionKneadVector",
      forAllShrinkSmall genInflatedWords partitionKneadVector) :
   []
