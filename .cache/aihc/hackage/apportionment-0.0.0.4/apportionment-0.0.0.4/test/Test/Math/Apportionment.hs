-- Do not edit! Automatically created with doctest-extract from src/Math/Apportionment.hs
{-# LINE 21 "src/Math/Apportionment.hs" #-}

module Test.Math.Apportionment where

import Math.Apportionment
import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 22 "src/Math/Apportionment.hs" #-}
import     Control.Applicative ((<$>))
import     qualified Test.QuickCheck as QC

forAllNonNegatives     ::
       (Num a, Ord a, Show a, QC.Arbitrary a) => ([a] -> Bool) -> QC.Property
forAllNonNegatives     = QC.forAll $
       (map QC.getNonNegative <$> QC.arbitrary) `QC.suchThat` (\xs -> sum xs > 0)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Math.Apportionment:67: "
{-# LINE 67 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 67 "src/Math/Apportionment.hs" #-}
     (\xs -> xs == largestRemainder (map fromIntegral xs :: [Rational]))
 DocTest.printPrefix "Math.Apportionment:68: "
{-# LINE 68 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 68 "src/Math/Apportionment.hs" #-}
     (forAllNonNegatives $ \xs -> round (sum xs) == sum (largestRemainder (xs :: [Rational])))
 DocTest.printPrefix "Math.Apportionment:62: "
{-# LINE 62 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 62 "src/Math/Apportionment.hs" #-}
   (largestRemainder [1,2,3::Rational])
  [ExpectedLine [LineChunk "[1,2,3]"]]
 DocTest.printPrefix "Math.Apportionment:64: "
{-# LINE 64 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 64 "src/Math/Apportionment.hs" #-}
   (largestRemainder [1.1,2.2,3.3,4.4::Rational])
  [ExpectedLine [LineChunk "[1,2,3,5]"]]
 DocTest.printPrefix "Math.Apportionment:85: "
{-# LINE 85 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 85 "src/Math/Apportionment.hs" #-}
     (forAllNonNegatives $ \xs -> xs == largestRemainderScaled (sum xs) (map fromIntegral xs :: [Rational]))
 DocTest.printPrefix "Math.Apportionment:86: "
{-# LINE 86 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 86 "src/Math/Apportionment.hs" #-}
     (\(QC.Positive s) -> forAllNonNegatives $ \xs -> s == sum (largestRemainderScaled s (xs :: [Rational])))
 DocTest.printPrefix "Math.Apportionment:77: "
{-# LINE 77 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 77 "src/Math/Apportionment.hs" #-}
   (largestRemainderScaled 100 [1,2,3::Rational])
  [ExpectedLine [LineChunk "[17,33,50]"]]
 DocTest.printPrefix "Math.Apportionment:82: "
{-# LINE 82 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 82 "src/Math/Apportionment.hs" #-}
   (largestRemainderScaled 100 [1,10,100::Rational])
  [ExpectedLine [LineChunk "[1,9,90]"]]
 DocTest.printPrefix "Math.Apportionment:135: "
{-# LINE 135 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 135 "src/Math/Apportionment.hs" #-}
     (forAllNonNegatives $ \xs -> xs == highestAveragesScaled dHondtDivisors (sum xs) (map fromIntegral xs :: [Rational]))
 DocTest.printPrefix "Math.Apportionment:136: "
{-# LINE 136 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 136 "src/Math/Apportionment.hs" #-}
     (forAllNonNegatives $ \xs -> xs == highestAveragesScaled sainteLagueDivisors (sum xs) (map fromIntegral xs :: [Rational]))
 DocTest.printPrefix "Math.Apportionment:138: "
{-# LINE 138 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 138 "src/Math/Apportionment.hs" #-}
     (\(QC.Positive s) -> forAllNonNegatives $ \xs -> s == sum (highestAveragesScaled dHondtDivisors s (xs :: [Rational])))
 DocTest.printPrefix "Math.Apportionment:139: "
{-# LINE 139 "src/Math/Apportionment.hs" #-}
 DocTest.property
{-# LINE 139 "src/Math/Apportionment.hs" #-}
     (\(QC.Positive s) -> forAllNonNegatives $ \xs -> s == sum (highestAveragesScaled sainteLagueDivisors s (xs :: [Rational])))
 DocTest.printPrefix "Math.Apportionment:125: "
{-# LINE 125 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 125 "src/Math/Apportionment.hs" #-}
   (highestAveragesScaled dHondtDivisors 100 [1,2,3::Rational])
  [ExpectedLine [LineChunk "[17,33,50]"]]
 DocTest.printPrefix "Math.Apportionment:127: "
{-# LINE 127 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 127 "src/Math/Apportionment.hs" #-}
   (highestAveragesScaled dHondtDivisors 100 [1,10,100::Rational])
  [ExpectedLine [LineChunk "[0,9,91]"]]
 DocTest.printPrefix "Math.Apportionment:130: "
{-# LINE 130 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 130 "src/Math/Apportionment.hs" #-}
   (highestAveragesScaled sainteLagueDivisors 100 [1,2,3::Rational])
  [ExpectedLine [LineChunk "[17,33,50]"]]
 DocTest.printPrefix "Math.Apportionment:132: "
{-# LINE 132 "src/Math/Apportionment.hs" #-}
 DocTest.example
{-# LINE 132 "src/Math/Apportionment.hs" #-}
   (highestAveragesScaled sainteLagueDivisors 100 [1,10,100::Rational])
  [ExpectedLine [LineChunk "[1,9,90]"]]
