-- Do not edit! Automatically created with doctest-extract from src/Numeric/Interpolation/Piecewise.hs
{-# LINE 10 "src/Numeric/Interpolation/Piecewise.hs" #-}

module Test.Numeric.Interpolation.Piecewise where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 11 "src/Numeric/Interpolation/Piecewise.hs" #-}
import     qualified Numeric.Interpolation.Piecewise as Piecewise
import     qualified Numeric.Interpolation.NodeList as Nodes
import     qualified Numeric.Interpolation.Type as Type

import     qualified Data.List as List
import     qualified Data.Set as Set
import     Data.Array (accumArray, listArray)
import     Data.List.HT (lengthAtLeast)

import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((==>))

forAllSortedRatios     ::
       (QC.Testable prop) => ([Rational] -> Rational -> prop) -> QC.Property
forAllSortedRatios     f =
       QC.forAll (fmap Set.toAscList QC.arbitrary) $ \nodeXs x ->
          f (map fromInteger nodeXs) (fromInteger x)

checkEq     ::
       (Ord x, Eq y, Num y) =>
       Type.T x y ny -> [x] -> x -> Bool
checkEq     typ nodeXs x =
       let ys =
              map
                 (flip (Piecewise.interpolateConstantExt typ) x)
                 (Type.basisFunctions typ nodeXs)
           bounds = (0, length ys - 1)
       in  listArray bounds ys
           ==
           accumArray (flip const) 0 bounds
              (Type.sampleBasisFunctions typ nodeXs x)

quantile     :: (Show a, Ord a, Fractional a) => [a] -> a -> a
quantile     [] _ = error "quantile: empty list"
quantile     [y] _ = y
quantile     ys x =
       let len = fromIntegral (length ys - 1)
       in Piecewise.interpolateConstantExt Type.linear
             (Nodes.fromList $ zip (map (/ len) $ map fromInteger [0..]) $
              List.sort ys)
             x

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:58: "
{-# LINE 58 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.example
{-# LINE 58 "src/Numeric/Interpolation/Piecewise.hs" #-}
   (Piecewise.interpolate Type.linear (Nodes.fromList [(0,0),(3,6),(5,10::Rational)]) 2)
  [ExpectedLine [LineChunk "4 % 1"]]
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:60: "
{-# LINE 60 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.example
{-# LINE 60 "src/Numeric/Interpolation/Piecewise.hs" #-}
   (Piecewise.interpolate Type.hermite1 (Nodes.fromList [(0,(0,0)),(3,(9,6)),(5,(25,10::Rational))]) 2)
  [ExpectedLine [LineChunk "4 % 1"]]
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:62: "
{-# LINE 62 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.example
{-# LINE 62 "src/Numeric/Interpolation/Piecewise.hs" #-}
   (Piecewise.interpolate Type.hermite1 (Nodes.fromList [(0,(1,-2)),(3,(4,4)),(5,(16,8::Rational))]) 2)
  [ExpectedLine [LineChunk "1 % 1"]]
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:75: "
{-# LINE 75 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.property
{-# LINE 75 "src/Numeric/Interpolation/Piecewise.hs" #-}
     (forAllSortedRatios $ checkEq Type.linear)
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:76: "
{-# LINE 76 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.property
{-# LINE 76 "src/Numeric/Interpolation/Piecewise.hs" #-}
     (forAllSortedRatios $ checkEq Type.hermite1)
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:77: "
{-# LINE 77 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.property
{-# LINE 77 "src/Numeric/Interpolation/Piecewise.hs" #-}
     (forAllSortedRatios $ \nodeXs x -> lengthAtLeast 4 nodeXs ==> checkEq Type.cubicLinear nodeXs x)
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:78: "
{-# LINE 78 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.property
{-# LINE 78 "src/Numeric/Interpolation/Piecewise.hs" #-}
     (forAllSortedRatios $ \nodeXs x -> lengthAtLeast 4 nodeXs ==> checkEq Type.cubicParabola nodeXs x)
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:91: "
{-# LINE 91 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.property
{-# LINE 91 "src/Numeric/Interpolation/Piecewise.hs" #-}
     (\(QC.NonEmpty xs) -> quantile (xs::[Rational]) 0 == minimum xs)
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:92: "
{-# LINE 92 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.property
{-# LINE 92 "src/Numeric/Interpolation/Piecewise.hs" #-}
     (\(QC.NonEmpty xs) -> quantile (xs::[Rational]) 1 == maximum xs)
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:84: "
{-# LINE 84 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.example
{-# LINE 84 "src/Numeric/Interpolation/Piecewise.hs" #-}
   (quantile [2,5,3::Rational] 0.5)
  [ExpectedLine [LineChunk "3 % 1"]]
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:86: "
{-# LINE 86 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.example
{-# LINE 86 "src/Numeric/Interpolation/Piecewise.hs" #-}
   (quantile [2,5,3,7::Rational] 0.5)
  [ExpectedLine [LineChunk "4 % 1"]]
 DocTest.printPrefix "Numeric.Interpolation.Piecewise:88: "
{-# LINE 88 "src/Numeric/Interpolation/Piecewise.hs" #-}
 DocTest.example
{-# LINE 88 "src/Numeric/Interpolation/Piecewise.hs" #-}
   (quantile [2,5,3,7::Rational] 0.25)
  [ExpectedLine [LineChunk "11 % 4"]]
