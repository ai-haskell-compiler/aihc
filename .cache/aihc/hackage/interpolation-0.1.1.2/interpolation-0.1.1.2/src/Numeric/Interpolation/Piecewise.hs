module Numeric.Interpolation.Piecewise (
   interpolate,
   interpolateConstantExt,
   ) where

import qualified Numeric.Interpolation.NodeList as Nodes
import qualified Numeric.Interpolation.Type as Type


{- $setup
>>> import qualified Numeric.Interpolation.Piecewise as Piecewise
>>> import qualified Numeric.Interpolation.NodeList as Nodes
>>> import qualified Numeric.Interpolation.Type as Type
>>>
>>> import qualified Data.List as List
>>> import qualified Data.Set as Set
>>> import Data.Array (accumArray, listArray)
>>> import Data.List.HT (lengthAtLeast)
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((==>))
>>>
>>> forAllSortedRatios ::
>>>    (QC.Testable prop) => ([Rational] -> Rational -> prop) -> QC.Property
>>> forAllSortedRatios f =
>>>    QC.forAll (fmap Set.toAscList QC.arbitrary) $ \nodeXs x ->
>>>       f (map fromInteger nodeXs) (fromInteger x)
>>>
>>> checkEq ::
>>>    (Ord x, Eq y, Num y) =>
>>>    Type.T x y ny -> [x] -> x -> Bool
>>> checkEq typ nodeXs x =
>>>    let ys =
>>>           map
>>>              (flip (Piecewise.interpolateConstantExt typ) x)
>>>              (Type.basisFunctions typ nodeXs)
>>>        bounds = (0, length ys - 1)
>>>    in  listArray bounds ys
>>>        ==
>>>        accumArray (flip const) 0 bounds
>>>           (Type.sampleBasisFunctions typ nodeXs x)
>>>
>>> quantile :: (Show a, Ord a, Fractional a) => [a] -> a -> a
>>> quantile [] _ = error "quantile: empty list"
>>> quantile [y] _ = y
>>> quantile ys x =
>>>    let len = fromIntegral (length ys - 1)
>>>    in Piecewise.interpolateConstantExt Type.linear
>>>          (Nodes.fromList $ zip (map (/ len) $ map fromInteger [0..]) $
>>>           List.sort ys)
>>>          x
-}


{- |
It is a checked error to interpolate outside of the range of nodes.

>>> Piecewise.interpolate Type.linear (Nodes.fromList [(0,0),(3,6),(5,10::Rational)]) 2
4 % 1
>>> Piecewise.interpolate Type.hermite1 (Nodes.fromList [(0,(0,0)),(3,(9,6)),(5,(25,10::Rational))]) 2
4 % 1
>>> Piecewise.interpolate Type.hermite1 (Nodes.fromList [(0,(1,-2)),(3,(4,4)),(5,(16,8::Rational))]) 2
1 % 1
-}
interpolate :: (Ord x) => Type.T x y ny -> Nodes.T x ny -> x -> y
interpolate typ ns x =
   case Nodes.lookup ns x of
      (Just p0, Just p1) -> Type.interpolatePiece typ p0 p1 x
      _ -> error "interpolate: argument outside range"

{- |
Outside the range of nodes the interpolation function
takes the value of the respective border.

prop> forAllSortedRatios $ checkEq Type.linear
prop> forAllSortedRatios $ checkEq Type.hermite1
prop> forAllSortedRatios $ \nodeXs x -> lengthAtLeast 4 nodeXs ==> checkEq Type.cubicLinear nodeXs x
prop> forAllSortedRatios $ \nodeXs x -> lengthAtLeast 4 nodeXs ==> checkEq Type.cubicParabola nodeXs x


Linear interpolation can be used to compute the median, a quartile
or any other quantile of a list of arbitrary numbers.

>>> quantile [2,5,3::Rational] 0.5
3 % 1
>>> quantile [2,5,3,7::Rational] 0.5
4 % 1
>>> quantile [2,5,3,7::Rational] 0.25
11 % 4

prop> \(QC.NonEmpty xs) -> quantile (xs::[Rational]) 0 == minimum xs
prop> \(QC.NonEmpty xs) -> quantile (xs::[Rational]) 1 == maximum xs
-}
interpolateConstantExt ::
   (Ord x) => Type.T x y ny -> Nodes.T x ny -> x -> y
interpolateConstantExt typ ns x =
   case Nodes.lookup ns x of
      (Just p0, Just p1) -> Type.interpolatePiece typ p0 p1 x
      (Just p, Nothing) -> Type.valueFromNode typ $ snd p
      (Nothing, Just p) -> Type.valueFromNode typ $ snd p
      (Nothing, Nothing) -> error "interpolateConstantExt: empty node list"
