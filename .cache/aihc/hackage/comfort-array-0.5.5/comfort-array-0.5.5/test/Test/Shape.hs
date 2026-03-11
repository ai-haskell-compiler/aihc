module Test.Shape where

import qualified Data.Array.Comfort.Shape.Test as ShapeTest
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Shape ((::+)((::+)))

import qualified Test.QuickCheck as QC
import Test.Utility (prefix)

import Control.Applicative (liftA2, liftA3, pure, (<$>))

import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tagged (Tagged(Tagged))


genZeroBased :: Int -> QC.Gen (Shape.ZeroBased Int)
genZeroBased n = fmap Shape.ZeroBased $ QC.choose (0,n)

tag :: sh -> Tagged Ordering sh
tag = Tagged

simplex ::
   (Shape.SimplexOrderC order) =>
   Shape.SimplexOrder order -> [(String, QC.Property)]
simplex order =
   prefix "Mixed"
      (ShapeTest.tests $
       liftA2 (Shape.Simplex order)
         (take 4 <$> QC.listOf (QC.elements [Shape.Distinct,Shape.Repetitive]))
         (genZeroBased 10)) ++
   prefix "Strict"
      (ShapeTest.tests $
       liftA2 (Shape.Simplex order)
         (take 4 <$> QC.listOf (pure Shape.AllDistinct))
         (genZeroBased 10)) ++
   prefix "Weak"
      (ShapeTest.tests $
       liftA2 (Shape.Simplex order)
         (take 4 <$> QC.listOf (pure Shape.SomeRepetitive))
         (genZeroBased 10)) ++
   []

tests :: [(String, QC.Property)]
tests =
   prefix "ZeroBased"
      (ShapeTest.tests $ genZeroBased 10) ++
   prefix "OneBased"
      (ShapeTest.tests $ fmap Shape.OneBased $ QC.choose (0,10::Int)) ++
   prefix "Range"
      (ShapeTest.tests $ do
         from <- QC.choose (0,10::Int)
         to <- QC.choose (from-1, 10)
         return $ Shape.Range from to) ++
   prefix "Shifted"
      (ShapeTest.tests $
       liftA2 Shape.Shifted
         (QC.choose (-10,10::Int)) (QC.choose (0,10::Int))) ++
   prefix "Enumeration Bool"
      (ShapeTest.tests $
       return (Shape.Enumeration :: Shape.Enumeration Bool)) ++
   prefix "Enumeration Ordering"
      (ShapeTest.tests $
       return (Shape.Enumeration :: Shape.Enumeration Ordering)) ++
   prefix "Set"
      (ShapeTest.tests $
       fmap Set.fromList (QC.listOf (QC.choose ('a','z')))) ++
   prefix "Map"
      (ShapeTest.tests $
       fmap Map.fromList
         (QC.listOf (liftA2 (,) (QC.choose ('a','z')) (genZeroBased 10)))) ++
   prefix "IntSet"
      (ShapeTest.tests $
       fmap IntSet.fromList (QC.listOf (QC.choose (1,10)))) ++
   prefix "IntMap"
      (ShapeTest.tests $
       fmap IntMap.fromList
         (QC.listOf (liftA2 (,) (QC.choose (1,10)) (genZeroBased 10)))) ++
   prefix "Deferred Shifted"
      (ShapeTest.tests $ fmap Shape.Deferred $
       liftA2 Shape.Shifted
         (QC.choose (-10,10::Int)) (QC.choose (0,10::Int))) ++
   prefix "Tagged Shifted"
      (ShapeTest.tests $ fmap tag $
       liftA2 Shape.Shifted
         (QC.choose (-10,10::Int)) (QC.choose (0,10::Int))) ++
   prefix "Pair"
      (ShapeTest.tests $ liftA2 (,) (genZeroBased 10) (genZeroBased 10)) ++
   prefix "Triple"
      (ShapeTest.tests $
       liftA3 (,,) (genZeroBased 10) (genZeroBased 10) (genZeroBased 10)) ++
   prefix "Append"
      (ShapeTest.tests $ liftA2 (::+) (genZeroBased 10) (genZeroBased 10)) ++
   prefix "Square"
      (ShapeTest.tests $ fmap Shape.Square $ genZeroBased 10) ++
   prefix "Cube"
      (ShapeTest.tests $ fmap Shape.Cube $ genZeroBased 10) ++
   prefix "Triangular Lower"
      (ShapeTest.tests $
       fmap (Shape.Triangular Shape.Lower) (genZeroBased 10)) ++
   prefix "Triangular Upper"
      (ShapeTest.tests $
       fmap (Shape.Triangular Shape.Upper) (genZeroBased 10)) ++
   prefix "Simplex"
      (prefix "Upper" (simplex Shape.Ascending) ++
       prefix "Lower" (simplex Shape.Descending)) ++
   prefix "Cyclic"
      (ShapeTest.tests $ fmap Shape.Cyclic $ QC.choose (0,10::Int)) ++
   []
