-- Do not edit! Automatically created with doctest-extract from src/Numeric/BLAS/Vector/Slice.hs
{-# LINE 97 "src/Numeric/BLAS/Vector/Slice.hs" #-}

{-# OPTIONS_GHC -XTypeOperators #-}
{-# OPTIONS_GHC -XGADTs #-}
module Test.Double.Numeric.BLAS.Vector.Slice where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 100 "src/Numeric/BLAS/Vector/Slice.hs" #-}
import     qualified Test.Slice as TestSlice
import     Test.Double.Numeric.BLAS.Vector
       (maxElem, maxDim, genVector, number_, real_, complex_)
import     Test.Double.Type (Number_)
import     Test.Generator (genNumber)
import     Test.Utility (approx, approxReal)

import     qualified Numeric.BLAS.Matrix.RowMajor as Matrix
import     qualified Numeric.BLAS.Vector.Slice as VectorSlice
import     qualified Numeric.BLAS.Vector as Vector
import     qualified Numeric.BLAS.Slice as Slice
import     qualified Numeric.BLAS.Scalar as Scalar
import     qualified Numeric.Netlib.Class as Class
import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.List as List
import     Numeric.BLAS.Vector ((+++), (|+|))
import     Numeric.BLAS.Scalar (RealOf, absolute, realPart, minusOne)
import     Data.Array.Comfort.Shape ((::+)((::+)))
import     Data.Tuple.HT (mapPair)
import     Data.Complex (Complex)
import     Control.Applicative (liftA2)

import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((==>))

type     Real_ = RealOf Number_
type     Complex_ = Complex Real_

maxDim1     :: Int
maxDim1     = 10

type     ShapeInt = Shape.ZeroBased Int
type     Shape = ShapeInt ::+ (ShapeInt, ShapeInt) ::+ ShapeInt
type     Vector = Vector.Vector Shape
type     Sliced = VectorSlice.T Shape ShapeInt

genDim     :: QC.Gen Int
genDim     = QC.choose (0,maxDim)

genShapeDim     :: Int -> QC.Gen Shape
genShapeDim     numRows = do
       left <- fmap Shape.ZeroBased $ QC.choose (0,maxDim)
       right <- fmap Shape.ZeroBased $ QC.choose (0,maxDim)
       columns <- fmap Shape.ZeroBased $ QC.choose (1,maxDim1)
       return (left ::+ (Shape.ZeroBased numRows, columns) ::+ right)

genShape     :: QC.Gen Shape
genShape     = genShapeDim =<< QC.choose (0,maxDim1)

forAll_     :: (Show a) => QC.Gen a -> (a -> QC.Property) -> QC.Property
forAll_     = QC.forAll

isNonEmpty     :: Shape.C sh => VectorSlice.T shA sh a -> Bool
isNonEmpty     xs = Shape.size (VectorSlice.shape xs) > 0

takeColumn     ::
       (Shape.Indexed sh1, Shape.C sh, Shape.C sh0, Shape.C sh2) =>
       Shape.Index sh1 ->
       Vector.Vector (sh0 ::+ (sh, sh1) ::+ sh2) a ->
       VectorSlice.T (sh0 ::+ (sh, sh1) ::+ sh2) sh a
takeColumn     c =
       VectorSlice.slice (Slice.column c . Slice.left . Slice.right) .
       VectorSlice.fromVector

listFromSlice     ::
       (Shape.C sh, Class.Floating a) => VectorSlice.T shA sh a -> [a]
listFromSlice     = Vector.toList . VectorSlice.toVector

genSlicedDim     ::
       (Class.Floating a) =>
       Int -> QC.Gen a -> QC.Gen (Int, Vector a)
genSlicedDim     numRows genElem = do
       shape@(_::+(_rows,columns)::+_) <- genShapeDim numRows
       c <- QC.elements (Shape.indices columns)
       fmap ((,) c) $ genVector shape genElem

genSliced     ::
       (Class.Floating a) =>
       QC.Gen a -> QC.Gen (Int, Vector a)
genSliced     genElem = flip genSlicedDim genElem =<< genDim

shrinkSliced     ::
       (Shape.C sh0, Shape.Indexed sh1, QC.Arbitrary a, Class.Floating a) =>
       (Shape.Index sh1,
        Vector.Vector (ShapeInt ::+ (sh0, sh1) ::+ ShapeInt) a) ->
       [(Shape.Index sh1,
         Vector.Vector (ShapeInt ::+ (sh0, sh1) ::+ ShapeInt) a)]
shrinkSliced     (c,xs) =
       let xs0 = Vector.takeLeft xs in
       let xs1 = Vector.takeRight xs in
       let xs10 = Vector.takeLeft xs1 in
       let xs11 = Vector.takeRight xs1 in
       map (\(ysl,ysr) ->
             (c,
              Vector.autoFromList ysl +++ xs10 +++ Vector.autoFromList ysr)) $
       QC.shrink (Vector.toList xs0, Vector.toList xs11)

forSliced     ::
       (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
       QC.Gen a -> (Sliced a -> prop) -> QC.Property
forSliced     genElem prop =
       QC.forAllShrink (genSliced genElem) shrinkSliced
          (prop . uncurry takeColumn)

genSliced2     ::
       (Class.Floating a) =>
       QC.Gen a -> QC.Gen ((Int, Vector a), (Int, Vector a))
genSliced2     genElem = do
       dim <- genDim
       liftA2 (,) (genSlicedDim dim genElem) (genSlicedDim dim genElem)

forSliced2     ::
       (QC.Testable prop, Class.Floating a, Show a) =>
       QC.Gen a -> (Sliced a -> Sliced a -> prop) -> QC.Property
forSliced2     genElem prop =
       QC.forAll (genSliced2 genElem)
          (uncurry prop . mapPair (uncurry takeColumn, uncurry takeColumn))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:301: "
{-# LINE 301 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 301 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      QC.forAll genShape $ \shape@(_::+(_rows,columns)::+_) -> QC.forAll (QC.elements (Shape.indices columns)) $ \c -> QC.forAll (genVector shape number_) $ \xs -> VectorSlice.extract (Slice.column c . Slice.left . Slice.right) xs == Matrix.takeColumn c (Vector.takeLeft (Vector.takeRight (xs :: Vector Number_)))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:303: "
{-# LINE 303 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 303 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forAll_ (TestSlice.genShapeSelect 4 100) $ \(TestSlice.ShapeSelect sh select) -> QC.forAll (genVector sh number_) $ \xs -> case TestSlice.instantiate sh select of TestSlice.Extraction slice cut -> VectorSlice.extract slice xs == cut xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:323: "
{-# LINE 323 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 323 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      QC.forAll genShape $ \shape a -> VectorSlice.toVector (VectorSlice.replicate shape a) == Vector.constant shape (a :: Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:335: "
{-# LINE 335 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.example(
{-# LINE 335 "src/Numeric/BLAS/Vector/Slice.hs" #-}
    List.map realPart $ Vector.toList $ VectorSlice.append (VectorSlice.fromVector $ Vector.autoFromList [3,1,4,1]) (VectorSlice.replicate (Shape.ZeroBased (3::Int)) (0::Number_))
  )
  [ExpectedLine [LineChunk "[3.0,1.0,4.0,1.0,0.0,0.0,0.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:365: "
{-# LINE 365 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.example(
{-# LINE 365 "src/Numeric/BLAS/Vector/Slice.hs" #-}
    List.map realPart $ Vector.toList $ let matrix = Vector.fromList (Shape.ZeroBased (4::Int), Shape.ZeroBased (3::Int)) $ map (\x -> fromInteger x :: Number_) [0 ..] in VectorSlice.concat $ map (\k -> VectorSlice.chunk (VectorSlice.slice (Slice.column k) $ VectorSlice.fromVector matrix)) [0,1,2]
  )
  [ExpectedLine [LineChunk "[0.0,3.0,6.0,9.0,1.0,4.0,7.0,10.0,2.0,5.0,8.0,11.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:393: "
{-# LINE 393 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 393 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced2 number_ $ \xs ys -> VectorSlice.inner xs ys == Vector.dot (VectorSlice.conjugate xs) (VectorSlice.toVector ys)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:394: "
{-# LINE 394 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 394 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.inner xs xs == Scalar.fromReal (VectorSlice.norm2Squared xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:477: "
{-# LINE 477 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 477 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.sum xs == List.sum (listFromSlice xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:550: "
{-# LINE 550 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 550 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.normInf xs == List.maximum (0 : List.map absolute (listFromSlice xs))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:561: "
{-# LINE 561 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 561 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.normInf1 xs == List.maximum (0 : List.map Scalar.norm1 (listFromSlice xs))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:576: "
{-# LINE 576 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 576 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> isNonEmpty xs ==> let (xi,xm) = VectorSlice.argAbsMaximum xs in VectorSlice.access xs xi == xm
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:577: "
{-# LINE 577 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 577 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> isNonEmpty xs ==> let (_xi,xm) = VectorSlice.argAbsMaximum xs in List.all (\x -> absolute x <= absolute xm) $ listFromSlice xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:578: "
{-# LINE 578 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 578 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> forSliced number_ $ \ys -> isNonEmpty xs && isNonEmpty ys ==> let (_xi,xm) = VectorSlice.argAbsMaximum xs; (_yi,ym) = VectorSlice.argAbsMaximum ys; (zi,zm) = Vector.argAbsMaximum (VectorSlice.toVector xs +++ VectorSlice.toVector ys) in case zi of Left _ -> xm==zm && absolute xm >= absolute ym; Right _ -> ym==zm && absolute xm < absolute ym
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:625: "
{-# LINE 625 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 625 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.argAbsMaximum xs == VectorSlice.argAbs1Maximum xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:650: "
{-# LINE 650 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 650 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      QC.forAll genShape $ \sh@(_::+(_rows,columns)::+_) -> QC.forAll (QC.elements (Shape.indices columns)) $ \c -> QC.forAll (genVector sh $ genNumber 3) $ \xt -> let xs = takeColumn c xt in approx 1e-2 (VectorSlice.product xs) (List.product (listFromSlice (xs :: Sliced Number_)))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:661: "
{-# LINE 661 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 661 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.minimum xs == List.minimum (listFromSlice xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:662: "
{-# LINE 662 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 662 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.maximum xs == List.maximum (listFromSlice xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:678: "
{-# LINE 678 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 678 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.limits xs == Array.limits (VectorSlice.toVector xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:708: "
{-# LINE 708 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 708 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.negate xs == VectorSlice.scale minusOne xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:709: "
{-# LINE 709 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 709 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.scale 2 xs == VectorSlice.add xs xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:768: "
{-# LINE 768 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 768 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced2 number_ $ \xs ys -> VectorSlice.add xs ys == VectorSlice.add ys xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:769: "
{-# LINE 769 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 769 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced2 number_ $ \xs ys -> VectorSlice.toVector xs == VectorSlice.sub xs ys |+| VectorSlice.toVector ys
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:794: "
{-# LINE 794 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 794 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced number_ $ \xs -> VectorSlice.toVector xs == Vector.negate (VectorSlice.negate xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:807: "
{-# LINE 807 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 807 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      QC.forAll (genNumber maxElem) $ \d -> forSliced number_ $ \xs -> VectorSlice.toVector xs == Vector.raise (-d) (VectorSlice.raise d xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:824: "
{-# LINE 824 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 824 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced2 number_ $ \xs ys -> VectorSlice.mul xs ys == VectorSlice.mul ys xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:832: "
{-# LINE 832 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 832 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced2 number_ $ \xs ys -> VectorSlice.mulConj xs ys == Vector.mul (VectorSlice.conjugate xs) (VectorSlice.toVector ys)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector.Slice:956: "
{-# LINE 956 "src/Numeric/BLAS/Vector/Slice.hs" #-}
 DocTest.property(
{-# LINE 956 "src/Numeric/BLAS/Vector/Slice.hs" #-}
      forSliced complex_ $ \xs -> approxReal 1e-2 (VectorSlice.norm2 xs) $ let (xrs,xis) = VectorSlice.unzipComplex xs in sqrt $ VectorSlice.norm2Squared xrs + VectorSlice.norm2Squared xis
  )
