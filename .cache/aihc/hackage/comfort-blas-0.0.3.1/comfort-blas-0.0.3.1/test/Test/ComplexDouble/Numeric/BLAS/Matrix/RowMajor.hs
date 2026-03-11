-- Do not edit! Automatically created with doctest-extract from src/Numeric/BLAS/Matrix/RowMajor.hs
{-# LINE 63 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}

module Test.ComplexDouble.Numeric.BLAS.Matrix.RowMajor where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 64 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
import     Test.ComplexDouble.Type (Number_)
import     Test.ComplexDouble.Numeric.BLAS.Vector (forVector, genVector, number_)
import     Test.Slice (ShapeInt, shapeInt)
import     qualified Numeric.BLAS.Matrix.RowMajor as Matrix
import     qualified Numeric.BLAS.Vector as Vector
import     qualified Numeric.Netlib.Class as Class
import     Numeric.BLAS.Scalar (RealOf)
import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Test.QuickCheck as QC

type     Matrix = Matrix.Matrix (Shape.ZeroBased Int) (Shape.ZeroBased Int)
type     Real_ = RealOf Number_

maxDim     :: Int
maxDim     = 10

forMatrix     ::
       (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
       QC.Gen a -> (Matrix a -> prop) -> QC.Property
forMatrix     genElem =
       QC.forAll
          (do height <- fmap shapeInt $ QC.choose (0,maxDim)
              width <- fmap shapeInt $ QC.choose (0,maxDim)
              genVector (height, width) genElem)

genIdentityTrans     ::
       (Shape.C sh, Class.Floating a) =>
       sh -> QC.Gen (Matrix.Transposable sh sh a)
genIdentityTrans     sh = do
       trans <- QC.arbitrary
       return $
          if trans
             then Matrix.transposed (Matrix.identity sh)
             else Matrix.nonTransposed (Matrix.identity sh)

transpose     ::
       (Shape.C height, Eq height, Shape.C width, Class.Floating a) =>
       Matrix.Matrix height width a -> Matrix.Matrix width height a
transpose     a =
       Matrix.multiplyTransposable
          (Matrix.transposed a)
          (Matrix.nonTransposed (Matrix.identity (Matrix.height a)))

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:129: "
{-# LINE 129 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 129 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
    Matrix.identity (Shape.ZeroBased 0) :: Matrix.Square (Shape.ZeroBased Int) Real_
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (ZeroBased {",WildCardChunk,LineChunk " 0},ZeroBased {",WildCardChunk,LineChunk " 0}) []"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:131: "
{-# LINE 131 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 131 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
    Matrix.identity (Shape.ZeroBased 3) :: Matrix.Square (Shape.ZeroBased Int) Real_
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (ZeroBased {",WildCardChunk,LineChunk " 3},ZeroBased {",WildCardChunk,LineChunk " 3}) [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:339: "
{-# LINE 339 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 339 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
    Matrix.multiplyVectorLeft (Array.vectorFromList [3,1,4]) (Array.fromList (Shape.ZeroBased (3::Int), Shape.Range 'a' 'b') [0,1,0,0,1,0::Real_])
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (Range {rangeFrom = 'a', rangeTo = 'b'}) [4.0,3.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:342: "
{-# LINE 342 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 342 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forVector number_ $ \xs ->
   Matrix.multiplyVectorLeft xs (Matrix.identity (Array.shape xs)) == xs
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:353: "
{-# LINE 353 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 353 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
    Matrix.multiplyVectorRight (Array.fromList (Shape.Range 'a' 'b', Shape.ZeroBased (3::Int)) [0,0,1,1,0,0]) (Array.vectorFromList [3,1,4::Real_])
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (Range {rangeFrom = 'a', rangeTo = 'b'}) [4.0,3.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:355: "
{-# LINE 355 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 355 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
    Matrix.multiplyVectorRight (Array.fromList (Shape.Range 'a' 'b', Shape.ZeroBased (3::Int)) [2,7,1,8,2,8]) (Array.vectorFromList [3,1,4::Real_])
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (Range {rangeFrom = 'a', rangeTo = 'b'}) [17.0,58.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:358: "
{-# LINE 358 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 358 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forVector number_ $ \xs ->
   Matrix.multiplyVectorRight (Matrix.identity (Array.shape xs)) xs == xs
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:363: "
{-# LINE 363 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 363 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (genVector (snd $ Array.shape a) number_) $ \x ->
   Matrix.singleColumn (Matrix.multiplyVectorRight a x)
   ==
   Matrix.multiply a (Matrix.singleColumn x)
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:371: "
{-# LINE 371 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 371 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (genVector (fst $ Array.shape a) number_) $ \x ->
   QC.forAll (genVector (snd $ Array.shape a) number_) $ \y ->
   Vector.dot x (Matrix.multiplyVectorRight a y)
   ==
   Vector.dot (Matrix.multiplyVectorLeft x a) y
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:380: "
{-# LINE 380 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 380 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (genVector (snd $ Array.shape a) number_) $ \x ->
   Matrix.multiplyVectorRight a x
   ==
   Matrix.multiplyVectorLeft x (transpose a)
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:458: "
{-# LINE 458 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 458 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
      
   Matrix.multiply
      (Array.fromList (shapeInt 2, shapeInt 2) [1000,100,10,1])
      (Array.fromList (shapeInt 2, shapeInt 3) [0..5::Real_])
  )
  [ExpectedLine [WildCardChunk,LineChunk " [300.0,1400.0,2500.0,3.0,14.0,25.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:465: "
{-# LINE 465 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 465 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
      Matrix.multiply (Matrix.identity (Matrix.height a)) a == a
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:469: "
{-# LINE 469 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 469 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
      Matrix.multiply a (Matrix.identity (Matrix.width a)) == a
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:473: "
{-# LINE 473 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 473 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   forMatrix number_ $ \c ->
   QC.forAll (genVector (Matrix.width a, Matrix.height c) number_) $ \b ->
      Matrix.multiply a (Matrix.multiply b c)
      ==
      Matrix.multiply (Matrix.multiply a b) c
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:488: "
{-# LINE 488 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 488 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (genIdentityTrans (Matrix.height a)) $ \eye ->
      a == Matrix.multiplyTransposable eye (Matrix.nonTransposed a)
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:493: "
{-# LINE 493 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 493 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (genIdentityTrans (Matrix.width a)) $ \eye ->
      a == Matrix.multiplyTransposable (Matrix.nonTransposed a) eye
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:498: "
{-# LINE 498 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 498 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (genIdentityTrans (Matrix.width a)) $ \leftEye ->
   QC.forAll (genIdentityTrans (Matrix.height a)) $ \rightEye ->
      Matrix.multiplyTransposable leftEye (Matrix.transposed a)
      ==
      Matrix.multiplyTransposable (Matrix.transposed a) rightEye
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:506: "
{-# LINE 506 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 506 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   forMatrix number_ $ \a ->
   QC.forAll (QC.choose (0,maxDim)) $ \n ->
   QC.forAll (genVector (Matrix.width a, shapeInt n) number_) $ \b ->
      transpose (Matrix.multiply a b)
      ==
      Matrix.multiplyTransposable (Matrix.transposed b) (Matrix.transposed a)
  )
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:564: "
{-# LINE 564 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 564 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
      
   Matrix.kronecker
      (Array.fromList (shapeInt 2, shapeInt 2) [0,1,-1,0::Real_])
      (Array.fromList (shapeInt 2, shapeInt 3) [1..6])
  )
  [ExpectedLine [WildCardChunk,LineChunk " [0.0,0.0,0.0,1.0,2.0,3.0,0.0,0.0,0.0,4.0,5.0,6.0,-1.0,-2.0,-3.0,0.0,0.0,0.0,-4.0,-5.0,-6.0,0.0,0.0,0.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:571: "
{-# LINE 571 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.example(
{-# LINE 571 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
      
   Matrix.kronecker
      (Array.fromList (shapeInt 2, shapeInt 2) [1,2,3,4::Real_])
      (Array.fromList (shapeInt 2, shapeInt 3) [1,2,4,8,16,32])
  )
  [ExpectedLine [WildCardChunk,LineChunk " [1.0,2.0,4.0,2.0,4.0,8.0,8.0,16.0,32.0,16.0,32.0,64.0,3.0,6.0,12.0,4.0,8.0,16.0,24.0,48.0,96.0,32.0,64.0,128.0]"]]
 DocTest.printPrefix "Numeric.BLAS.Matrix.RowMajor:578: "
{-# LINE 578 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
 DocTest.property(
{-# LINE 578 "src/Numeric/BLAS/Matrix/RowMajor.hs" #-}
        
   QC.forAll (QC.choose (0,5)) $ \m ->
   QC.forAll (QC.choose (0,5)) $ \n ->
      Matrix.kronecker
         (Matrix.identity (shapeInt m))
         (Matrix.identity (shapeInt n))
      ==
      (Matrix.identity (shapeInt m, shapeInt n)
         :: Matrix.Square (ShapeInt, ShapeInt) Number_)
  )
