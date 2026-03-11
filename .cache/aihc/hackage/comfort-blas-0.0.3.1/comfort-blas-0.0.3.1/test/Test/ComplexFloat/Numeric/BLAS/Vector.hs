-- Do not edit! Automatically created with doctest-extract from src/Numeric/BLAS/Vector.hs
{-# LINE 100 "src/Numeric/BLAS/Vector.hs" #-}

module Test.ComplexFloat.Numeric.BLAS.Vector where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 101 "src/Numeric/BLAS/Vector.hs" #-}
import     Test.ComplexFloat.Type (Number_)
import     Test.Generator (genNumber)
import     Test.Slice (shapeInt)
import     Test.Utility (approx)
import     qualified Numeric.BLAS.Matrix.RowMajor as Matrix
import     qualified Numeric.BLAS.Vector as Vector
import     qualified Numeric.Netlib.Class as Class
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.List as List
import     Numeric.BLAS.Vector ((+++), (|+|), (|-|))
import     Numeric.BLAS.Scalar (RealOf, absolute, minusOne)
import     Data.Array.Comfort.Storable (Array, (!))
import     Data.Complex (Complex((:+)))
import     Data.Monoid ((<>))
import     Data.Tuple.HT (mapPair)
import     Control.Applicative (liftA2)
import     Control.Monad (replicateM)

import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((==>))

type     Vector = Vector.Vector (Shape.ZeroBased Int)
type     Real_ = RealOf Number_
type     Complex_ = Complex Real_

maxElem     :: Integer
maxElem     = 10

maxDim     :: Int
maxDim     = 100

genVector     ::
       (Shape.C sh, Class.Floating a) =>
       sh -> QC.Gen a -> QC.Gen (Vector.Vector sh a)
genVector     shape genElem =
       fmap (Vector.fromList shape) $
       replicateM (Shape.size shape) genElem

real_     :: QC.Gen Real_
real_     = genNumber maxElem

complex_     :: QC.Gen Complex_
complex_     = genNumber maxElem

number_     :: QC.Gen Number_
number_     = genNumber maxElem

isNonEmpty     :: Shape.C sh => Array sh a -> Bool
isNonEmpty     xs = Shape.size (Array.shape xs) > 0

forVector     ::
       (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
       QC.Gen a -> (Vector a -> prop) -> QC.Property
forVector     genElem =
       QC.forAllShrink
          (flip genVector genElem . shapeInt =<< QC.choose (0,maxDim))
          (map Vector.autoFromList . QC.shrink . Vector.toList)

forVector2     ::
       (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
       QC.Gen a -> (Vector a -> Vector a -> prop) -> QC.Property
forVector2     genElem prop =
       QC.forAllShrink
          (do shape <- fmap shapeInt $ QC.choose (0,maxDim)
              liftA2 (,) (genVector shape genElem) (genVector shape genElem))
          (map (mapPair (Vector.autoFromList, Vector.autoFromList) . unzip) .
           QC.shrink .
           uncurry zip . mapPair (Vector.toList, Vector.toList))
          (uncurry prop)

type     CyclicVector = Vector.Vector (Shape.Cyclic Int)

genCyclicVector     ::
       (Class.Floating a) =>
       Integer -> Int -> QC.Gen (CyclicVector a)
genCyclicVector     maxE dim =
       fmap (Vector.fromList (Shape.Cyclic dim)) $
       replicateM dim $ genNumber maxE

cyclicVectorFromListGen     :: (Class.Floating a) => [a] -> CyclicVector a
cyclicVectorFromListGen     xs = Vector.fromList (Shape.Cyclic $ length xs) xs

cyclicVectorFromList     :: [Number_] -> CyclicVector Number_
cyclicVectorFromList     = cyclicVectorFromListGen

forCyclicVector     ::
       (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
       QC.Gen a -> (CyclicVector a -> prop) -> QC.Property
forCyclicVector     genElem =
       QC.forAllShrink
          (flip genVector genElem . Shape.Cyclic =<< QC.choose (0,maxDim))
          (map cyclicVectorFromListGen . QC.shrink . Vector.toList)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.BLAS.Vector:211: "
{-# LINE 211 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 211 "src/Numeric/BLAS/Vector.hs" #-}
      QC.forAll number_ $ \x -> Vector.constant () x == Vector.singleton x
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:213: "
{-# LINE 213 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 213 "src/Numeric/BLAS/Vector.hs" #-}
        
   QC.forAll (QC.choose (0,1000)) $ \n ->
   QC.forAll number_ $ \x ->
      Vector.sum (Vector.constant (shapeInt n) x) == fromIntegral n * x
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:225: "
{-# LINE 225 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 225 "src/Numeric/BLAS/Vector.hs" #-}
        
   QC.forAll (QC.choose (0,1000)) $ \n ->
   Vector.sum (Vector.zero (shapeInt n)) == (0 :: Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:234: "
{-# LINE 234 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 234 "src/Numeric/BLAS/Vector.hs" #-}
        
   QC.forAll (QC.choose (0,1000)) $ \n ->
   Vector.sum (Vector.one (shapeInt n)) == (fromIntegral n :: Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:243: "
{-# LINE 243 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 243 "src/Numeric/BLAS/Vector.hs" #-}
        
   QC.forAll (fmap shapeInt $ QC.choose (1,1000)) $ \sh ->
   QC.forAll (QC.elements $ Shape.indices sh) $ \k ->
   Vector.sum (Vector.unit sh k) == (1 :: Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:265: "
{-# LINE 265 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 265 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> forVector number_ $ \ys -> forVector number_ $ \zs -> Vector.toList ((xs +++ ys) +++ zs) == Vector.toList (xs +++ (ys +++ zs))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:274: "
{-# LINE 274 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 274 "src/Numeric/BLAS/Vector.hs" #-}
      Vector.autoFromList [] == (Vector.reverse $ Vector.autoFromList [] :: Vector Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:275: "
{-# LINE 275 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 275 "src/Numeric/BLAS/Vector.hs" #-}
      Vector.autoFromList [1] == (Vector.reverse $ Vector.autoFromList [1] :: Vector Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:276: "
{-# LINE 276 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 276 "src/Numeric/BLAS/Vector.hs" #-}
      Vector.autoFromList [3,2,1] == (Vector.reverse $ Vector.autoFromList [1,2,3] :: Vector Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:278: "
{-# LINE 278 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 278 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> reverse (Vector.toList xs) == Vector.toList (Vector.reverse xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:279: "
{-# LINE 279 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 279 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> xs == Vector.reverse (Vector.reverse xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:280: "
{-# LINE 280 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 280 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> forVector number_ $ \ys -> Vector.reverse (xs <> ys) == Vector.reverse ys <> Vector.reverse xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:295: "
{-# LINE 295 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 295 "src/Numeric/BLAS/Vector.hs" #-}
      cyclicVectorFromList [] == Vector.cyclicReverse (cyclicVectorFromList [])
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:296: "
{-# LINE 296 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 296 "src/Numeric/BLAS/Vector.hs" #-}
      cyclicVectorFromList [1] == Vector.cyclicReverse (cyclicVectorFromList [1])
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:297: "
{-# LINE 297 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 297 "src/Numeric/BLAS/Vector.hs" #-}
      cyclicVectorFromList [1,3,2] == Vector.cyclicReverse (cyclicVectorFromList [1,2,3])
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:298: "
{-# LINE 298 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 298 "src/Numeric/BLAS/Vector.hs" #-}
      cyclicVectorFromList [1,6,5,4,3,2] == Vector.cyclicReverse (cyclicVectorFromList [1,2,3,4,5,6])
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:300: "
{-# LINE 300 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 300 "src/Numeric/BLAS/Vector.hs" #-}
      forCyclicVector number_ $ \xs -> xs == Vector.cyclicReverse (Vector.cyclicReverse xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:317: "
{-# LINE 317 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 317 "src/Numeric/BLAS/Vector.hs" #-}
        
   QC.forAll (QC.choose (1,100)) $ \dim ->
   QC.forAll (QC.choose (0, dim-1)) $ \i ->
   QC.forAll (QC.choose (0, dim-1)) $ \j ->
      Vector.unit (Shape.ZeroBased dim) i
      ==
      (Vector.swap i j (Vector.unit (Shape.ZeroBased dim) j) :: Vector Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:344: "
{-# LINE 344 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 344 "src/Numeric/BLAS/Vector.hs" #-}
        
   forVector2 number_ $ \xs ys ->
      Vector.dot xs ys
      ==
      Matrix.multiply (Matrix.singleRow xs) (Matrix.singleColumn ys) ! ((),())
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:351: "
{-# LINE 351 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 351 "src/Numeric/BLAS/Vector.hs" #-}
        
   QC.forAll (QC.choose (1,100)) $ \dim ->
   QC.forAll (QC.choose (0, dim-1)) $ \i ->
   QC.forAll (QC.choose (0, dim-1)) $ \j ->
      Vector.dot
         (Vector.unit (shapeInt dim) i)
         (Vector.unit (shapeInt dim) j)
      ==
      (fromIntegral (fromEnum (i==j)) :: Number_)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:377: "
{-# LINE 377 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 377 "src/Numeric/BLAS/Vector.hs" #-}
      forVector2 number_ $ \xs ys -> Vector.inner xs ys == Vector.dot (Vector.conjugate xs) ys
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:430: "
{-# LINE 430 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 430 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> Vector.sum xs == List.sum (Vector.toList xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:490: "
{-# LINE 490 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 490 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> Vector.normInf xs == List.maximum (0 : List.map absolute (Vector.toList xs))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:514: "
{-# LINE 514 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.example(
{-# LINE 514 "src/Numeric/BLAS/Vector.hs" #-}
    Vector.argAbsMaximum $ Vector.autoFromList [1:+2, 3:+4, 5, 6 :: Complex_]
  )
  [ExpectedLine [LineChunk "(3,6.0 :+ 0.0)"]]
 DocTest.printPrefix "Numeric.BLAS.Vector:517: "
{-# LINE 517 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 517 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> isNonEmpty xs ==> let (xi,xm) = Vector.argAbsMaximum xs in xs!xi == xm
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:518: "
{-# LINE 518 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 518 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> isNonEmpty xs ==> let (_xi,xm) = Vector.argAbsMaximum xs in List.all (\x -> absolute x <= absolute xm) $ Vector.toList xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:519: "
{-# LINE 519 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 519 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> forVector number_ $ \ys -> isNonEmpty xs && isNonEmpty ys ==> let (_xi,xm) = Vector.argAbsMaximum xs; (_yi,ym) = Vector.argAbsMaximum ys; (zi,zm) = Vector.argAbsMaximum (xs+++ys) in case zi of Left _ -> xm==zm && absolute xm >= absolute ym; Right _ -> ym==zm && absolute xm < absolute ym
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:563: "
{-# LINE 563 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.example(
{-# LINE 563 "src/Numeric/BLAS/Vector.hs" #-}
    Vector.argAbs1Maximum $ Vector.autoFromList [1:+2, 3:+4, 5, 6 :: Complex_]
  )
  [ExpectedLine [LineChunk "(1,3.0 :+ 4.0)"]]
 DocTest.printPrefix "Numeric.BLAS.Vector:566: "
{-# LINE 566 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 566 "src/Numeric/BLAS/Vector.hs" #-}
      forVector real_ $ \xs -> isNonEmpty xs ==> Vector.argAbsMaximum xs == Vector.argAbs1Maximum xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:599: "
{-# LINE 599 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 599 "src/Numeric/BLAS/Vector.hs" #-}
      QC.forAll (QC.choose (0,10)) $ \dim -> QC.forAll (genVector (shapeInt dim) (genNumber 3)) $ \xs -> approx 1e-2 (Vector.product xs) (List.product (Vector.toList (xs :: Vector Number_)))
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:609: "
{-# LINE 609 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 609 "src/Numeric/BLAS/Vector.hs" #-}
      forVector real_ $ \xs -> isNonEmpty xs ==> Vector.minimum xs == List.minimum (Vector.toList xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:610: "
{-# LINE 610 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 610 "src/Numeric/BLAS/Vector.hs" #-}
      forVector real_ $ \xs -> isNonEmpty xs ==> Vector.maximum xs == List.maximum (Vector.toList xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:611: "
{-# LINE 611 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 611 "src/Numeric/BLAS/Vector.hs" #-}
      forVector real_ $ \xs -> isNonEmpty xs ==> - Vector.maximum xs == Vector.minimum (Vector.negate xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:627: "
{-# LINE 627 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 627 "src/Numeric/BLAS/Vector.hs" #-}
      forVector real_ $ \xs -> isNonEmpty xs ==> Vector.limits xs == Array.limits xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:657: "
{-# LINE 657 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 657 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> Vector.negate xs == Vector.scale minusOne xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:658: "
{-# LINE 658 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 658 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> Vector.scale 2 xs == xs |+| xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:727: "
{-# LINE 727 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 727 "src/Numeric/BLAS/Vector.hs" #-}
      forVector2 number_ $ \xs ys -> xs |+| ys == ys |+| xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:728: "
{-# LINE 728 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 728 "src/Numeric/BLAS/Vector.hs" #-}
      forVector2 number_ $ \xs ys -> xs == xs |-| ys |+| ys
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:758: "
{-# LINE 758 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 758 "src/Numeric/BLAS/Vector.hs" #-}
      forVector number_ $ \xs -> xs == Vector.negate (Vector.negate xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:771: "
{-# LINE 771 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 771 "src/Numeric/BLAS/Vector.hs" #-}
      QC.forAll (genNumber maxElem) $ \d -> forVector number_ $ \xs -> xs == Vector.raise (-d) (Vector.raise d xs)
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:788: "
{-# LINE 788 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 788 "src/Numeric/BLAS/Vector.hs" #-}
      forVector2 number_ $ \xs ys -> Vector.mul xs ys == Vector.mul ys xs
  )
 DocTest.printPrefix "Numeric.BLAS.Vector:796: "
{-# LINE 796 "src/Numeric/BLAS/Vector.hs" #-}
 DocTest.property(
{-# LINE 796 "src/Numeric/BLAS/Vector.hs" #-}
      forVector2 number_ $ \xs ys -> Vector.mulConj xs ys == Vector.mul (Vector.conjugate xs) ys
  )
