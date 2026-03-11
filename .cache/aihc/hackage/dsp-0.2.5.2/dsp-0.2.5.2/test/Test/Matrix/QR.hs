module Test.Matrix.QR where

import qualified Matrix.QR.Householder as Householder
import qualified Matrix.QR.Givens as Givens
import qualified Matrix.LU as LU
import qualified Matrix.Vector as Vector
import qualified Matrix.Sparse as Sparse
import Matrix.Matrix (m_trans, mm_mult, mv_mult)
import DSP.Basic ((^!))

import Control.Applicative (liftA2, (<$>))

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import Data.Array

import qualified Test.QuickCheck as QC


doubleArray :: (Ix i) => Array i Int -> Array i Double
doubleArray = fmap fromIntegral

gramian :: Array (Int,Int) Double -> Double
gramian m = LU.det (m_trans m `mm_mult` m)

fullRank :: Array (Int,Int) Int -> Bool
fullRank m = round (gramian $ doubleArray m) /= (0::Integer)

arbitraryIntArray :: (Ix i) => (i,i) -> QC.Gen (Array i Int)
arbitraryIntArray bnds =
   fmap (listArray bnds) $ QC.vectorOf (rangeSize bnds) $ QC.choose (-10,10)

genMatrix :: QC.Gen (Array (Int,Int) Int)
genMatrix = do
   m <- QC.choose (0,5)
   n <- QC.choose (0,m)
   arbitraryIntArray ((1,1),(m,n))

genForward :: QC.Gen (Array (Int,Int) Int, Array Int Int)
genForward = do
   a <- genMatrix `QC.suchThat` fullRank
   let ((_m0,n0), (_m1,n1)) = bounds a
   x <- arbitraryIntArray (n0,n1)
   return (a,x)

genInverse :: QC.Gen (Array (Int,Int) Int, Array Int Int)
genInverse = do
   a <- genMatrix `QC.suchThat` fullRank
   let ((m0,_n0), (m1,_n1)) = bounds a
   b <- arbitraryIntArray (m0,m1)
   return (a,b)


arbitraryIntSparse ::
   (Ix i, Ix j) => ((i, j), (i, j)) -> QC.Gen (Sparse.Matrix i j Int)
arbitraryIntSparse bnds =
   fmap
      (Sparse.fromMap bnds . fmap fst .
       Map.filter snd . Map.fromList . zip (range bnds)) $
      QC.vectorOf (rangeSize bnds) $
      liftA2 (,) (QC.choose (-10,10)) QC.arbitrary

genSparse :: QC.Gen (Sparse.Matrix Int Int Int)
genSparse = do
   m <- QC.choose (0,5)
   n <- QC.choose (0,m)
   arbitraryIntSparse ((1,1),(m,n))

genSparseInverse :: QC.Gen (Sparse.Matrix Int Int Int, Array Int Int)
genSparseInverse = do
   a <- genSparse `QC.suchThat` (fullRank . Sparse.toDense)
   let ((m0,_n0), (m1,_n1)) = Sparse.bounds a
   b <- arbitraryIntArray (m0,m1)
   return (a,b)

genSquare :: QC.Gen (Array (Int,Int) Int)
genSquare = do
   m <- QC.choose (0,5)
   arbitraryIntArray ((1,1),(m,m))

genSparseSquare :: QC.Gen (Sparse.Matrix Int Int Int)
genSparseSquare = do
   m <- QC.choose (0,5)
   arbitraryIntSparse ((1,1),(m,m))



approx :: (Fractional a, Ord a) => a -> a -> Bool
approx x y  =  abs (x-y) <= 1e-5 * max 1 (abs x + abs y)

maxNorm :: (Num a, Ord a, Ix i) => Array i a -> a
maxNorm = Fold.foldl max 0 . fmap abs

approxAbsVector :: (Fractional a, Ord a, Ix i) => Array i a -> Array i a -> Bool
approxAbsVector x y = maxNorm (Vector.sub x y) <= 1e-5



solveHouseholder :: QC.Property
solveHouseholder =
   QC.forAll genForward $ \(a,x) ->
      let b = mv_mult a x
      in  x ==
          fmap round (Householder.leastSquares (doubleArray a) (doubleArray b))

solveGivens :: QC.Property
solveGivens =
   QC.forAll genForward $ \(a,x) ->
      let b = mv_mult a x
      in  x ==
          fmap round
            (Givens.leastSquares
               (Sparse.fromDense $ doubleArray a) (doubleArray b))


leastSquares :: QC.Property
leastSquares =
   QC.forAll genInverse $ \(a,b) ->
      Householder.leastSquares (doubleArray a) (doubleArray b)
      `approxAbsVector`
      Givens.leastSquares (Sparse.fromDense $ doubleArray a) (doubleArray b)

leastSquaresSparse :: QC.Property
leastSquaresSparse =
   QC.forAll genSparseInverse $ \(a,b) ->
      Householder.leastSquares (doubleArray $ Sparse.toDense a) (doubleArray b)
      `approxAbsVector`
      Givens.leastSquares (fmap fromIntegral a) (doubleArray b)



gramianHouseholder :: QC.Property
gramianHouseholder =
   QC.forAll (fmap doubleArray genMatrix) $ \a ->
      gramian a `approx` (Householder.detAbs a ^! 2)

gramianGivens :: QC.Property
gramianGivens =
   QC.forAll (fmap fromIntegral <$> genSparse) $ \a ->
      gramian (Sparse.toDense a)  `approx`  (Givens.detAbs a ^! 2)

detHouseholder :: QC.Property
detHouseholder =
   QC.forAll (fmap doubleArray genSquare) $ \a ->
      LU.det a `approx` Householder.det a

detGivens :: QC.Property
detGivens =
   QC.forAll (fmap fromIntegral <$> genSparseSquare) $ \a ->
      LU.det (Sparse.toDense a)  `approx`  Givens.det a


longCheck :: QC.Property -> IO ()
longCheck =
   QC.quickCheckWith (QC.stdArgs {QC.maxSuccess=10000})

tests :: [(String, IO ())]
tests =
   ("solveHouseholder", longCheck solveHouseholder) :
   ("solveGivens", longCheck solveGivens) :
   ("leastSquares", longCheck leastSquares) :
   ("leastSquaresSparse", longCheck leastSquaresSparse) :
   ("gramianHouseholder", longCheck gramianHouseholder) :
   ("gramianGivens", longCheck gramianGivens) :
   ("detHouseholder", longCheck detHouseholder) :
   ("detGivens", longCheck detGivens) :
   []
