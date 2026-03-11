-- Do not edit! Automatically created with doctest-extract from src/MathObj/Matrix.hs
{-# LINE 71 "src/MathObj/Matrix.hs" #-}

module Test.MathObj.Matrix where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 72 "src/MathObj/Matrix.hs" #-}
import     qualified MathObj.Matrix as Matrix
import     qualified Algebra.Ring as Ring
import     qualified Algebra.Laws as Laws
import     Test.NumericPrelude.Utility ((/\))
import     qualified Test.QuickCheck as QC
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base as P
import     Prelude ()

import     Control.Monad (replicateM, join)
import     Control.Applicative (liftA2)
import     Data.Function.HT (nest)

genDimension     :: QC.Gen Int
genDimension     = QC.choose (0,20)

genMatrixFor     :: (QC.Arbitrary a) => Int -> Int -> QC.Gen (Matrix.T a)
genMatrixFor     m n =
       fmap (Matrix.fromList m n) $ replicateM (m*n) QC.arbitrary

genMatrix     :: (QC.Arbitrary a) => QC.Gen (Matrix.T a)
genMatrix     = join $ liftA2 genMatrixFor genDimension genDimension

genIntMatrix     :: QC.Gen (Matrix.T Integer)
genIntMatrix     = genMatrix

genFactorMatrix     :: (QC.Arbitrary a) => Matrix.T a -> QC.Gen (Matrix.T a)
genFactorMatrix     a = genMatrixFor (Matrix.numColumns a) =<< genDimension

genSameMatrix     :: (QC.Arbitrary a) => Matrix.T a -> QC.Gen (Matrix.T a)
genSameMatrix     = uncurry genMatrixFor . Matrix.dimension

test :: DocTest.T ()
test = do
 DocTest.printPrefix "MathObj.Matrix:118: "
{-# LINE 118 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 118 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> Matrix.rows a == Matrix.columns (Matrix.transpose a))
 DocTest.printPrefix "MathObj.Matrix:119: "
{-# LINE 119 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 119 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> Matrix.columns a == Matrix.rows (Matrix.transpose a))
 DocTest.printPrefix "MathObj.Matrix:120: "
{-# LINE 120 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 120 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> genSameMatrix a /\ \b -> Laws.homomorphism Matrix.transpose (+) (+) a b)
 DocTest.printPrefix "MathObj.Matrix:141: "
{-# LINE 141 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 141 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> a == uncurry Matrix.fromRows (Matrix.dimension a) (Matrix.rows a))
 DocTest.printPrefix "MathObj.Matrix:152: "
{-# LINE 152 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 152 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> a == uncurry Matrix.fromColumns (Matrix.dimension a) (Matrix.columns a))
 DocTest.printPrefix "MathObj.Matrix:195: "
{-# LINE 195 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 195 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> genSameMatrix a /\ \b -> Laws.commutative (+) a b)
 DocTest.printPrefix "MathObj.Matrix:196: "
{-# LINE 196 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 196 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> genSameMatrix a /\ \b -> genSameMatrix b /\ \c -> Laws.associative (+) a b c)
 DocTest.printPrefix "MathObj.Matrix:212: "
{-# LINE 212 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 212 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> Laws.identity (+) (uncurry Matrix.zero $ Matrix.dimension a) a)
 DocTest.printPrefix "MathObj.Matrix:228: "
{-# LINE 228 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 228 "src/MathObj/Matrix.hs" #-}
     (genDimension /\ \n -> Matrix.one n == Matrix.diagonal (replicate n Ring.one :: [Integer]))
 DocTest.printPrefix "MathObj.Matrix:242: "
{-# LINE 242 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 242 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> Laws.leftIdentity  (*) (Matrix.one (Matrix.numRows a)) a)
 DocTest.printPrefix "MathObj.Matrix:243: "
{-# LINE 243 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 243 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> Laws.rightIdentity (*) (Matrix.one (Matrix.numColumns a)) a)
 DocTest.printPrefix "MathObj.Matrix:244: "
{-# LINE 244 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 244 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> genFactorMatrix a /\ \b -> Laws.homomorphism Matrix.transpose (*) (flip (*)) a b)
 DocTest.printPrefix "MathObj.Matrix:245: "
{-# LINE 245 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 245 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> genFactorMatrix a /\ \b -> genFactorMatrix b /\ \c -> Laws.associative (*) a b c)
 DocTest.printPrefix "MathObj.Matrix:246: "
{-# LINE 246 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 246 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \b -> genSameMatrix b /\ \c -> genFactorMatrix b /\ \a -> Laws.leftDistributive (*) (+) a b c)
 DocTest.printPrefix "MathObj.Matrix:247: "
{-# LINE 247 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 247 "src/MathObj/Matrix.hs" #-}
     (genIntMatrix /\ \a -> genFactorMatrix a /\ \b -> genSameMatrix b /\ \c -> Laws.rightDistributive (*) (+) a b c)
 DocTest.printPrefix "MathObj.Matrix:248: "
{-# LINE 248 "src/MathObj/Matrix.hs" #-}
 DocTest.property
{-# LINE 248 "src/MathObj/Matrix.hs" #-}
     (QC.choose (0,10) /\ \k -> genDimension /\ \n -> genMatrixFor n n /\ \a -> a^k == nest (fromInteger k) ((a::Matrix.T Integer)*) (Matrix.one n))
