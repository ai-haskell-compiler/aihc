{-# LANGUAGE CPP #-}

module Matrix3 where

import qualified Math.ExpPairs.Matrix3 as M3
#ifdef MIN_VERSION_matrix
import Data.Foldable
import qualified Data.Matrix as M
#endif

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Instances ()

#ifdef MIN_VERSION_matrix
toM :: M3.Matrix3 a -> M.Matrix a
toM = M.fromList 3 3 . toList

toM3 :: M.Matrix a -> M3.Matrix3 a
toM3 = M3.fromList . toList

testOp :: (M3.Matrix3 Integer -> M3.Matrix3 Integer -> M3.Matrix3 Integer) -> (M.Matrix Integer -> M.Matrix Integer -> M.Matrix Integer) -> M3.Matrix3 Integer -> M3.Matrix3 Integer -> Bool
testOp op1 op2 m1 m2 = m'==m'' where
  m'  = toM $ m1 `op1` m2
  m'' = toM m1 `op2` toM m2

testDet1 :: M3.Matrix3 Integer -> Bool
testDet1 m = M3.det m == M.detLaplace (toM m)

testDet2 :: M3.Matrix3 Rational -> Bool
testDet2 m = M3.det m == M.detLU (toM m)

testConv :: M3.Matrix3 Integer -> Bool
testConv m = (toM3 . toM) m == m

testMultCol :: M3.Matrix3 Integer -> (Integer, Integer, Integer) -> Bool
testMultCol m v@(v1, v2, v3) = a==a' && b==b' && c==c' where
  (a, b, c) = M3.multCol m v
  [a', b', c'] = M.toList $ toM m * M.fromList 3 1 [v1, v2, v3]
#endif

testMakarov :: M3.Matrix3 Integer -> M3.Matrix3 Integer -> Bool
testMakarov m1 m2 = m1 * m2 == m1 `M3.makarovMult` m2

testLaderman :: M3.Matrix3 Integer -> M3.Matrix3 Integer -> Bool
testLaderman m1 m2 = m1 * m2 == m1 `M3.ladermanMult` m2

testRecip :: M3.Matrix3 Rational -> Bool
testRecip m = M3.det m==0 || m/=m' && m==m'' && M3.det m * M3.det m' == 1 where
  m' = recip m
  m'' = recip m'

testNormalize :: Integer -> M3.Matrix3 Integer -> Bool
testNormalize a m = (M3.normalize m' == m') && (a==0 || a>0 && m'==m'' || a<0 && m'==negate m'') where
  m' = M3.normalize m
  m'' = M3.normalize (m * fromInteger a)

testSuite :: TestTree
testSuite = testGroup "Matrix3"
  [ QC.testProperty "makarov"     testMakarov
  , QC.testProperty "laderman"    testLaderman
  , QC.testProperty "recip"       testRecip
  , QC.testProperty "normalize"   testNormalize
#ifdef MIN_VERSION_matrix
  , QC.testProperty "plus"      $ testOp (+) (+)
  , QC.testProperty "minus"     $ testOp (-) (-)
  , QC.testProperty "mult"      $ testOp (*) (*)
  , QC.testProperty "det1"        testDet1
  , QC.testProperty "det2"        testDet2
  , QC.testProperty "conversion"  testConv
  , QC.testProperty "mult column" testMultCol
#endif
  ]
