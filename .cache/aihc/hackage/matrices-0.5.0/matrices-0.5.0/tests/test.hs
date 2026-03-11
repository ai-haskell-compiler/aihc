import Test.Tasty
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Matrix.Class as C
import qualified Data.Matrix.Sparse.Generic as MS
import qualified Data.Vector.Unboxed as U
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "Main"
    [ testCase "xx" testEqual
    , subMatrixTest
    , subMatrixEqual
    ]

subMatrixEqual :: TestTree
subMatrixEqual = testCase "submatrix equal" $ assertEqual "x" mat submat
  where
    mat = MU.fromLists [[1]] :: MU.Matrix Int
    submat = MU.subMatrix (0,0) (0,0) (MU.fromLists [[1,2],[3,4]]) :: MU.Matrix Int

testEqual :: Assertion
testEqual = do
    let xs = [0,0,0,0,1,2,3,0,0,0,0,0,4,5,67,0,0,2,40,0,2,0,0,20,0,0,0]
        m1 = MU.fromList (3,9) xs
        al = filter ((/=0) . snd) $ MU.toList $ MU.imap (\i v -> (i,v)) m1
        m2 = C.fromList (3,9) xs :: MS.CSR U.Vector Int
        m3 = MS.fromAscAL (3,9) (length al) al :: MS.CSR U.Vector Int

        row1 = C.toRows m1
        row2 = C.toRows m2

--    assertEqual "x" (MG.flatten m1) (MG.flatten m2)
    assertEqual "x" (C.flatten m2) (C.flatten m3)
    assertEqual "x" row1 row2

subMatrixTest :: TestTree
subMatrixTest = testGroup "subMatrix"
    [ testCase "case 1" $ [[5,6], [8,9]] @=? MU.toLists sub1
    , testCase "case 2" $ [[5,6]] @=? MU.toLists sub2
    , testCase "case 3" $ [[5], [8]] @=? MU.toLists sub3
    , testCase "case 4" $ [[6], [9]] @=? MU.toLists sub4
    , testCase "case 5" $ [[9]] @=? MU.toLists sub5
    , testCase "case 6" $ [[5]] @=? MU.toLists sub6
    , testCase "case 7" $ [[8]] @=? MU.toLists sub7
    ]
  where
    ori = MU.fromLists [[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: MU.Matrix Int
    sub1 = MU.subMatrix (1,1) (2,2) ori
    sub2 = MU.subMatrix (0,0) (0,1) sub1
    sub3 = MU.subMatrix (0,0) (1,0) sub1
    sub4 = MU.subMatrix (0,1) (1,1) sub1
    sub5 = MU.subMatrix (1,1) (1,1) sub1
    sub6 = MU.subMatrix (0,0) (0,0) sub1
    sub7 = MU.subMatrix (1,0) (1,0) sub1
