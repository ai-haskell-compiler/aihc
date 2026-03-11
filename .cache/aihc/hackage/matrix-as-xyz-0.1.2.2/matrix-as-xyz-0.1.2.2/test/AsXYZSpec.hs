module AsXYZSpec where

import Control.Exception (evaluate)

import Test.Hspec
import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXYZ

readTest str mat = do
  it ("read " ++ str) $ do
    fromXYZ str `shouldBe` mat

curryM f (a,b) = do
  f a b

readData = [
  ("z,x,y",
    {- shouldBe -}
    fromLists [
      [0,0,1,0],
      [1,0,0,0],
      [0,1,0,0],
      [0,0,0,1]]),
  ("x+1,y+1,z+1",
    {- shouldBe -}
    fromLists [
      [1,0,0,1],
      [0,1,0,1],
      [0,0,1,1],
      [0,0,0,1]]),
  ("x-1,y-1,z-1",
    {- shouldBe -}
    fromLists [
      [1,0,0,-1],
      [0,1,0,-1],
      [0,0,1,-1],
      [0,0,0,1]]),
  ("x+1/2,y-2/3,z+3/4",
    {- shouldBe -}
    fromLists [
      [1,0,0, 1%2],
      [0,1,0,-2%3],
      [0,0,1, 3%4],
      [0,0,0,   1]]),
  ("x-5/8,y+7/16,z-9/32",
    {- shouldBe -}
    fromLists [
      [1,0,0,-5%8 ],
      [0,1,0, 7%16],
      [0,0,1,-9%32],
      [0,0,0,    1]]),
  ("1/2,1/2,1/2",
    {- shouldBe -}
    fromLists [
      [0,0,0,1%2],
      [0,0,0,1%2],
      [0,0,0,1%2],
      [0,0,0,  1]])
  ]

spec :: Spec
spec = do

   describe "Data.Matrix.AsXYZ.fromXYZ" $ do

     it "read empty throws exception" $ do
       evaluate (fromXYZ "") `shouldThrow` anyException

     it "read a,b throws exception" $ do
       evaluate (fromXYZ "a,b,c") `shouldThrow` anyException

     it "read x,y throws exception" $ do
       evaluate (fromABC "x,y,z") `shouldThrow` anyException

     it "read x,y,z" $ do
       fromXYZ "x,y,z" `shouldBe` (identity 4)

     it "read X,Y,Z" $ do
       fromXYZ "X,Y,Z"
        `shouldBe` (identity 4)
     
     mapM_ (curryM readTest) readData

     it "read a,b,c" $ do
       fromABC "a,b,c" `shouldBe` (identity 4)

     it "read A,B,C" $ do
       fromABC "A,B,C" `shouldBe` (identity 4)

     it "row size" $ do
       (nrows . fromXYZ $ "0,0,0") `shouldBe` 4

     it "col size" $ do
       (ncols . fromXYZ $ "0,0,0") `shouldBe` 4

   describe "Data.Matrix.AsXYZ.prettyXYZ" $ do

     it "show 0 (3x3)" $ do
       prettyXYZ (zero 3 3) `shouldBe` "0,0,0"

     it "show 0 (3x4)" $ do
       prettyXYZ (zero 3 4) `shouldBe` "0,0,0"

     it "show 0 (4x4)" $ do
       prettyXYZ (zero 4 4) `shouldBe` "0,0,0"

     it "show 1 (4x4)" $ do
       prettyXYZ (identity 4) `shouldBe` "x,y,z"

     it "show 1 (3x4)" $ do
       prettyXYZ (submatrix 1 3 1 4 $ identity 4) `shouldBe` "x,y,z"

     it "show 1 (3x3)" $ do
       prettyXYZ (submatrix 1 3 1 3 $ identity 4) `shouldBe` "x,y,z"

     it "integers" $ do
       prettyXYZ (fromLists [[1,2,3,4],[-5,-6,-7,-8],[9,10,11,12]])
        `shouldBe` "x+2y+3z+4,-5x-6y-7z-8,9x+10y+11z+12"

     it "rationals" $ do
       prettyXYZ (fromLists [[1%2,1%3,1%4,1%5],[2%3,2%5,2%7,2%9],[3%10,11%20,30%41,30%40]])
        `shouldBe` "1/2x+1/3y+1/4z+1/5,2/3x+2/5y+2/7z+2/9,3/10x+11/20y+30/41z+3/4"

     it "positive first" $ do
       prettyXYZ (fromLists [[1,-1,-1],[-1,1,-1],[-1,-1,1]])
        `shouldBe` "x-y-z,y-x-z,z-x-y"

     it "number last" $ do
       prettyXYZ (fromLists [[-1,-1,-1,-1],[-1,-1,-1,0],[-1,-1,-1,1]])
        `shouldBe` "-x-y-z-1,-x-y-z,-x-y-z+1"

   describe "Data.Matrix.AsXYZ.prettyABC" $ do

     it "show 0" $ do
       prettyABC (zero 4 4) `shouldBe` "0,0,0"

     it "show 1" $ do
       prettyABC (identity 4) `shouldBe` "a,b,c"
