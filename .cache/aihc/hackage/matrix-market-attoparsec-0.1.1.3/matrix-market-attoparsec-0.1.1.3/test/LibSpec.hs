module LibSpec where

import System.Directory (removeFile)

import Test.Hspec
-- import Test.Hspec.QuickCheck

import Data.Matrix.MatrixMarket



main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Data.Matrix.MatrixMarket" $ do
    -- prop "ourAdd is commutative" $ \x y ->
    --   ourAdd x y `shouldBe` ourAdd y x
    it "fidapm05 : read/write/read roundtrip" $ 
      roundTrip Coordinate "fidapm05.mtx" -- fidapm05_rhs1.mtx
    it "fidapm05_rhs1 : read/write/read roundtrip" $ 
      roundTrip Array "fidapm05_rhs1.mtx"
    -- it "memplus : read/write/read roundtrip" $ do 
    --   roundTrip "memplus.mtx"
    -- it "memplus_rhs1 : imports all array entries" $ do 
    --   x <- readArray "data/memplus_rhs1.mtx"
    --   consistentDimsArr x `shouldBe` True    


-- | Helpers

-- | Check if matrix dimensions (read from header vs counted from memory) coincide
-- NB : only valid for General matrices (in MatrixMarket, symmetric entries are not written on file)
consistentDims :: Matrix t -> Bool
consistentDims m = nnz m == numDat m

-- check if array dimensions (read from header vs counted from memory) coincide
consistentDimsArr :: Array a -> Bool
consistentDimsArr mm = d == numDatArr mm where
  (m,n) = dimArr mm
  d = m*n

dataDir :: String
dataDir = "data"

roundTrip :: Format -> FilePath -> IO ()
roundTrip f fname0 = do
  let
    fname = dataDir ++ "/" ++ fname0
    ftemp = fname ++ "_temp"
  if f == Coordinate
    then do   
    m0 <- readMatrix fname   -- load original
    writeMatrix ftemp m0     -- save as temp
    m1 <- readMatrix ftemp   -- load temp
    m0 `shouldBe` m1
    else do
    m0 <- readArray fname   -- load original
    writeArray ftemp m0     -- save as temp
    m1 <- readArray ftemp   -- load temp
    m0 `shouldBe` m1
  removeFile ftemp         -- remove temp 


-- withTempFile :: IO FilePath -> (FilePath -> IO c) -> IO c
-- withTempFile createf = bracket createf removeFile

