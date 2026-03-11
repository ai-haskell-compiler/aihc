module DoubleBufferSpec where

import Control.Monad
import DataSketches.Quantiles.RelativeErrorQuantile.Internal.DoubleBuffer
import DataSketches.Quantiles.RelativeErrorQuantile.Types
import qualified Data.Vector.Unboxed as UV
import Test.Hspec
import qualified Data.List

spec :: Spec
spec = do
  describe "Merge Sort" $ do
    let mergeSortTestImpl direction = do
          buf <- mkBuffer 16 0 direction
          forM [0..31] (append buf)
          buf2 <- copyBuffer buf
          mergeSortIn buf buf2
          v <- getVector buf
          UV.freeze v
    it "works upwards" $ do
      v <- mergeSortTestImpl False
      v `shouldBe` UV.fromList (Data.List.sort ([0..31] ++ [0..31]))
    it "works downwards" $ do
      v <- mergeSortTestImpl True
      v `shouldBe` UV.fromList (Data.List.sort ([0..31] ++ [0..31]))
  describe "getEvensOrOdds" $ do
    checkGetEvensOrOdds False False
    checkGetEvensOrOdds False True
    checkGetEvensOrOdds True False
    checkGetEvensOrOdds True True
  describe "checkAppendAndSpaceTop" $ do
    checkAppendAndSpace True
    checkAppendAndSpace False
  describe "checkEnsureCapacity" $ do
    mapM_ checkEnsureCapacity [True, False]
  describe "checkCountLessThanImpl" $ do
    mapM_ checkCountLessThanImpl [True, False]

checkGetEvensOrOdds :: Bool -> Bool -> Spec
checkGetEvensOrOdds odds spaceAtBottom = 
  it ("works for odds=" ++ show odds ++ " spaceAtBottom=" ++ show spaceAtBottom) $ do
    buf <- mkBuffer cap 0 spaceAtBottom
    forM_ [0..cap `div` 2] (append buf . fromIntegral)
    out <- getEvensOrOdds buf 0 (cap `div` 2) odds
    v <- UV.freeze =<< getVector out
    v `shouldSatisfy` UV.all (\x -> if odds then floor x `mod` 2 == 1 else floor x `mod` 2 == 0)
  where
    cap = 16

checkAppendAndSpace :: Bool -> Spec
checkAppendAndSpace spaceAtBottom = it ("works for spaceAtBottom=" ++ show spaceAtBottom) $ do
  buf <- mkBuffer 2 2 spaceAtBottom
  do
    count_ <- getCount buf
    count_ `shouldBe` 0
    capacity_ <- getCapacity buf 
    capacity_ `shouldBe` 2
    space_ <- getSpace buf
    space_ `shouldBe` 2
  append buf 1
  do
    count_ <- getCount buf
    count_ `shouldBe` 1
    capacity_ <- getCapacity buf 
    capacity_ `shouldBe` 2
    space_ <- getSpace buf
    space_ `shouldBe` 1
  append buf 2
  do
    count_ <- getCount buf
    count_ `shouldBe` 2
    capacity_ <- getCapacity buf 
    capacity_ `shouldBe` 2
    space_ <- getSpace buf
    space_ `shouldBe` 0
  append buf 3
  do
    count_ <- getCount buf
    count_ `shouldBe` 3
    capacity_ <- getCapacity buf 
    capacity_ `shouldBe` 5
    space_ <- getSpace buf
    space_ `shouldBe` 2

checkEnsureCapacity :: Bool -> Spec
checkEnsureCapacity spaceAtBottom = it ("works for spaceAtBottom=" ++ show spaceAtBottom) $ do
  buf <- mkBuffer 4 2 spaceAtBottom
  append buf 2
  append buf 1
  append buf 3
  ensureCapacity buf 8
  sort buf
  x1 <- buf ! 0
  x1 `shouldBe` 1
  x2 <- buf ! 1
  x2 `shouldBe` 2
  x3 <- buf ! 2
  x3 `shouldBe` 3

checkCountLessThanImpl :: Bool -> Spec
checkCountLessThanImpl spaceAtBottom = it ("works for spaceAtBottom=" ++ show spaceAtBottom) $ do 
  buf <- mkBuffer 7 0 spaceAtBottom
  mapM_ (append buf) [1..7]
  sort buf
  buf2 <- mkBuffer 7 0 spaceAtBottom
  mergeSortIn buf2 buf
  do
    count_ <- getCountWithCriterion buf2 4 (:<)
    count_ `shouldBe` 3
  mergeSortIn buf2 buf
  do
    count_ <- getCountWithCriterion buf2 4 (:<)
    count_ `shouldBe` 6
  do
    count_ <- getCount buf2
    count_ `shouldBe` 14