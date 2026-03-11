{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Concurrent.Async
import Control.Concurrent.STMSupply
import Control.Monad
import Data.List
import Data.List.Unique
import GHC.Conc
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- Each split supply should generate distinct sequences of integers.
prop_idSplitSupply :: Positive Int -> Property
prop_idSplitSupply n = monadicIO $ do
  s   <- run newSTMSupplyIO
  s'  <- run . atomically $ splitSupply s
  ls1 <- run $ testImpl_idSequenceN n s
  ls2 <- run $ testImpl_idSequenceN n s'
  assert (satisfy n ls1 &&
          satisfy n ls2 &&
          ls1 /= ls2)

-- Spawn multiple async actions and have them each get a fresh ID.
-- The sorted IDs should all be distinct and sequential.
prop_idSequenceN :: Positive Int -> Property
prop_idSequenceN n = monadicIO $ do
  supply <- run newSTMSupplyIO
  ls     <- run $ testImpl_idSequenceN n supply
  assert $ satisfy n ls

-- Spawn multiple async actions and have them each get some fresh IDs
-- and split the supply. We want every ID retrieved to be unique.
-- We will spawn N async actions that each split the supply and then
-- take N IDs. We expect NxN unique integers.
prop_idSequenceSplitsN :: Positive Int -> Property
prop_idSequenceSplitsN p@(Positive n) = monadicIO $ do
  s  <- run newSTMSupplyIO
  ls <- run $ sortUniq . concat <$>
        (mapM wait =<< mapM (\_ -> async (asyncAction s)) [0..(n-1)])
  assert (length ls == (n * n))
  where
    asyncAction s = do
      threadDelay <$> randomDelay
      newSupply <- atomically $ splitSupply s
      atomically $ replicateM n (freshId newSupply)

-- testImpl_idSequence accepts an Int N and an STMSupply
-- and creates N async workers that will each atomically get a
-- freshID. We should produce a sorted list of integers L such that
-- L contains N distinct, sequential integers
testImpl_idSequenceN :: Positive Int -> STMSupply -> IO [Int]
testImpl_idSequenceN (Positive n) supply =
  sort <$> (mapM wait =<< mapM (\_ -> async asyncAction) [0..(n-1)])
  where
    asyncAction = do
      threadDelay <$> randomDelay
      atomically $ freshId supply

-- random delay of 350 - 700ms
randomDelay :: IO Int
randomDelay = (fst . randomR (350000, 700000)) <$> newStdGen

-- a list of integers satisfies the test condition if there
-- are N distinct integers and they are sequential.
satisfy :: Positive Int -> [Int] -> Bool
satisfy (Positive n) ls = length ls == n && isSequence ls
  where
    isSequence []       = True
    isSequence [x]      = True
    isSequence l@(x:xs) = all (==1) $ zipWith (-) xs l

return []
runTests = $quickCheckAll

main :: IO ()
main = void runTests
