module Control.Concurrent.BFQueueSpec (spec) where

import Control.Concurrent.Async
import Control.Concurrent.BFQueue
import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic


prop_FillFlushNonBlocking :: [[Int]] -> Property
prop_FillFlushNonBlocking lss = monadicIO $ do
  ls <- run $ do
    q <- newBFQueue (fromIntegral (sum (map length lss)))
    mapConcurrently_ (foldMap (writeBFQueue q)) lss
    flushBFQueue q
  return (sort ls === sort (concat lss))

prop_FillAndBlockFlush :: Positive Int -> [Int] -> Int -> Property
prop_FillAndBlockFlush (Positive bound) ls oneExtra =
  bound < length ls ==> monadicIO $ do
    let (fillWith, leftOver) = splitAt bound ls
    run $ do
      q <- newBFQueue $ fromIntegral bound
      isSuccess' <- and <$> mapConcurrently (tryWriteBFQueue q) fillWith
      hasSpace <- or <$> mapConcurrently (tryWriteBFQueue q) leftOver
      len <- lengthBFQueue q
      eLs <- race (writeBFQueue q oneExtra >> flushBFQueue q) (flushBFQueue q)
      return $
        conjoin
          [ counterexample "Queue wasn't fully filled up" isSuccess'
          , counterexample "Left over was placed on the queue" (not hasSpace)
          , fromIntegral len === length fillWith
          , either
              (\o ->
                 o === [oneExtra] .||.
                 counterexample "Placed an element on the full queue concurrently" False)
              (\ls' -> sort ls' === sort fillWith)
              eLs
          ]

prop_FillReadTakeNonBlocking :: NonEmptyList Int -> Property
prop_FillReadTakeNonBlocking (NonEmpty xs) =
  monadicIO $
  run $ do
    let i = fromIntegral $ length xs - 1
    q <- newBFQueue (i + 1)
    mapM_ (writeBFQueue q) xs
    [x'] <- takeBFQueue 1 q
    xs' <- takeBFQueue i q
    isEmpty <- isEmptyBFQueue q
    return (head xs === x' .&&. tail xs === xs' .&&. counterexample "Queue is non-empty" isEmpty)

spec :: Spec
spec =
  describe "Fill+Flush" $ do
    it "FillFlushNonBlocking" $ property prop_FillFlushNonBlocking
    it "FillAndBlockFlush" $ property prop_FillAndBlockFlush
    it "FillReadTakeNonBlocking" $ property prop_FillReadTakeNonBlocking
