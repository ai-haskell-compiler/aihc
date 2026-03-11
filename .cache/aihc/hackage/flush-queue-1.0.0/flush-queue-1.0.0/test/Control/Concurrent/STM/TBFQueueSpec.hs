module Control.Concurrent.STM.TBFQueueSpec (spec) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBFQueue
import Data.List

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_FillFlushNonBlocking :: [[Int]] -> Property
prop_FillFlushNonBlocking lss =
  monadicIO $ do
    ls <-
      run $ do
        q <- newTBFQueueIO (fromIntegral (sum (map length lss)))
        mapConcurrently_ (foldMap (atomically . writeTBFQueue q)) lss
        atomically $ flushTBFQueue q
    return (sort ls === sort (concat lss))

prop_FillAndBlockFlush :: Positive Int -> [Int] -> Int -> Property
prop_FillAndBlockFlush (Positive bound) ls oneExtra =
  bound < length ls ==> monadicIO $ do
    let (fillWith, leftOver) = splitAt bound ls
    run $ do
      q <- atomically $ newTBFQueue $ fromIntegral bound
      isSuccess' <- and <$> mapConcurrently (atomically . tryWriteTBFQueue q) fillWith
      hasSpace <- or <$> mapConcurrently (atomically . tryWriteTBFQueue q) leftOver
      len <- atomically $ lengthTBFQueue q
      eLs <- atomically $ orElse (Left <$> writeTBFQueue q oneExtra) (Right <$> flushTBFQueue q)
      return $
        conjoin
          [ counterexample "Queue wasn't fully filled up" isSuccess'
          , counterexample "Left over was placed on the queue" (not hasSpace)
          , fromIntegral len === length fillWith
          , either
              (\_ -> counterexample "Placed an element on the full queue" False)
              (\ls' -> sort ls' === sort fillWith)
              eLs
          ]

newFullQueueFromList :: Foldable t => t a -> IO (TBFQueue a)
newFullQueueFromList xs = do
    q <- newTBFQueueIO $ fromIntegral $ length xs
    mapM_ (atomically . writeTBFQueue q) xs
    pure q

prop_FillReadTakeNonBlocking :: NonEmptyList Int -> Property
prop_FillReadTakeNonBlocking (NonEmpty xs) =
  monadicIO $
  run $ do
    let i = fromIntegral (length xs - 1)
    q <- newFullQueueFromList xs
    x' <- atomically $ readTBFQueue q
    xs' <- atomically $ takeTBFQueue i q
    isEmpty <- atomically $ isEmptyTBFQueue q
    return (head xs === x' .&&. tail xs === xs' .&&. counterexample "Queue is non-empty" isEmpty)


prop_FillReadTakeBlocking :: NonEmptyList Int -> Int -> Property
prop_FillReadTakeBlocking (NonEmpty xs) y =
  monadicIO $
  run $ do
    let i = fromIntegral (length xs - 1)
    q <- newFullQueueFromList xs
    ((), x') <- concurrently (atomically $ writeTBFQueue q y) (atomically $ readTBFQueue q)
    xs' <- atomically $ takeTBFQueue i q
    y' <- atomically $ readTBFQueue q
    return (head xs === x' .&&. tail xs === xs' .&&. y === y')


prop_FillTakeNonBlocking :: [Int] -> NonNegative Int -> Property
prop_FillTakeNonBlocking xs (NonNegative i) =
  monadicIO $
  run $ do
    let n = length xs
    q <- newTBFQueueIO $ fromIntegral n
    mapM_ (atomically . writeTBFQueue q) xs
    xs1 <- atomically $ takeTBFQueue (fromIntegral i) q
    xs2 <- atomically $ takeTBFQueue (fromIntegral (max 0 (n - i))) q
    return (xs === xs1 ++ xs2)


prop_PushPopConcurrently1 :: Int -> Positive Int -> Property
prop_PushPopConcurrently1 x (Positive bound) =
  monadicIO $
  run $ do
    q <- newTBFQueueIO $ fromIntegral bound
    (x', ()) <- concurrently (atomically $ readTBFQueue q) (atomically $ writeTBFQueue q x)
    return (x === x')

prop_PushPopConcurrentlyMany :: [Int] -> Positive Int -> Property
prop_PushPopConcurrentlyMany xs (Positive bound) =
  monadicIO $
  run $ do
    q <- newTBFQueueIO $ fromIntegral bound
    (xs', ()) <-
      concurrently
        (mapM (const (atomically (readTBFQueue q))) xs)
        (mapM_ (atomically . writeTBFQueue q) xs)
    return (sort xs === sort xs')


spec :: Spec
spec =
  describe "Fill+Flush" $ do
    it "FillFlushNonBlocking" $ property prop_FillFlushNonBlocking
    it "FillAndBlockFlush" $ property prop_FillAndBlockFlush
    it "FillReadTakeNonBlocking" $ property prop_FillReadTakeNonBlocking
    it "FillReadTakeBlocking" $ property prop_FillReadTakeBlocking
    it "FillTakeNonBlocking" $ property prop_FillTakeNonBlocking
    it "PushPopConcurrently1" $ property prop_PushPopConcurrently1
    it "PushPopConcurrentlyMany" $ property prop_PushPopConcurrentlyMany
