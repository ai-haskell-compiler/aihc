module Data.Conduit.FoldDebounceSpec
    ( main
    , spec
    ) where

import           Test.Hspec

import           Control.Applicative          ((<$>))
import           Control.Concurrent           (myThreadId, threadDelay)
import           Control.Concurrent.STM       (TVar, atomically, newTVarIO, readTVar, retry,
                                               writeTVar)
import           Control.Monad                (forM_, void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Resource (ResourceT, register, runResourceT)
import           Data.Conduit                 (ConduitT, bracketP, runConduit, yield, (.|))
import qualified Data.Conduit.FoldDebounce    as F
import qualified Data.Conduit.List            as CL
import           Data.Maybe                   (isJust)
import           Data.Monoid                  (Monoid)
import           System.Timeout               (timeout)

main :: IO ()
main = hspec spec

delayedSource :: [(Int, a)] -> ConduitT () a (ResourceT IO) ()
delayedSource [] = return ()
delayedSource ((delay, item):rest) = do
  liftIO $ threadDelay delay
  yield item
  delayedSource rest

periodicSource :: Int -> [a] -> ConduitT () a (ResourceT IO) ()
periodicSource interval items = delayedSource $ zip (repeat interval) items

terminationDetector :: IO (TVar Bool, (ConduitT i o (ResourceT IO) r -> ConduitT i o (ResourceT IO) r))
terminationDetector = do
  terminated <- newTVarIO False
  return (terminated, makeDetector terminated)
  where
    makeDetector terminated orig = bracketP initCompletion setTermination runAction
      where
        initCompletion = newTVarIO False
        runAction v_completed = do
          ret <- orig
          liftIO $ atomically $ writeTVar v_completed True
          return ret
        setTermination v_completed = atomically $ do
          completed <- readTVar v_completed
          writeTVar terminated $ not completed

attachResource :: ConduitT () a (ResourceT IO) () -> IO (TVar Bool, ConduitT () a (ResourceT IO) ())
attachResource src = do
  released <- newTVarIO False
  let src' = do
        void $ register $ atomically $ writeTVar released True
        src
  return (released, src')

debSum :: Int -> ConduitT () Int (ResourceT IO) () -> ConduitT () Int (ResourceT IO) ()
debSum delay = F.debounce F.Args { F.init = 0, F.fold = (+), F.cb = undefined } F.def { F.delay = delay }

debMonoid :: Monoid i => Int -> ConduitT () i (ResourceT IO) () -> ConduitT () i (ResourceT IO) ()
debMonoid delay = F.debounce F.forMonoid F.def { F.delay = delay }

shouldSatisfyEventually :: Int -> TVar a -> (a -> Bool) -> IO ()
shouldSatisfyEventually timeout_usec got predicate = do
  eventually_got <- timeout timeout_usec $ atomically $ doCheck
  fmap predicate eventually_got `shouldBe` Just True
  where
    doCheck = do
      ret <- readTVar got
      if predicate ret then return ret else retry

shouldSatisfyEventually' :: TVar a -> (a -> Bool) -> IO ()
shouldSatisfyEventually' = shouldSatisfyEventually 20000000

spec :: Spec
spec = do
  describe "debounce" $ do
    it "should fold inputs" $ do
      ret <- runResourceT $ runConduit $ debSum 500000 (periodicSource 10000 [1..10]) .| CL.consume
      ret `shouldBe` [sum [1..10]]
    it "should debounce source" $ do
      let s = delayedSource [(1000, "a"), (1000, "b"), (200000, "c"), (1000, "d"), (1000, "e"), (200000, "f")]
      ret <- runResourceT $ runConduit $ debMonoid 100000 s .| CL.consume
      ret `shouldBe` ["ab", "cde", "f"]
    it "should terminate debounced Source immediately if the original Source terminates immediately" $ do
      ret <- timeout 50000000 $ runResourceT $ runConduit
             $ debMonoid 60000000 (CL.sourceList ["A", "B", "C", "D", "E"]) .| CL.consume
      ret `shouldBe` Just ["ABCDE"]
    it "should terminate the Sink for the original Source if the Sink for the debounced Source terminates" $ do
      (terminated, detector) <- terminationDetector
      let orig_source = detector $ periodicSource 10000 (repeat "a")
      ret <- runResourceT $ runConduit $ debMonoid 50000 orig_source .| CL.take 4
      length ret `shouldBe` 4
      forM_ ret (`shouldContain` "a")
      terminated `shouldSatisfyEventually'` (== True)
    it "should terminate the debounced Source gracefully if the original Source throws exception" $ do
      -- For now, the exception in the original Source is not handled,
      -- i.e., we just let it terminate the thread. So we'll see the
      -- error message while running the test.
      let s = (periodicSource 1000 ["a", "b"]) >> error "Exception in origSource" >> (periodicSource 1000 ["c", "d"])
      ret <- runResourceT $ runConduit $ debMonoid 100000 s .| CL.consume
      ret `shouldBe` ["ab"]
    it "should terminated the original Source gracefully if the downstream Sink throws exception" $ do
      (terminated, detector) <- terminationDetector
      let orig_source = detector $ periodicSource 10000 (repeat "a")
          ret_sink = do
            taken <- CL.take 4
            _ <- error "Exception in retSink"
            return taken
      (runResourceT $ runConduit $ debMonoid 50000 orig_source .| ret_sink) `shouldThrow` errorCall "Exception in retSink"
      terminated `shouldSatisfyEventually'` (== True)
    it "should connect the original Source in another thread" $ do
      this_thread <- myThreadId
      orig_thread_t <- newTVarIO Nothing
      let orig_source = do
            liftIO $ atomically . writeTVar orig_thread_t . Just =<< myThreadId
            yield 10
            return ()
      ret <- runResourceT $ runConduit $ debSum 10000 orig_source .| CL.consume
      ret `shouldBe` [10]
      orig_thread <- atomically (readTVar orig_thread_t)
      orig_thread `shouldSatisfy` isJust
      orig_thread `shouldSatisfy` (/= Just this_thread)
    it "should release the resource in the original Source when the original Source finishes" $ do
      (released, orig_source) <- attachResource $ periodicSource 10000 [1,2,3,4]
      ret <- runResourceT $ runConduit $ debSum 500000 orig_source .| CL.consume
      ret `shouldBe` [10]
      released `shouldSatisfyEventually'` (== True)
    it "should release the resource in the original Source when the downstream Sink throws exception" $ do
      (released, orig_source) <- attachResource $ periodicSource 10000 $ repeat "a"
      let sink = do
            taken <- CL.take 4
            _ <- error "Exception in downstream"
            return taken
      (runResourceT $ runConduit $ debMonoid 500000 orig_source .| sink)
        `shouldThrow` errorCall "Exception in downstream"
      released `shouldSatisfyEventually'` (== True)
    it "should release the resource in the original Source when the original Source throws exception" $ do
      -- Because the error is not handled, we'll see the error message
      -- while running the test. (See above for the case "original
      -- Source throwing exception")
      (released, orig_source) <- attachResource (periodicSource 10000 ["a", "b"] >> error "Exception in source")
      ret <- runResourceT $ runConduit $ debMonoid 500000 orig_source .| CL.consume
      ret `shouldBe` ["ab"]
      released `shouldSatisfyEventually'` (== True)


