module Control.FoldDebounceSpec
    ( main
    , spec
    ) where

import           Control.Applicative          ((<$>))
import           Control.Concurrent           (threadDelay)
import           Data.Ratio                   ((%))

import           Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, tryReadTChan, writeTChan)
import qualified Control.FoldDebounce         as F
import           Control.Monad.STM            (STM, atomically)
import           Data.Time                    (addUTCTime, getCurrentTime)
import           Test.Hspec

main :: IO ()
main = hspec spec

forFIFO :: ([Int] -> IO ()) -> F.Args Int [Int]
forFIFO cb = F.Args
  { F.cb = cb
  , F.fold = \l v -> l ++ [v]
  , F.init = []
  }

callbackToTChan :: TChan a -> a -> IO ()
callbackToTChan output = atomically . writeTChan output

fifoTrigger :: F.Opts Int [Int] -> IO (F.Trigger Int [Int], TChan [Int])
fifoTrigger opts = do
  output <- atomically newTChan
  trig <- F.new (forFIFO $ callbackToTChan output) opts
  return (trig, output)

repeatFor :: Integer -> IO () -> IO ()
repeatFor duration_usec action = repeatUntil . addUTCTime (fromRational (duration_usec % 1000000)) =<< getCurrentTime
  where
    repeatUntil goal_time = do
      action
      cur_time <- getCurrentTime
      if cur_time > goal_time then return () else repeatUntil goal_time

readAllTChan :: TChan a -> STM [a]
readAllTChan chan = reverse <$> readAllTChan' []
  where
    readAllTChan' cur_ret = maybe (return cur_ret) (\val -> readAllTChan' (val : cur_ret)) =<< tryReadTChan chan

spec :: Spec
spec = do
  describe "Trigger" $ do
    it "emits single output event for single input event" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 50000 }
      F.send trig 10
      atomically (readTChan output) `shouldReturn` [10]
      F.close trig
    it "emits single FIFO list for multiple input events" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 50000 }
      F.send trig 10
      F.send trig 20
      F.send trig 30
      atomically (readTChan output) `shouldReturn` [10,20,30]
      F.close trig
    it "waits for more events that follow the first event" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 500000 }
      F.send trig 10
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` Nothing
      threadDelay 500000
      atomically (tryReadTChan output) `shouldReturn` Just [10]
      F.close trig
    it "emits the output event 'delay' interval after the first input event (alwaysResetTimer = False)" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 500000 }
      F.send trig 10
      threadDelay 100000
      F.send trig 20
      threadDelay 100000
      F.send trig 30
      threadDelay 100000
      F.send trig 40
      threadDelay 100000
      F.send trig 50
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` Just [10,20,30,40,50]
      F.close trig
    it "emits the output event 'delay' interval after the last input event (alwaysResetTimer = True)" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 500000, F.alwaysResetTimer = True }
      F.send trig 10
      threadDelay 100000
      F.send trig 20
      threadDelay 100000
      F.send trig 30
      threadDelay 100000
      F.send trig 40
      threadDelay 100000
      F.send trig 50
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` Nothing
      threadDelay 400000
      atomically (tryReadTChan output) `shouldReturn` Just [10,20,30,40,50]
    it "throws AlreadyClosedException after closed" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 10000 }
      F.send trig 10
      atomically (readTChan output) `shouldReturn` [10]
      F.close trig
      F.send trig 20 `shouldThrow` (\e -> case e of
                                       F.AlreadyClosedException -> True
                                       _                        -> False)
    it "emits a pending output event when closed" $ do
      (trig, output) <- fifoTrigger F.def { F.delay = 100000 }
      F.send trig 10
      F.send trig 20
      F.send trig 30
      F.close trig
      atomically (tryReadTChan output) `shouldReturn` Just [10,20,30]
    it "is ok to close after close" $ do
      (trig, _) <- fifoTrigger F.def { F.delay = 20000 }
      F.close trig
      F.close trig
    it "throws UnexpectedClosedException when close after the thread abnormally dies" $ do
      trig <- F.new F.Args { F.cb = error "Boom!", F.fold = (++), F.init = ""} F.def { F.delay = 10000 }
      F.send trig "hogehoge"
      threadDelay 50000
      F.close trig `shouldThrow` (\e -> case e of
                                     F.UnexpectedClosedException _ -> True
                                     _                             -> False)
    it "folds input events strictly" $ do
      output <- atomically newTChan
      trig <- F.new F.Args { F.cb = callbackToTChan output, F.fold = (+), F.init = 0 }
                    F.def { F.delay = 100000 }
      F.send trig 10
      F.send trig 20
      F.send trig undefined
      threadDelay 200000
      atomically (tryReadTChan output) `shouldReturn` (Nothing :: Maybe Int)
      F.close trig `shouldThrow` (\e -> case e of
                                        F.UnexpectedClosedException _ -> True
                                        _                             -> False)
    it "emits output events even if input events are coming intensely" $ do
      output <- atomically newTChan
      trig <- F.new F.Args { F.cb = callbackToTChan output, F.fold = (\_ i -> i), F.init = "" }
                    F.def { F.delay = 500 }
      repeatFor 2000 $ F.send trig "abc"
      F.close trig
      output_events <- atomically $ readAllTChan output
      output_events `shouldSatisfy` ((> 2) . length)
  describe "forStack" $ do
    it "creates a stacked FoldDebounce" $ do
      output <- atomically newTChan
      trig <- F.new (F.forStack $ callbackToTChan output)
                    F.def { F.delay = 50000 }
      F.send trig 10
      F.send trig 20
      F.send trig 30
      atomically (readTChan output) `shouldReturn` ([30,20,10] :: [Int])
      F.close trig
  describe "forMonoid" $ do
    it "creates a FoldDebounce for Monoids" $ do
      output <- atomically newTChan
      trig <- F.new (F.forMonoid $ callbackToTChan output)
                    F.def { F.delay = 50000 }
      F.send trig [10]
      F.send trig [20]
      F.send trig [30]
      atomically (readTChan output) `shouldReturn` ([10,20,30] :: [Int])
      F.close trig
  describe "forVoid" $ do
    it "discards input events, but starts the timer" $ do
      output <- atomically newTChan
      trig <- F.new (F.forVoid $ callbackToTChan output "hoge")
              F.def { F.delay = 50000 }
      F.send trig "foo1"
      F.send trig "foo2"
      F.send trig "foo3"
      atomically (readTChan output) `shouldReturn` "hoge"
      threadDelay 100000
      atomically (tryReadTChan output) `shouldReturn` Nothing
      F.close trig

