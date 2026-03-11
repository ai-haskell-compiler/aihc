{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Test.Hspec

import           Control.Concurrent
import           Control.Concurrent.AlarmClock
import           Control.Concurrent.AlarmClock.TimeScale
import           Control.Concurrent.STM
import           Control.Monad
import           Data.IORef
import           Data.Proxy
import           Data.Time
import           System.Clock

main :: IO ()
main = hspec $ describe "Control.Concurrent.AlarmClock" $ do

  describe "Timescale" $ do
    let beforeTimescaleTest :: TimeScale t => IO (t,t)
        beforeTimescaleTest = do
          putStrLn "getting first time"
          t0 <- getAbsoluteTime
          threadDelay 500000
          putStrLn "getting second time"
          t1 <- getAbsoluteTime
          putStrLn "got times"
          return (t0, t1)

        timescaleSpec :: (TimeScale t, Eq t, Show t) => Proxy t -> SpecWith (t,t)
        timescaleSpec _ = do
          describe "earlierOf" $ do
            it "picks the first if earlier"  $ \(t0,t1) -> earlierOf t0 t1 `shouldBe` t0
            it "picks the second if earlier" $ \(t0,t1) -> earlierOf t1 t0 `shouldBe` t0

          describe "microsecondsDiff" $ do
            it "finds a difference of at least 500ms"
              $ \(t0,t1) -> microsecondsDiff t1 t0 `shouldSatisfy` (>= 500000)
            it "finds a difference of no more than 600ms"
              $ \(t0,t1) -> microsecondsDiff t1 t0 `shouldSatisfy` (<= 600000)
            it "finds a negative difference if args reversed"
              $ \(t0,t1) -> microsecondsDiff t0 t1 `shouldSatisfy` (<= (-500000))

    describe "UTCTime"   $ beforeAll beforeTimescaleTest $ timescaleSpec (Proxy :: Proxy UTCTime)
    describe "Monotonic" $ beforeAll beforeTimescaleTest $ timescaleSpec (Proxy :: Proxy MonotonicTime)

  let makeLog :: IO (a -> IO (), IO [a])
      makeLog = do
        logVar <- newIORef []
        return (modifyIORef logVar . (:), reverse <$> readIORef logVar)

  describe "AlarmClock" $ do
    let waitUntilUnset ac = do
          atomically $ guard . not =<< isAlarmSetSTM ac
          threadDelay 50000 -- alarm becomes unset before action has completed

        alarmClockSpec :: (TimeScale t, Eq t, Show t)
          => (AlarmClock t -> AlarmClock t) -> (Double -> t -> t) -> Spec
        alarmClockSpec acid addTime = do

          it "wakes up immediately on setAlarmNow" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              setAlarmNow $ acid ac
              threadDelay 100000
            readLog `shouldReturn` ["alarm went off"]

          it "wakes up a bit later using setAlarm" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              setAlarm ac . addTime 0.2 =<< getAbsoluteTime
              writeLog "waiting"
              threadDelay 100000
              writeLog "still waiting"
              threadDelay 200000
              writeLog "should have gone off by now"
            readLog `shouldReturn`
              [ "waiting"
              , "still waiting"
              , "alarm went off"
              , "should have gone off by now"
              ]

          it "wakes up at the earliest set time and no others" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              setAlarm ac . addTime 0.4 =<< getAbsoluteTime
              setAlarm ac . addTime 0.2 =<< getAbsoluteTime
              writeLog "waiting"
              threadDelay 100000
              writeLog "still waiting"
              threadDelay 200000
              writeLog "should have gone off once by now"
              threadDelay 200000
              writeLog "should not have gone off again"
            readLog `shouldReturn`
              [ "waiting"
              , "still waiting"
              , "alarm went off"
              , "should have gone off once by now"
              , "should not have gone off again"
              ]

          it "reports whether it is set or not" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              let logIfSet = do
                    currentlySet <- isAlarmSet ac
                    writeLog $ if currentlySet then "alarm is set" else "alarm is not set"
              logIfSet
              setAlarm ac . addTime 0.2 =<< getAbsoluteTime
              logIfSet
              writeLog "waiting"
              threadDelay 100000
              writeLog "still waiting"
              logIfSet
              threadDelay 200000
              writeLog "should have gone off by now"
              logIfSet
            readLog `shouldReturn`
              [ "alarm is not set"
              , "alarm is set"
              , "waiting"
              , "still waiting"
              , "alarm is set"
              , "alarm went off"
              , "should have gone off by now"
              , "alarm is not set"
              ]

          it "works within the STM monad" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              now <- getAbsoluteTime
              atomically $ do
                guard . not =<< isAlarmSetSTM ac
                setAlarmSTM ac $ addTime 0.1 now
                guard =<< isAlarmSetSTM ac
              writeLog "alarm is set"
              waitUntilUnset ac
              writeLog "alarm now not set again"
            readLog `shouldReturn`
              [ "alarm is set"
              , "alarm went off"
              , "alarm now not set again"
              ]

          it "can be set again once it goes off" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              startTime <- getAbsoluteTime
              setAlarm ac $ addTime 0.1 startTime
              writeLog "alarm is set"
              waitUntilUnset ac
              writeLog "alarm is not set"
              setAlarm ac $ addTime 0.2 startTime
              writeLog "alarm is set"
              waitUntilUnset ac
              writeLog "alarm is not set"
            readLog `shouldReturn`
              [ "alarm is set"
              , "alarm went off"
              , "alarm is not set"
              , "alarm is set"
              , "alarm went off"
              , "alarm is not set"
              ]

          it "goes off immediately if set to go off in the past" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\_ _ -> writeLog "alarm went off") $ \ac -> do
              startTime <- getCurrentTime
              now <- getAbsoluteTime
              setAlarm ac $ addTime (-0.1) now
              writeLog "alarm is set"
              waitUntilUnset ac
              writeLog "done"
              endTime <- getCurrentTime
              diffUTCTime endTime startTime `shouldSatisfy` (\t -> 0.0 <= t && t < 0.1)
            readLog `shouldReturn`
              [ "alarm is set"
              , "alarm went off"
              , "done"
              ]

          it "blocks destruction if the alarm is going off" $ do
            (writeLog, readLog) <- makeLog
            let alarmAction _ _ = do
                  writeLog "alarm going off"
                  threadDelay 200000
                  writeLog "alarm finished going off"

            withAlarmClock alarmAction $ \ac -> do
              setAlarmNow $ acid ac
              threadDelay 100000
              writeLog "destroying alarm clock"
            writeLog "alarm clock destroyed"
            readLog `shouldReturn`
              [ "alarm going off"
              , "destroying alarm clock"
              , "alarm finished going off"
              , "alarm clock destroyed"
              ]

          it "successfully destroys even if wakeup action tries to set again" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\ac t -> writeLog "alarm went off" >> setAlarm ac (addTime 0.2 t)) $ \ac -> do
              setAlarmNow $ acid ac
              threadDelay 100000
            threadDelay 300000
            readLog `shouldReturn` ["alarm went off"]

          it "picks the shorter time if a longer time is set in the wakeup" $ do
            (writeLog, readLog) <- makeLog
            withAlarmClock (\ac t -> writeLog "alarm went off" >> setAlarm ac (addTime 0.5 t)) $ \ac -> do
              setAlarmNow $ acid ac
              threadDelay 100000
              setAlarm ac . addTime 0.1 =<< getAbsoluteTime
              writeLog "set a shorter time"
              threadDelay 200000
              writeLog "alarm should have gone off after shorter time"
              threadDelay 500000
              writeLog "alarm should have gone off after longer time"
            readLog `shouldReturn`
              [ "alarm went off"
              , "set a shorter time"
              , "alarm went off"
              , "alarm should have gone off after shorter time"
              , "alarm went off"
              , "alarm should have gone off after longer time"
              ]

          it "doesn't block destruction even in a tight loop" $ withAlarmClock (\ac t -> setAlarm ac t) $ \ac -> do
            setAlarmNow $ acid ac
            threadDelay 100000

    describe "UTCTime"       $ alarmClockSpec id $ addUTCTime . fromRational . toRational
    describe "MonotonicTime" $ alarmClockSpec id $ \dts (MonotonicTime ts) ->
      MonotonicTime $ fromNanoSecs $ toNanoSecs ts + floor (dts * 1e9)

    it "re-checks the time before going off" $
      withAlarmClock (\_ _ -> return ()) $ \ac -> do
        startTime <- getCurrentTime
        setAlarm ac $ AdjustingClock $ addUTCTime 0.3 startTime
        atomically $ guard . not =<< isAlarmSetSTM ac
        endTime <- getCurrentTime
        diffUTCTime endTime startTime `shouldSatisfy` (\t -> 0.3 <= t && t <= 0.5)

newtype AdjustingClock = AdjustingClock UTCTime deriving (Show, Eq, Ord)

instance TimeScale AdjustingClock where
  getAbsoluteTime        = AdjustingClock <$> getCurrentTime
  earlierOf              = min
  microsecondsDiff (AdjustingClock t1) (AdjustingClock t2) = 10000 + microsecondsDiff t1 t2 `div` 2
