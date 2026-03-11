{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C
import Data.Char
import Data.HashMap.Strict qualified as HashMap
import Data.List (sort)
import Data.Maybe
import Data.Vector qualified as V
import Network.Socket
import Network.Socket.ByteString
import System.Metrics.StatsD
import System.Metrics.StatsD.Internal
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import UnliftIO
  ( MonadIO (liftIO),
    MonadUnliftIO,
    atomically,
    bracket,
    link,
    newTQueueIO,
    newTVarIO,
    readTQueue,
    withAsync,
    writeTQueue,
  )

main :: IO ()
main = hspec $ do
  describe "parser" $ do
    prop "formats and parses a report" $ \r ->
      (parseReport . formatReport) r `shouldBe` Just r
  describe "statistics" $ do
    prop "calculates timing lengths" $ \ps -> do
      let ts = makeTimingStats ps
      length ts.timings `shouldBe` length ps
    prop "calculates p75 and p95 timing lengths" $ \ps -> do
      let ts = makeTimingStats ps
      length (trimPercentile 95 ts).timings `shouldBe` length (percent 95 ps)
      length (trimPercentile 75 ts).timings `shouldBe` length (percent 75 ps)
    prop "computes median" $ \ps ->
      not (null ps) ==> do
        let ts = makeTimingStats ps
        median ts `shouldBe` med ps
    prop "computes p75 and p95 median" $ \ps ->
      2 <= length ps ==> do
        let ts = makeTimingStats ps
        median (trimPercentile 95 ts) `shouldBe` med (percent 95 ps)
        median (trimPercentile 75 ts) `shouldBe` med (percent 75 ps)
    prop "computes mean" $ \ps ->
      not (null ps) ==> do
        let ts = makeTimingStats ps
        mean ts `shouldBe` mea ps
    prop "computes p75 and p95 mean" $ \ps ->
      2 <= length ps ==> do
        let ts = makeTimingStats ps
        mean (trimPercentile 95 ts) `shouldBe` mea (percent 95 ps)
        mean (trimPercentile 75 ts) `shouldBe` mea (percent 75 ps)
    prop "computes stdev" $ \ps ->
      not (null ps) ==> do
        let ts = makeTimingStats ps
        stdev ts `shouldBe` std ps
    prop "computes sums" $ \ps ->
      not (null ps) ==> do
        let ts = makeTimingStats ps
        V.last ts.cumsums `shouldBe` sum ps
    prop "computes p75 and p95 sums" $ \ps ->
      2 <= length ps ==> do
        let ts = makeTimingStats ps
        V.last (trimPercentile 95 ts).cumsums `shouldBe` sum (percent 95 ps)
        V.last (trimPercentile 75 ts).cumsums `shouldBe` sum (percent 75 ps)
    prop "computes sum of squares" $ \ps ->
      not (null ps) ==> do
        let ts = makeTimingStats ps
        V.last ts.cumsquares `shouldBe` sumsq ps
    prop "computes p75 and p95 sum of squares" $ \ps ->
      2 <= length ps ==> do
        let ts = makeTimingStats ps
        V.last (trimPercentile 95 ts).cumsquares `shouldBe` sumsq (percent 95 ps)
        V.last (trimPercentile 75 ts).cumsquares `shouldBe` sumsq (percent 75 ps)
  around (withMockStats testConfig) $ do
    describe "counter" $ do
      it "creates two counters" $ \(stats, _) -> do
        _ <- newStatCounter stats "planets" 1
        _ <- newStatCounter stats "moons" 1
        return () :: Expectation
      it "reports on two counters" $ \(stats, report) -> do
        planets <- newStatCounter stats "planets" 1
        moons <- newStatCounter stats "moons" 1
        incrementCounter planets 10
        incrementCounter moons 20
        rs <- replicateM 2 report
        let rs' =
              [ Report "planets" (Counter 10) 1.0,
                Report "moons" (Counter 20) 1.0
              ]
        rs `shouldMatchList` rs'
      it "filters reported samples" $ \(stats, report) -> do
        c <- newStatCounter stats "moons" 4
        let counts = [11 .. 30]
        forM_ counts (incrementCounter c)
        rs <- replicateM 5 report
        let rs' =
              [ Report "moons" (Counter x) 0.25
                | (i, x) <- zip [(1 :: Int) ..] counts,
                  i `mod` 4 == 0
              ]
        rs `shouldMatchList` rs'
        r <- report
        r.key `shouldNotBe` "moons"
      it "reports stats" $ \(stats, report) -> do
        c <- newStatCounter stats "planets" 4
        replicateM_ 20 (incrementCounter c 10)
        replicateM_ 5 report
        do
          rs <- replicateM 2 report
          let rs' =
                [ Report "stats.counters.planets.count" (Counter 200) 1.0,
                  Report "stats.counters.planets.rate" (Counter 4000) 1.0
                ]
          rs `shouldMatchList` rs'
        do
          rs <- replicateM 2 report
          let rs' =
                [ Report "stats.counters.planets.count" (Counter 0) 1.0,
                  Report "stats.counters.planets.rate" (Counter 0) 1.0
                ]
          rs `shouldMatchList` rs'
    describe "timing" $ do
      it "reports events" $ \(stats, report) -> do
        let timings = [51 .. 55]
        t <- newStatTiming stats "trips" 1
        forM_ timings (addTiming t)
        rs <- replicateM (length timings) report
        let rs' = map (\n -> Report "trips" (Timing n) 1.0) timings
        rs `shouldMatchList` rs'
      it "reports empty stats" $ \(stats, report) -> do
        _ <- newStatTiming stats "holes" 1
        replicateM_ 2 $ do
          let rs' =
                [ Report "stats.timers.holes.count" (Counter 0) 1.0,
                  Report "stats.timers.holes.count_ps" (Counter 0) 1.0,
                  Report "stats.timers.holes.count_90" (Counter 0) 1.0,
                  Report "stats.timers.holes.count_95" (Counter 0) 1.0
                ]
          rs <- replicateM (length rs') report
          rs `shouldMatchList` rs'
      it "filters reported samples" $ \(stats, report) -> do
        let timings = [1001 .. 2000]
        t <- newStatTiming stats "cats" 100
        forM_ timings (addTiming t)
        rs <- replicateM 10 report
        let rs' =
              [ Report "cats" (Timing x) 0.01
                | (i, x) <- zip [(1 :: Int) ..] timings,
                  i `mod` 100 == 0
              ]
        rs `shouldMatchList` rs'
      it "reports stats" $ \(stats, report) -> do
        let timings = [5 .. 1500]
        t <- newStatTiming stats "kittens" 0
        forM_ timings (addTiming t)
        let l = length timings
            r k v = Report ("stats.timers.kittens." <> k) v 1.0
        do
          let rs' =
                [ r "count" (Counter l),
                  r "count_ps" (Counter (l * 20)),
                  r "std" (Timing 432),
                  r "mean" (Timing 752),
                  r "lower" (Timing 5),
                  r "upper" (Timing 1500),
                  r "sum" (Timing 1125740),
                  r "sum_squares" (Timing 1126125220),
                  r "median" (Timing 752),
                  r "count_95" (Counter 1421),
                  r "mean_95" (Timing 715),
                  r "lower_95" (Timing 5),
                  r "upper_95" (Timing 1425),
                  r "sum_95" (Timing 1016015),
                  r "sum_squares_95" (Timing 965562395),
                  r "median_95" (Timing 715),
                  r "count_90" (Counter 1346),
                  r "mean_90" (Timing 677),
                  r "lower_90" (Timing 5),
                  r "upper_90" (Timing 1350),
                  r "sum_90" (Timing 911915),
                  r "sum_squares_90" (Timing 821036445),
                  r "median_90" (Timing 677)
                ]
          rs <- replicateM (length rs') report
          rs `shouldMatchList` rs'
        do
          let rs' =
                concatMap
                  (replicate 2)
                  [ r "count" (Counter 0),
                    r "count_ps" (Counter 0),
                    r "count_90" (Counter 0),
                    r "count_95" (Counter 0)
                  ]
          rs <- replicateM (length rs') report
          rs `shouldMatchList` rs'
    describe "gauge" $ do
      it "reports set events" $ \(stats, report) -> do
        let gauges = [51 .. 55]
        g <- newStatGauge stats "speed" 50
        forM_ gauges (setGauge g)
        rs <- replicateM (length gauges) report
        let rs' = map (\n -> Report "speed" (Gauge n False) 1.0) gauges
        rs `shouldMatchList` rs'
      it "reports stats" $ \(stats, report) -> do
        let gauges = [51 .. 55]
        g <- newStatGauge stats "radius" 50
        r <- report
        r `shouldBe` Report "stats.gauges.radius" (Gauge 50 False) 1.0
        forM_ gauges (setGauge g)
        replicateM_ (length gauges) report
        rs <- replicateM 2 report
        let rs' =
              replicate 2 $
                Report "stats.gauges.radius" (Gauge 55 False) 1.0
        rs `shouldMatchList` rs'
      it "increments and decrements value" $ \(stats, report) -> do
        g <- newStatGauge stats "breadth" 50
        let sk = "breadth"
            tk = "stats.gauges.breadth"
            r k v t = Report k (Gauge v t) 1.0
        incrementGauge g 5
        report `shouldReturn` r sk 5 True
        report `shouldReturn` r tk 55 False
        incrementGauge g (-10)
        report `shouldReturn` r sk (-10) True
        report `shouldReturn` r tk 45 False
        decrementGauge g 50
        report `shouldReturn` r sk (-50) True
        report `shouldReturn` r tk 0 False
        decrementGauge g (-25)
        report `shouldReturn` r sk 25 True
        report `shouldReturn` r tk 25 False
        incrementGauge g maxBound
        report `shouldReturn` r sk maxBound True
        report `shouldReturn` r tk maxBound False
        decrementGauge g minBound
        report `shouldReturn` r sk minBound True
        report `shouldReturn` r tk 0 False
        incrementGauge g maxBound
        report `shouldReturn` r sk maxBound True
        report `shouldReturn` r tk maxBound False
        decrementGauge g 5
        report `shouldReturn` r sk (-5) True
        report `shouldReturn` r tk (maxBound - 5) False
    describe "set" $ do
      it "reports events" $ \(stats, report) -> do
        let set = ["one", "two", "three", "three"]
        s <- newStatSet stats "potatoes"
        forM_ set (newSetElement s)
        rs <- replicateM (length set) report
        let rs' = map (\x -> Report "potatoes" (Set (C.pack x)) 1.0) set
        rs `shouldMatchList` rs'
      it "reports stats" $ \(stats, report) -> do
        let set = ["two", "three", "one", "two", "three", "three"]
        s <- newStatSet stats "bananas"
        forM_ set (newSetElement s)
        let rs' =
              [ Report "stats.sets.bananas.count" (Counter 3) 1.0,
                Report "stats.sets.bananas.count" (Counter 0) 1.0
              ]
        rs <- replicateM (length set + length rs') report
        rs `shouldEndWith` rs'

withMockStats ::
  (MonadUnliftIO m) =>
  StatConfig ->
  ((Stats, m Report) -> m a) ->
  m a
withMockStats cfg go =
  withMockSockets $ \(s1, s2) -> do
    q <- newTQueueIO
    withAsync (fwd s2 q) $ \a1 -> do
      link a1
      m <- newTVarIO HashMap.empty
      let s = newStats cfg {appendNewline = True} m s1
      withAsync (statsLoop s) $ \a2 -> do
        link a2
        go (s, atomically (readTQueue q))
  where
    fwd s2 q = forever $ do
      bs <- liftIO $ C.lines <$> recv s2 (2 ^ (20 :: Int))
      let rs = map (fromJust . parseReport) bs
      mapM_ (atomically . writeTQueue q) rs

withMockSockets :: (MonadUnliftIO m) => ((Socket, Socket) -> m a) -> m a
withMockSockets =
  bracket
    (liftIO (socketPair AF_UNIX Stream 0))
    (\(s1, s2) -> liftIO (close s1 >> close s2))

testConfig :: StatConfig
testConfig = defStatConfig {flushInterval = 50}

med :: [Int] -> Int
med ls
  | odd (length ls) =
      sort ls !! (length ls `div` 2)
  | otherwise =
      let ix = length ls `div` 2
          x = sort ls !! ix
          y = sort ls !! (ix - 1)
       in (x + y) `div` 2

mea :: [Int] -> Int
mea ls = sum ls `div` length ls

sumsq :: [Int] -> Int
sumsq = sum . map (\x -> x * x)

percent :: Int -> [Int] -> [Int]
percent pc ls = take (length ls * pc `div` 100) (sort ls)

std :: [Int] -> Int
std ls =
  let l = fromIntegral (length ls)
      ds = map (subtract (mea ls)) ls
      s = fromIntegral $ sumsq ds :: Double
      v = s / l
   in round (sqrt v)

genValidString :: Gen ByteString
genValidString =
  C.pack <$> listOf1 (arbitraryASCIIChar `suchThat` isValidChar)

isValidChar :: Char -> Bool
isValidChar c =
  isAscii c && (isAlpha c || isNumber c || isWhitelist c)
  where
    isWhitelist = flip elem (".-_" :: [Char])

genRate :: Gen Double
genRate = arbitrary `suchThat` \n -> n > 0.0 && n <= 1.0

genNatural :: Gen Int
genNatural = arbitrary `suchThat` (0 <=)

instance Arbitrary Value where
  arbitrary =
    oneof [counter, gauge, timing, set]
    where
      counter = Counter <$> genNatural
      timing = Timing <$> genNatural
      set = Set <$> genValidString
      gauge = do
        t <- arbitrary
        v <- if t then arbitrary else genNatural
        return $ Gauge v t

instance Arbitrary Report where
  arbitrary = do
    key <- genValidString
    value <- arbitrary
    rate <- case value of
      Counter {} -> genRate
      Gauge {} -> pure 1.0
      Timing {} -> genRate
      Set {} -> pure 1.0
    return $ Report key value rate