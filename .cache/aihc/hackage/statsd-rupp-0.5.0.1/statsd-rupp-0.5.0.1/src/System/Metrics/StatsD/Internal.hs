{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module System.Metrics.StatsD.Internal
  ( Stats (..),
    newStats,
    StatParams,
    newParams,
    StatConfig (..),
    MetricData (..),
    Store (..),
    Metrics,
    newMetrics,
    Value (..),
    Sample (..),
    Report (..),
    StatCounter (..),
    StatGauge (..),
    StatTiming (..),
    StatSet (..),
    addMetric,
    newMetric,
    validateKey,
    addReading,
    newReading,
    processSample,
    statsLoop,
    statsFlush,
    flushStats,
    catKey,
    statReports,
    TimingStats (..),
    makeTimingStats,
    timingReports,
    trimPercentile,
    percentileSuffix,
    timingStats,
    cumulativeSums,
    cumulativeSquares,
    stdev,
    mean,
    median,
    flush,
    toReport,
    formatReport,
    submit,
    connectStatsD,
    parseReport,
    parseRead,
    parseInt,
  )
where

import Control.Monad (MonadPlus (..), forM_, forever, guard, unless, void, when)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Builder (byteString, char8, intDec, string8, toLazyByteString)
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as L
import Data.Char (isAlphaNum, isAscii)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.List (nub, sort)
import Data.Vector (Vector, (!))
import Data.Vector qualified as V
import Network.Socket (Socket)
import Network.Socket qualified as Net
import Network.Socket.ByteString qualified as Net
import Text.Printf (printf)
import Text.Read (readMaybe)
import UnliftIO (MonadIO, handleIO, liftIO, throwIO)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.STM
  ( STM,
    TVar,
    atomically,
    modifyTVar,
    newTVarIO,
    readTVar,
    stateTVar,
  )

data Stats = Stats
  { metrics :: !(TVar Metrics),
    socket :: !Socket,
    params :: !StatParams
  }

data StatParams = StatParams
  { pfx :: !ByteString,
    pfxCounter :: !ByteString,
    pfxTimer :: !ByteString,
    pfxGauge :: !ByteString,
    pfxSet :: !ByteString,
    flush :: !Int,
    stats :: !Bool,
    samples :: !Bool,
    percentiles :: ![Int],
    newline :: !Bool
  }

data StatConfig = StatConfig
  { reportStats :: !Bool,
    reportSamples :: !Bool,
    namespace :: !String,
    prefixStats :: !String,
    prefixCounter :: !String,
    prefixTimer :: !String,
    prefixGauge :: !String,
    prefixSet :: !String,
    statsdServer :: !String,
    statsdPort :: !Int,
    flushInterval :: !Int,
    timingPercentiles :: ![Int],
    appendNewline :: !Bool
  }
  deriving (Show, Read, Eq, Ord)

data MetricData
  = CounterData !Int
  | GaugeData !Int
  | TimingData ![Int]
  | SetData !(HashSet ByteString)

data Store = Store
  { index :: !Int,
    dat :: !(Maybe MetricData)
  }

type Metrics = HashMap ByteString Store

data Value
  = Counter !Int
  | Gauge !Int !Bool
  | Timing !Int
  | Set !ByteString
  deriving (Eq, Ord, Show, Read)

data Report = Report
  { key :: !ByteString,
    value :: !Value,
    rate :: !Double
  }
  deriving (Eq, Ord, Show, Read)

data Sample = Sample
  { key :: !ByteString,
    value :: !Value,
    sampling :: !Int,
    index :: !Int
  }
  deriving (Eq, Ord, Show, Read)

data StatCounter = StatCounter
  { stats :: !Stats,
    key :: !ByteString,
    sampling :: !Int
  }

data StatGauge = StatGauge
  { stats :: !Stats,
    key :: !ByteString
  }

data StatTiming = StatTiming
  { stats :: !Stats,
    key :: !ByteString,
    sampling :: !Int
  }

data StatSet = StatSet
  { stats :: !Stats,
    key :: !ByteString
  }

addMetric :: StatParams -> ByteString -> MetricData -> Metrics -> Metrics
addMetric params key dat =
  HashMap.insert key $
    Store 0 $
      if params.stats then Just dat else Nothing

newMetric :: (MonadIO m) => Stats -> ByteString -> MetricData -> m ()
newMetric stats key store = do
  unless (validateKey key) $ do
    throwIO $ userError "Metric key is invalid"
  e <- atomically $ do
    exists <- HashMap.member key <$> readTVar stats.metrics
    if exists
      then return True
      else do
        modifyTVar
          stats.metrics
          (addMetric stats.params key store)
        return False
  when e $ throwIO $ userError $ "StatsD key exists: " <> C.unpack key

validateKey :: ByteString -> Bool
validateKey t = not (C.null t) && C.all valid t
  where
    valid c = elem c ("._-" :: [Char]) || isAscii c && isAlphaNum c

addReading :: Value -> ByteString -> Metrics -> Metrics
addReading reading = HashMap.adjust adjust
  where
    adjust m = m {index = m.index + 1, dat = change <$> m.dat}
    change store = case (reading, store) of
      (Counter c, CounterData s) ->
        CounterData (s + c)
      (Gauge i False, GaugeData _) ->
        GaugeData i
      (Gauge i True, GaugeData g)
        | i > maxBound - g -> GaugeData maxBound
        | otherwise -> GaugeData (max 0 (g + i))
      (Timing t, TimingData s) ->
        TimingData (t : s)
      (Set e, SetData s) ->
        SetData (HashSet.insert e s)
      _ -> error "Stats reading mismatch"

newReading :: Stats -> ByteString -> Value -> STM Int
newReading stats key reading = do
  modifyTVar stats.metrics (addReading reading key)
  maybe 0 (.index) . HashMap.lookup key
    <$> readTVar stats.metrics

processSample ::
  (MonadIO m) => Stats -> Int -> ByteString -> Value -> m ()
processSample stats sampling key val = do
  idx <- atomically $ newReading stats key val
  when stats.params.samples $
    submit stats $
      Sample key val sampling idx

newMetrics :: (MonadIO m) => m (TVar Metrics)
newMetrics = newTVarIO HashMap.empty

newParams :: StatConfig -> StatParams
newParams cfg
  | valid =
      StatParams
        { pfx = pfx_namespace,
          pfxCounter = full_pfx_stats <> counters <> ".",
          pfxGauge = full_pfx_stats <> gauges <> ".",
          pfxTimer = full_pfx_stats <> timers <> ".",
          pfxSet = full_pfx_stats <> sets <> ".",
          newline = cfg.appendNewline,
          stats = cfg.reportStats,
          samples = cfg.reportSamples,
          percentiles = 100 : nub cfg.timingPercentiles,
          flush = cfg.flushInterval
        }
  | otherwise = error "StatsD config invalid"
  where
    namespace = C.pack cfg.namespace
    pfx_stats = C.pack cfg.prefixStats
    gauges = C.pack cfg.prefixGauge
    counters = C.pack cfg.prefixCounter
    timers = C.pack cfg.prefixTimer
    sets = C.pack cfg.prefixSet
    valid =
      all validateKey [pfx_stats, gauges, counters, timers, sets]
        && bool (validateKey namespace) True (C.null namespace)
        && cfg.flushInterval > 0
        && all (\pc -> pc > 0 && 100 > pc) cfg.timingPercentiles
    pfx_namespace =
      if C.null namespace
        then ""
        else namespace <> "."
    full_pfx_stats = pfx_namespace <> pfx_stats <> "."

newStats :: StatConfig -> TVar Metrics -> Socket -> Stats
newStats cfg metrics socket =
  Stats
    { metrics = metrics,
      socket = socket,
      params = newParams cfg
    }

statsLoop :: (MonadIO m) => Stats -> m ()
statsLoop stats = forever $ do
  threadDelay (stats.params.flush * 1000)
  statsFlush stats

statsFlush :: (MonadIO m) => Stats -> m ()
statsFlush stats = do
  mapM_ (send stats)
    =<< atomically
      (stateTVar stats.metrics (flushStats stats.params))

flushStats :: StatParams -> Metrics -> ([Report], Metrics)
flushStats params metrics =
  let f xs key m = maybe xs ((<> xs) . statReports params key) m.dat
      rs = HashMap.foldlWithKey' f [] metrics
      g m = m {dat = flush <$> m.dat}
      ms = HashMap.map g metrics
   in (rs, ms)

catKey :: [ByteString] -> ByteString
catKey = C.intercalate "." . filter (not . C.null)

statReports :: StatParams -> ByteString -> MetricData -> [Report]
statReports params key dat = case dat of
  CounterData c ->
    [ Report
        { key = params.pfxCounter <> key <> ".count",
          value = Counter c,
          rate = 1.0
        },
      Report
        { key = params.pfxCounter <> key <> ".rate",
          value = Counter (computeRate params c),
          rate = 1.0
        }
    ]
  GaugeData s ->
    [ Report
        { key = params.pfxGauge <> key,
          value = Gauge s False,
          rate = 1.0
        }
    ]
  SetData s ->
    [ Report
        { key = params.pfxSet <> key <> ".count",
          value = Counter (HashSet.size s),
          rate = 1.0
        }
    ]
  TimingData s -> timingReports params key s

data TimingStats = TimingStats
  { timings :: !(Vector Int),
    cumsums :: !(Vector Int),
    cumsquares :: !(Vector Int)
  }
  deriving (Eq, Ord, Show, Read)

makeTimingStats :: [Int] -> TimingStats
makeTimingStats timings =
  TimingStats
    { timings = V.fromList sorted,
      cumsums = V.fromList (cumulativeSums sorted),
      cumsquares = V.fromList (cumulativeSquares sorted)
    }
  where
    sorted = sort timings

timingReports :: StatParams -> ByteString -> [Int] -> [Report]
timingReports params key timings =
  concatMap (timingStats params key tstats) params.percentiles
  where
    tstats = makeTimingStats timings

trimPercentile :: Int -> TimingStats -> TimingStats
trimPercentile pc ts =
  ts
    { timings = f ts.timings,
      cumsums = f ts.cumsums,
      cumsquares = f ts.cumsquares
    }
  where
    f ls = V.take (length ls * pc `div` 100) ls

percentileSuffix :: Int -> ByteString
percentileSuffix pc
  | pc == 100 = ""
  | otherwise = C.pack $ printf "_%d" pc

computeRate :: StatParams -> Int -> Int
computeRate params i = i * 1000 `div` params.flush

mean :: TimingStats -> Int
mean ts = V.last ts.cumsums `div` length ts.timings

timingStats :: StatParams -> ByteString -> TimingStats -> Int -> [Report]
timingStats params key tstats pc =
  concat
    [ [mkr "count" (Counter (length ts.timings))],
      [mkr "count_ps" (Counter rate) | pc == 100],
      [mkr "std" (Timing (stdev ts)) | pc == 100, not empty],
      [mkr "mean" (Timing (mean ts)) | not empty],
      [mkr "upper" (Timing (V.last ts.timings)) | not empty],
      [mkr "lower" (Timing (V.head ts.timings)) | not empty],
      [mkr "sum" (Timing (V.last ts.cumsums)) | not empty],
      [mkr "sum_squares" (Timing (V.last ts.cumsquares)) | not empty],
      [mkr "median" (Timing (median ts)) | not empty]
    ]
  where
    ts = trimPercentile pc tstats
    empty = null ts.timings
    rate = computeRate params (length ts.timings)
    sfx = percentileSuffix pc
    pfx = params.pfxTimer <> key <> "."
    mkr s v = Report {key = pfx <> s <> sfx, value = v, rate = 1.0}

cumulativeSums :: (Num a) => [a] -> [a]
cumulativeSums = scanl1 (+)

cumulativeSquares :: (Num a) => [a] -> [a]
cumulativeSquares = scanl1 (+) . map (\x -> x * x)

stdev :: TimingStats -> Int
stdev ts =
  round $ sqrt var
  where
    len = length ts.timings
    var = fromIntegral ds / fromIntegral len :: Double
    ds = sum $ map ((^ (2 :: Int)) . subtract (mean ts)) (V.toList ts.timings)

median :: TimingStats -> Int
median ts
  | null ts.timings = 0
  | even (length ts.timings) =
      let lower = ts.timings ! middle
          upper = ts.timings ! subtract 1 middle
       in (lower + upper) `div` 2
  | otherwise =
      ts.timings ! middle
  where
    middle = length ts.timings `div` 2

flush :: MetricData -> MetricData
flush (CounterData _) = CounterData 0
flush (GaugeData g) = GaugeData g
flush (TimingData _) = TimingData []
flush (SetData _) = SetData HashSet.empty

toReport :: StatParams -> Sample -> Maybe Report
toReport params sample
  | sample.sampling > 0 && sample.index `mod` sample.sampling == 0 =
      Just
        Report
          { key = params.pfx <> sample.key,
            value = sample.value,
            rate = 1.0 / fromIntegral sample.sampling
          }
  | otherwise = Nothing

formatReport :: Report -> ByteString
formatReport report = L.toStrict $ toLazyByteString builder
  where
    builder = byteString report.key <> char8 ':' <> val
    rate
      | report.rate < 1.0 =
          string8 $ printf "|@%f" report.rate
      | otherwise = mempty
    val =
      case report.value of
        Counter i ->
          intDec i <> "|c" <> rate
        Gauge g False ->
          intDec g <> "|g"
        Gauge g True ->
          let sign = if 0 <= g then char8 '+' else mempty
           in sign <> intDec g <> "|g"
        Timing t ->
          intDec t <> "|ms" <> rate
        Set e ->
          byteString e <> "|s"

submit :: (MonadIO m) => Stats -> Sample -> m ()
submit stats sample =
  forM_ (toReport stats.params sample) (send stats)

send :: (MonadIO m) => Stats -> Report -> m ()
send stats report =
  liftIO $
    handleIO (const (return ())) $
      void $
        Net.send
          stats.socket
          (formatReport report <> bool "" "\n" stats.params.newline)

connectStatsD :: (MonadIO m) => String -> Int -> m Socket
connectStatsD host port = liftIO $ do
  as <-
    Net.getAddrInfo
      Nothing
      (Just host)
      (Just $ show port)
  a <- case as of
    a : _ -> return a
    [] -> error $ "Cannot resolve: " <> host <> ":" <> show port
  sock <- Net.socket (Net.addrFamily a) Net.Datagram Net.defaultProtocol
  Net.connect sock (Net.addrAddress a)
  return sock

parseReport :: (MonadPlus m) => ByteString -> m Report
parseReport bs =
  case C.split '|' bs of
    [kv, t] -> do
      (k, v) <- parseKeyValue kv t
      return $ Report k v 1.0
    [kv, t, r] -> do
      (k, v) <- parseKeyValue kv t
      x <- parseRate r
      return $ Report k v x
    _ -> mzero
  where
    parseKeyValue kv t = do
      case C.split ':' kv of
        [key, v] -> do
          guard (validateKey key)
          value <- parseValue v t
          return (key, value)
        _ -> mzero
    parseValue v t =
      case C.unpack t of
        "c" -> Counter <$> parseNatural v
        "g" ->
          case C.uncons v of
            Just ('+', _) -> Gauge <$> parseInt v <*> pure True
            Just ('-', _) -> Gauge <$> parseInt v <*> pure True
            _ -> Gauge <$> parseNatural v <*> pure False
        "s" -> do
          guard (validateKey v)
          return (Set v)
        "ms" -> Timing <$> parseNatural v
        _ -> mzero
    parseRate r = case C.uncons r of
      Just ('@', s) -> do
        t <- parseRead s
        guard (t > 0.0)
        guard (t < 1.0)
        return t
      _ -> mzero

parseRead :: (MonadPlus m, Read a) => ByteString -> m a
parseRead = maybe mzero return . readMaybe . C.unpack

parseInt :: (MonadPlus m) => ByteString -> m Int
parseInt bs = case C.readInt bs of
  Just (i, bs') | B.null bs' -> return i
  _ -> mzero

parseNatural :: (MonadPlus m) => ByteString -> m Int
parseNatural bs = case C.readInt bs of
  Just (i, bs') | B.null bs' -> guard (0 <= i) >> return i
  _ -> mzero
