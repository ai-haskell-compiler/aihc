{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# OPTIONS_GHC -funbox-strict-fields  #-}
{- |
Module:       Miniterion
License:      MIT

Simple benchmarking utilities with API subset of
<https://hackage.haskell.org/package/criterion criterion> (and also a
subset of <https://hackage.haskell.org/package/gauge gauge> and
<https://hackage.haskell.org/package/tasty-bench tasty-bench>).

The goal of this package is to provide simple and lightweight
benchmark utilities with less amount of codes and dependency
packages. For robust and feature rich benchmarking utility, use the
other packages mentioned above.

This is the only module exposed from the @miniterion@ package. The
dependency packages of @miniterion@ are kept small (at the moment
@base@ and @deepseq@) to make the compilation time and installation
time short, by dropping some functionalities and efficiencies.

-}
module Miniterion
  (
    -- * Types
    Benchmark
  , Benchmarkable

    -- * Creating benchmark suite
  , env
  , envWithCleanup
  , perBatchEnv
  , perBatchEnvWithCleanup
  , perRunEnv
  , perRunEnvWithCleanup
  , toBenchmarkable
  , bench
  , bgroup

  -- * Running a benchmark
  , nf
  , whnf
  , nfIO
  , whnfIO
  , nfAppIO
  , whnfAppIO

    -- * Turning a suite of benchmarks into a program
  , defaultMain
  , defaultMainWith
  , defaultConfig

    -- * For interactive use
  , benchmark

    -- * Configuring Miniterion
    -- $miniterion_specific
  , Config(..)
  , UseColor(..)
  , MatchMode(..)
  , Timeout(..)

#ifdef DEV
    -- * For development, exposed for testing
  , showPicos5
  , showBytes
  , mu
  , MEnv
  , getDefaultMEnv
  , Doc
  , docToString
#endif
  ) where

-- base
import           Control.Exception      (Exception (..), SomeException (..),
                                         evaluate, finally, handle, throw,
                                         throwIO)
import           Control.Monad          (guard, unless, void, when, (>=>))
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bits              (shiftL, shiftR, xor, (.|.))
import           Data.Char              (toLower)
import           Data.Foldable          (find, foldlM)
import           Data.Int               (Int64)
import           Data.List              (intercalate, isPrefixOf, sort, tails,
                                         unfoldr)
import           Data.Maybe             (mapMaybe)
import           Data.String            (IsString (..))
import           Data.Word              (Word64)
import           System.Console.GetOpt  (ArgDescr (..), ArgOrder (..),
                                         OptDescr (..), getOpt', usageInfo)
import           System.CPUTime         (getCPUTime)
import           System.Environment     (getArgs, getProgName)
import           System.Exit            (die, exitFailure)
import           System.IO              (BufferMode (..), Handle, IOMode (..),
                                         hFlush, hGetLine, hIsEOF,
                                         hIsTerminalDevice, hPutStr, hPutStrLn,
                                         hSetBuffering, stderr, stdout,
                                         withFile)
import           System.Mem             (performGC, performMinorGC)
import           System.Timeout         (timeout)
import           Text.Printf            (printf)
import           Text.Read              (readMaybe)

#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable          (foldl')
#endif

#if defined(mingw32_HOST_OS)
import           Control.Exception      (bracket)
import           Data.Word              (Word32)
#endif

-- base, GHC.* modules
import           GHC.Arr                (listArray, (!))
import           GHC.Clock              (getMonotonicTimeNSec)
import           GHC.IO.Encoding        (getLocaleEncoding, setLocaleEncoding,
                                         textEncodingName, utf8)
import           GHC.Stats              (RTSStats (..), getRTSStats,
                                         getRTSStatsEnabled)
#if MIN_VERSION_base(4,15,0)
import           GHC.Exts               (SPEC (..))
#else
import           GHC.Exts               (SpecConstrAnnotation (..))
#endif

-- deepseq
import           Control.DeepSeq        (NFData, force, rnf)

#if IS_PACKAGE_BUILD
-- Internal
import           Paths_miniterion       (getDataFileName)
#endif


-- ------------------------------------------------------------------------
-- Exported
-- ------------------------------------------------------------------------

-- | Benchmarks are simple tree structure with names, and additional
-- information to support 'envWithCleanup'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#t:Benchmark Benchmark>@.
--
-- @since 0.1.0.0
data Benchmark
  = Bench String Benchmarkable
  | Bgroup String [Benchmark]
  | forall e. NFData e => Environment (IO e) (e -> IO ()) (e -> Benchmark)

-- | Something that can be benchmarked, produced by 'nf', 'whnf',
-- 'nfIO', 'whnfIO', 'nfAppIO', and 'whnfAppIO'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#t:Benchmarkable Benchmarkable>@.
--
-- @since 0.1.0.0
data Benchmarkable = forall a. NFData a =>
  Benchmarkable { allocEnv      :: Word64 -> IO a
                , cleanEnv      :: Word64 -> a -> IO ()
                , runRepeatedly :: a -> Word64 -> IO ()
                , perRun        :: Bool }

-- | Construct a t'Benchmarkable' value from an impure action, where
-- the 'Word64' parameter indicates the number of times to run the
-- action.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:toBenchmarkable toBenchmarkable>@.
--
-- @since 0.1.0.0
toBenchmarkable :: (Word64 -> IO ()) -> Benchmarkable
toBenchmarkable f = Benchmarkable noop (const noop) (const f) False
{-# INLINE toBenchmarkable #-}

-- | Attach a name to t'Benchmarkable'.
--
-- The type signature is compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:bench bench>@.
--
-- @since 0.1.0.0
bench
  :: String -- ^ Name of this benchmark.
  -> Benchmarkable -- ^ Benchmark target.
  -> Benchmark
bench = Bench

-- | Attach a name to a group of 'Benchmark'.
--
-- The type signature is compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:bgroup bgroup>@.
--
-- @since 0.1.0.0
bgroup
  :: String -- ^ Name of this benchmark group.
  -> [Benchmark] -- ^ List of benchmarks in the group.
  -> Benchmark
bgroup = Bgroup

-- | Run a benchmark (or collection of benchmarks) in the given
-- environment, usually reading large input data from file.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:env env>@.
--
-- @since 0.1.0.0
env
  :: NFData env
  => IO env -- ^ Action to create the environment.
  -> (env -> Benchmark) -- ^ A function returning benchmark.
  -> Benchmark
env alloc = envWithCleanup alloc noop

-- | Similar to 'env', but includes an additional argument to clean up
-- the environment.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:envWithCleanup envWithCleanup>@.
--
-- @since 0.1.0.0
envWithCleanup
  :: NFData env
  => IO env -- ^ Action to create the environment.
  -> (env -> IO a) -- ^ Action to cleanup the environment.
  -> (env -> Benchmark) -- ^ A function returning benchmark.
  -> Benchmark
envWithCleanup alloc clean = Environment alloc (void . clean)

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- batch of runs of the benchmarkable.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perBatchEnv perBatchEnv>@.
--
-- @since 0.1.0.0
perBatchEnv
  :: (NFData env, NFData b)
  => (Word64 -> IO env)
  -- ^ Action to create an environment for a batch of N runs.
  -> (env -> IO b)
  -- ^ Benchmark body function.
  -> Benchmarkable
perBatchEnv alloc = perBatchEnvWithCleanup alloc (const noop)

-- | Same as `perBatchEnv`, but but allows for an additional callback
-- to clean up the environment.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perBatchEnvWithCleanup perBatchEnvWithCleanup>@.
--
-- @since 0.1.0.0
perBatchEnvWithCleanup
  :: (NFData env, NFData b)
  => (Word64 -> IO env)
  -- ^ Action to create an environment for a batch of N runs.
  -> (Word64 -> env -> IO ())
  -- ^ Action to cleanup the environment.
  -> (env -> IO b)
  -- ^ Benchmark body function.
  -> Benchmarkable
perBatchEnvWithCleanup alloc clean run = Benchmarkable alloc clean run' False
  where
    run' = ioToBench rnf .  run

-- | Create a Benchmarkable where a fresh environment is allocated for
-- every run of the operation to benchmark.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perRunEnv perRunEnv>@.
--
-- @since 0.1.0.0
perRunEnv
  :: (NFData env, NFData b)
  => IO env -- ^ Action to create an environment for a single run.
  -> (env -> IO b) -- ^ Benchmark body function.
  -> Benchmarkable
perRunEnv alloc = perRunEnvWithCleanup alloc noop

-- | Same as `perBatchEnv`, but allows for an additional callback to
-- clean up the environment.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:perRunEnvWithCleanup perRunEnvWithCleanup>@.
--
-- @since 0.1.0.0
perRunEnvWithCleanup
  :: (NFData env, NFData b)
  => IO env -- ^ Action to create an environment for a single run.
  -> (env -> IO ()) -- ^ Action to cleanup the environment.
  -> (env -> IO b) -- ^ Benchmark body function.
  -> Benchmarkable
perRunEnvWithCleanup alloc clean run = bm {perRun = True}
  where
    bm = perBatchEnvWithCleanup (const alloc) (const clean) run

-- | 'nf' @f@ @x@ measures time to compute a normal form (by means of
-- 'Control.DeepSeq.rnf', not 'force') of an application of @f@ to
-- @x@.  This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nf nf>@.
--
-- @since 0.1.0.0
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = fmap toBenchmarkable . nf' rnf

-- | 'whnf' @f@ @x@ measures time to compute a weak head normal form
-- of an application of @f@ to @x@.  This does not include time to
-- evaluate @f@ or @x@ themselves.  Ideally @x@ should be a primitive
-- data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnf whnf>@.
--
-- @since 0.1.0.0
whnf :: (a -> b) -> a -> Benchmarkable
whnf = fmap toBenchmarkable . whnf'

-- | 'nfIO' @x@ measures time to evaluate side-effects of @x@ and
-- compute its normal form (by means of 'force', not
-- 'Control.DeepSeq.rnf').
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nfIO nfIO>@.
--
-- @since 0.1.0.0
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = toBenchmarkable . ioToBench rnf

-- | 'whnfIO' @x@ measures time to evaluate side-effects of @x@ and
-- compute its weak head normal form.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnfIO whnfIO>@.
--
-- @since 0.1.0.0
whnfIO :: IO a -> Benchmarkable
whnfIO = toBenchmarkable . ioToBench id

-- | 'nfAppIO' @f@ @x@ measures time to evaluate side-effects of an
-- application of @f@ to @x@ and compute its normal form (by means of
-- 'force', not 'Control.DeepSeq.rnf').  This does not include time to
-- evaluate @f@ or @x@ themselves.  Ideally @x@ should be a primitive
-- data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:nfAppIO nfAppIO>@.
--
-- @since 0.1.0.0
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = fmap toBenchmarkable . ioFuncToBench rnf

-- | 'whnfAppIO' @f@ @x@ measures time to evaluate side-effects of an
-- application of @f@ to @x@ and compute its weak head normal form.
-- This does not include time to evaluate @f@ or @x@ themselves.
-- Ideally @x@ should be a primitive data type like 'Data.Int.Int'.
--
-- Drop-in replacement for @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:whnfAppIO whnfAppIO>@.
--
-- @since 0.1.0.0
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = fmap toBenchmarkable . ioFuncToBench id

-- | Run a benchmark interactively, providing an interface compatible with
-- @Criterion.<https://hackage.haskell.org/package/criterion/docs/Criterion.html#v:benchmark benchmark>@.
--
-- @since 0.1.0.0
benchmark :: Benchmarkable -> IO ()
benchmark = void . flip runBenchmark defaultMEnv . bench "..."

-- | Run benchmarks and report results, providing an interface
-- compatible with @Criterion.Main.<https://hackage.haskell.org/package/criterion/docs/Criterion-Main.html#v:defaultMain defaultMain>@.
--
-- @since 0.1.0.0
defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
--
-- @since 0.1.2.1
defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith cfg bs = do
  let act = defaultMainWith' cfg bs
  setLocaleEncoding utf8
#if defined(mingw32_HOST_OS)
  codePage <- getConsoleOutputCP
  bracket (setConsoleOutputCP 65001) (\_ -> setConsoleOutputCP codePage)
          (const act)
#else
  act
#endif
{-# INLINABLE defaultMainWith #-}

-- | Default configuration used for running benchmarks.
--
-- @since 0.1.2.1
defaultConfig :: Config
defaultConfig = Config
  { cfgUseColor = Auto
  , cfgMatchMode = Prefix
  , cfgTimeout = NoTimeout
  , cfgInterval = 0.95
  , cfgResamples = 1000
  , cfgRelStdDev = 0.05
  , cfgVerbosity = 1
  , cfgBaselinePath = Nothing
  , cfgCsvPath = Nothing
  , cfgJsonPath = Nothing
  , cfgReportPath = Nothing
  , cfgFailIfFaster = 1.0 / 0.0
  , cfgFailIfSlower = 1.0 / 0.0
  }

-- $miniterion_specific
--
-- Data types for Miniterion specific configuration.
--
-- The t'Config' and some of the data used in the fields of t'Config'
-- are Miniterion specific data type. Their purpose and name may
-- overlap with those of other benchmark packages, but they could not
-- used as a drop-in replacement.
--
-- The data type t'Config' has the same name as
-- @Criterion.Types.<https://hackage.haskell.org/package/criterion/docs/Criterion-Types.html#t:Config Config>@,
-- but the implementation is different.

-- | Data type to hold configuration information.
--
-- @since 0.1.2.1
data Config = Config
  { cfgUseColor     :: !UseColor
    -- ^ When to use colored outputs.
  , cfgMatchMode    :: !MatchMode
    -- ^ Which mode to use for benchmark name pattern match.
  , cfgTimeout      :: !Timeout
    -- ^ Timeout duration in seconds.
  , cfgInterval     :: !Double
    -- ^ Confidence interval
  , cfgResamples    :: !Word64
    -- ^ Number of bootstrap resamples to perform
  , cfgRelStdDev    :: !Double
    -- ^ Relative standard deviation for terminating benchmarks.
  , cfgVerbosity    :: !Int
    -- ^ Verbosity level.
  , cfgBaselinePath :: !(Maybe FilePath)
    -- ^ Path to a file containing baseline data, usually a CSV file
    -- made with @--csv@ option in advance.
  , cfgCsvPath      :: !(Maybe FilePath)
    -- ^ Path to a file for writing results in CSV format.
  , cfgJsonPath     :: !(Maybe FilePath)
    -- ^ Path to a file for writing JSON summary.
  , cfgReportPath   :: !(Maybe FilePath)
    -- ^ Path to a file for writing HTML report.
  , cfgFailIfFaster :: Double
    -- ^ Upper bound of acceptable speed up.
  , cfgFailIfSlower :: Double
    -- ^ Upper bound of acceptable slow down.
  }

-- | When to use colored output.
--
-- @since 0.1.2.1
data UseColor
  = Always -- ^ Always use color.
  | Auto   -- ^ Use color if the output is a terminal device.
  | Never  -- ^ Don't use color.

-- | Data type to express how to match benchmark names.
--
-- @since 0.1.2.1
data MatchMode
  = Pattern  -- ^ Substring match.
  | Prefix   -- ^ Prefix match.
  | IPattern -- ^ Case insensitive prefix match.
  | Glob     -- ^ Glob pattern match.

-- | Express duration for timeout.
--
-- @since 0.1.2.1
data Timeout
  = Timeout !Double
  -- ^ Duration in seconds.
  | NoTimeout
  -- ^ Run without timeout.


-- ------------------------------------------------------------------------
-- Main
-- ------------------------------------------------------------------------

-- | Mode to execute the main function.
data RunMode
  = Help           -- ^ Show help message.
  | Version        -- ^ Show version info.
  | DoList         -- ^ Show benchmark names.
  | DoIter !Word64 -- ^ Run benchmarks for given repeat count, don't analyse.
  | DoBench        -- ^ Run benchmarks.

defaultMainWith' :: Config -> [Benchmark] -> IO ()
defaultMainWith' cfg0 bs = handleMiniterionException $ do
  args <- getArgs
  let (!opts, !pats, invalids, errs) = getOpt' Permute options args
      O !cfg1 !run_mode = foldl' (flip id) (O cfg0 DoBench) opts
  default_menv <- getDefaultMEnv cfg1
  let menv0 = default_menv {mePatterns = pats}
      root_bs = bgroup "" bs
      do_iter n = iterBenchmark n root_bs menv0
      do_bench = runBenchmark root_bs
      with_handles = withCsvSettings . withJSONSettings
      invalid o = "invalid option `" ++ o ++ "'\n"
      exit_with f os = do
        me <- getProgName
        die (concatMap (\o -> me ++ ": " ++ f o) os ++ briefUsageOf me)
      print_match n = when (isMatched menv0 n) (putStrLn n)
      show_names = mapM_ print_match (benchNames [] root_bs)
  case run_mode of
    Help     -> showHelp menv0
    _         | not (null errs)     -> exit_with id errs
              | not (null invalids) -> exit_with invalid invalids
    Version  -> putStrLn builtWithMiniterion
    DoList   -> show_names
    DoIter n -> do_iter n >>= summariseResults
    DoBench  -> with_handles do_bench menv0 >>= summariseResults

showHelp :: MEnv -> IO ()
showHelp menv = do
  me <- fmap fromString getProgName
  putStr $ (`usageInfo` options) $ docToString menv $ mconcat
    [ "Microbenchmark suite - " <> stringToDoc builtWithMiniterion <> "\n\n"
    , boldYellow "USAGE:" <> " " <> boldGreen me <> " [OPTIONS] [PATTERN]...\n\n"
    , boldYellow "ARGS:\n"
    , "  <PATTERN>...  Pattern(s) to select running benchmarks. If no pattern was\n"
    , "                given, run all benchmarks. Multiple patterns are combined\n"
    , "                with 'OR'. Selections are done by prefix match by default.\n"
    , "                See also \"--match\" option below.\n\n"
    , boldYellow "OPTIONS:"
    ]

#ifndef VERSION_miniterion
#define VERSION_miniterion "unknown version"
#endif

builtWithMiniterion :: String
builtWithMiniterion = "built with miniterion " ++ VERSION_miniterion

briefUsageOf :: String -> String
briefUsageOf me = "Try `" ++ me ++ " --help' for more information."


-- ------------------------------------------------------------------------
-- Miniterion's environment
-- ------------------------------------------------------------------------

-- | Internal environment for miniterion.
data MEnv = MEnv
  { meConfig          :: Config
    -- ^ Configuration of this environment.
  , mePatterns        :: ![String]
    -- ^ Patterns to filter running benchmarks
  , meCsvHandle       :: !(Maybe Handle)
    -- ^ File handle to write benchmark result in CSV format.
  , meJsonHandle      :: !(Maybe Handle)
    -- ^ File handle to write benchmark result of JSON summary.
  , meBaseline        :: !(Maybe Baseline)
    -- ^ Set containing baseline information, made from the file
    -- specified by 'cfgBaselinePath'.
  , meUseColor        :: !Bool
    -- ^ 'True' if using colored output.
  , meSupportsUnicode :: !Bool
    -- ^ 'True' if unicode is supported.
  , meHasRTSStats     :: !Bool
    -- ^ 'True' if GC statistics are available.
  , meTimeout         :: !(Maybe Word64)
    -- ^ 'Just' timeout duration in picoseconds, or 'Nothing' if
    -- running benchmarks without timeout.
  }

-- | The default environment.
defaultMEnv :: MEnv
defaultMEnv = MEnv
  { meCsvHandle = Nothing
  , meJsonHandle = Nothing
  , meBaseline = Nothing
  , mePatterns = []
  , meConfig = defaultConfig
  , meUseColor = False
  , meSupportsUnicode = False
  , meHasRTSStats = False
  , meTimeout = Nothing
  }
{-# INLINABLE defaultMEnv #-}

-- | Get the default t'MEnv' from given t'Config'.
getDefaultMEnv :: Config -> IO MEnv
getDefaultMEnv !cfg = do
  use_color <- case cfgUseColor cfg of
    Always -> pure True
    Never  -> pure False
    Auto   -> hIsTerminalDevice stdout
  supports_unicode <- isUnicodeSupported
  has_rts_stats <- getRTSStatsEnabled
  pure $! defaultMEnv
    { meConfig = cfg
    , meUseColor = use_color
    , meSupportsUnicode = supports_unicode
    , meHasRTSStats = has_rts_stats
    , meTimeout = case cfgTimeout cfg of
        Timeout secs -> Just (truncate (secs * 1e12))
        NoTimeout    -> Nothing
    }
{-# INLINABLE getDefaultMEnv #-}

-- | A monad to run 'IO' actions with t'MEnv', basically same as
-- @ReaderT MEnv IO@.
newtype Miniterion a = Miniterion {runMiniterion :: MEnv -> IO a}

instance Functor Miniterion where
  fmap f (Miniterion r) = Miniterion (fmap f . r)
  {-# INLINE fmap #-}

instance Applicative Miniterion where
  pure x = Miniterion (const (pure x))
  {-# INLINE pure #-}

  Miniterion f <*> Miniterion m = Miniterion (\e -> f e <*> m e)
  {-# INLINE (<*>) #-}

instance Monad Miniterion where
  Miniterion r >>= k = Miniterion (\e -> r e >>= \a -> runMiniterion (k a) e)
  {-# INLINE (>>=) #-}

instance MonadIO Miniterion where
  liftIO io = Miniterion (const io)
  {-# INLINE liftIO #-}

getMEnv :: Miniterion MEnv
getMEnv = Miniterion pure
{-# INLINE getMEnv #-}


-- ------------------------------------------------------------------------
-- Result
-- ------------------------------------------------------------------------

data Result
  = Done -- ^ Successfully finished running the benchmark.
  | Compared PassFail Change -- ^ Compared against baseline.
  | TimedOut String -- ^ Timed out.

data PassFail = Pass | Fail

data Change
  = Negligible -- ^ No outstanding change.
  | Slower String !Int64 -- ^ Slower than the baseline.
  | Faster String !Int64 -- ^ Faster than the baseline.

summariseResults :: [Result] -> IO ()
summariseResults rs = do
  let (!num_result, !num_failed) = foldl' f z rs
      z :: (Int, Int)
      z = (0, 0)
      f (!done, !fl) = \case
        Done            -> (done + 1, fl)
        Compared Pass _ -> (done + 1, fl)
        _               -> (done + 1, fl + 1)
      bs | 1 < num_result = "benchmarks"
         | otherwise = "benchmark" :: String
      pr (name, why) = putStrLn ("  - " ++ name ++ " (" ++ why ++ ")")
  when (0 < num_failed) $ do
    printf "\n%d out of %d %s failed:\n" num_failed num_result bs
    mapM_ (mapM_ pr . failedNameAndReason) (reverse rs)
    exitFailure
{-# INLINABLE summariseResults #-}

failedNameAndReason :: Result -> Maybe (String, String)
failedNameAndReason = \case
  TimedOut name                 -> Just (name, "timed out")
  Compared Fail (Slower name _) -> Just (name, "too slow")
  Compared Fail (Faster name _) -> Just (name, "too fast")
  _                             -> Nothing
{-# INLINE failedNameAndReason #-}


-- ------------------------------------------------------------------------
-- Running benchmarks
-- ------------------------------------------------------------------------

runBenchmark :: Benchmark -> MEnv -> IO [Result]
runBenchmark = runBenchmarkWith runBenchmarkable

iterBenchmark :: Word64 -> Benchmark -> MEnv -> IO [Result]
iterBenchmark n = runBenchmarkWith (iterBenchmarkable n)

runBenchmarkWith :: (Int -> String -> Benchmarkable -> Miniterion a)
                 -> Benchmark -> MEnv -> IO [a]
runBenchmarkWith !run b menv = fst <$> runMiniterion (go [] 0 b) menv
  where
    -- Benchmarks are always wrapped with the root group in
    -- defaultMainWith', selecting the benchmarks to run in Bgroup's
    -- case.
    go !parents !i bnch = case bnch of
      Bench name act -> do
        r <- run i (pathToName parents name) act
        pure ([r], i+1)
      Bgroup name bs -> do
        let !parents' = consNonNull name parents
            f (!rs, !j) !bnch'
              | any (isMatched menv) (benchNames parents' bnch') = do
                  (!rs', j') <- go parents' j bnch'
                  pure (rs' ++ rs, j')
              | otherwise = pure (rs, j)
        foldlM f ([],i) bs
      Environment !alloc !clean f -> liftIO $ do
        e <- alloc >>= \e -> evaluate (rnf e) >> pure e
        runMiniterion (go parents i (f e)) menv `finally` clean e

runBenchmarkable :: Int -> String -> Benchmarkable -> Miniterion Result
runBenchmarkable idx fullname b = do
  menv@MEnv{meConfig=cfg@Config{..}, ..} <- getMEnv
  putBenchname fullname
  debug "\n"
  liftIO $ hFlush stdout
  mb_sum <- liftIO $ withTimeout meTimeout (measureUntil menv b)
  let (result, summary) = case mb_sum of
        Nothing -> (TimedOut fullname, emptySummary)
        Just s  -> (compareVsBaseline meBaseline cfg fullname s, s)
  info (formatSummary result summary)
  liftIO $ do
    mapM_ (putCsvLine meHasRTSStats fullname summary) meCsvHandle
    mapM_ (putJSONObject idx fullname cfgInterval summary) meJsonHandle
  pure result

iterBenchmarkable :: Word64 -> Int -> String -> Benchmarkable
                  -> Miniterion Result
iterBenchmarkable n _idx fullname b = do
  MEnv{..} <- getMEnv
  putBenchname fullname
  liftIO $ hFlush stdout
  mb_unit <- liftIO $ withTimeout meTimeout (runLoop b n id)
  case mb_unit of
    Just () -> info "\n" >> pure Done
    _ -> do
      let result = TimedOut fullname
      info (formatSummary result emptySummary)
      pure result

putBenchname :: String -> Miniterion ()
putBenchname name = info (white "benchmarking " <> boldCyan (fromString name))
{-# INLINE putBenchname #-}

withTimeout :: Maybe Word64 -> IO a -> IO (Maybe a)
withTimeout tout io = case tout of
  Just pico -> timeout (picoToMicroSecWI pico) io
  Nothing   -> fmap Just io

benchNames :: [String] -> Benchmark -> [String]
benchNames = go
  where
    go !acc b = case b of
      Bench name _      -> [pathToName acc name]
      Bgroup name bs    -> concatMap (go (consNonNull name acc)) bs
      Environment _ _ f -> go acc (f (throw (UninitializedEnv acc)))

pathToName :: [String] -> String -> String
pathToName !prevs !me = foldl' (\b a -> a ++ "/" ++ b) me prevs

groupsToName :: [String] -> String
groupsToName = \case
  []    -> ""
  hd:tl -> pathToName tl hd

consNonNull :: String -> [String] -> [String]
consNonNull !x !xs = if null x then xs else x : xs

noop :: Applicative m => a -> m ()
noop = const (pure ())
{-# INLINE noop #-}


-- ------------------------------------------------------------------------
-- Printing with verbosity
-- ------------------------------------------------------------------------

info, _verbose, debug :: Doc -> Miniterion ()
info = Miniterion . flip info'
_verbose = Miniterion . flip verbose'
debug = Miniterion . flip debug'

info', verbose', debug' :: MEnv -> Doc -> IO ()
info' = putDocWith 1
verbose' = putDocWith 2
debug' = putDocWith 3

putDocWith :: Int -> MEnv -> Doc -> IO ()
putDocWith n menv doc =
  when (n <= cfgVerbosity (meConfig menv)) $ putDoc menv doc

isVerbose :: MEnv -> Bool
isVerbose e = 1 < cfgVerbosity (meConfig e)
{-# INLINABLE isVerbose #-}


-- ------------------------------------------------------------------------
-- Formatting
-- ------------------------------------------------------------------------

formatSummary :: Result -> Summary -> Doc
formatSummary (TimedOut _) _ =
  boldRed " FAIL" <> "\n" <>
  yellow "Timed out while running this benchmark\n\n"
formatSummary res (Summary{..}) =
  formatChange res <> "\n" <>
  --
  white "time                 " <> formatRanged smOLS <> "\n" <>
        "                     " <> formatR2 smR2 <> "\n" <>
  white "mean                 " <> formatRanged smMean <> "\n" <>
  white "std dev              " <> formatRanged smStdDev <>
  --
  formatOutliers smOutliers <>
  formatOutlierVariance smOutlierVar <>
  formatGC smMeasurement <> "\n\n"

formatChange :: Result -> Doc
formatChange = \case
  Compared pf change -> padl <> fmt pf change
  _                  -> ""
  where
    padl = Doc (\ !menv -> if isVerbose menv then "\n" else " ")
    fmt pf = \case
      Negligible -> white "(same as baseline)"
      Faster _ p -> more_or_less "less" p
      Slower _ p -> more_or_less "more" p
      where
        more_or_less which p =
          g ("(" <> showDoc p <> "% " <> which <> " than baseline)")
        g = case pf of
          Fail -> (boldRed "FAIL " <>) . yellow
          Pass -> white

formatRanged :: Ranged -> Doc
formatRanged (Ranged lo mid hi) =
  showPicos5 mid <> "   " <>
  white ("(" <> showPicos5 lo <> " .. " <> showPicos5 hi <> ")")

formatR2 :: Ranged -> Doc
formatR2 (Ranged lo mid hi) =
  fmt id mid <> "   " <>
  white "(" <> fmt white lo <> white " .. " <> fmt white hi <> white ")"
  where
    fmt on_other !val = color (stringToDoc (printf "%.3f R²" val))
      where
        !color | val < 0.90 = boldRed
               | val < 0.99 = yellow
               | otherwise  = on_other

formatOutlierVariance :: OutlierVariance -> Doc
formatOutlierVariance (OutlierVariance !oe _ frac) = Doc $ \ !menv ->
  let show_oe effect =
        white "\nvariance introduced by outliers: " <>
        stringToDoc (printf "%2d%% " (round (frac * 100) :: Int)) <>
        white ("(" <> effect <>")")
  in  docToString menv $ case oe of
    Unaffected | isVerbose menv -> show_oe "unaffected"
    Slight | isVerbose menv     -> show_oe "slightly inflated"
    Moderate                    -> show_oe "moderately inflated"
    Severe                      -> show_oe "severely inflated"
    _                           -> ""

-- Only shown when verbose.
formatOutliers :: Outliers -> Doc
formatOutliers ~(Outliers seen ls lm hm hs) = Doc $ \ !menv ->
  if isVerbose menv && 0 < os then
    docToString menv msg
  else
    ""
  where
    os = ls + lm + hm + hs
    frac n = (100::Double) * fromIntegral n / fromIntegral seen
    msg =
      "\n" <> white "found " <> showDoc os <>
      white " outliers among " <> showDoc seen <>
      white " samples (" <> stringToDoc (printf "%.1g%%" (frac os)) <>
      white ")" <>
      f ls "low severe" <>
      f lm "low mild" <>
      f hm "high mild" <>
      f hs "high severe"
    f n what =
      if 0 < n then
        "\n  " <> showDoc n <> white " (" <>
        stringToDoc (printf "%.1g%%" (frac n)) <> white ") " <>
        white what
      else
        ""

-- Only shown when collecting RTS stats.
formatGC :: Measurement -> Doc
formatGC ~(Measurement {measAllocs=a, measCopied=c, measMaxMem=p}) =
  Doc $ \ !e ->
  if meHasRTSStats e then
    let sb !b = fromString $! showBytes b
    in  docToString e $ "\n" <>
        white "        alloc  copied    peak" <> "\n" <>
        white "gc     " <> sb a <> "  " <> sb c <> "  " <> sb p
  else
    ""

formatBootstrap :: Word64 -> Word64 -> Word64 -> Word64 -> Doc
formatBootstrap dur nresample nvalid nmeas =
  white "\nmeasurement took " <> showPicos5 (word64ToDouble dur) <> "\n" <>
  white "analysing with " <> showDoc nresample <> white " resamples\n" <>
  white "bootstrapping with " <> showDoc nvalid <> white " of " <>
  showDoc nmeas <> white " samples (" <> showDoc percent <> "%)"
  where
    percent :: Int
    percent =
      truncate ((fromIntegral nvalid / fromIntegral nmeas :: Double) * 100)

formatMeasurement :: Measurement -> Double -> Double -> Doc
formatMeasurement (Measurement n t _ a c m) mean sd =
  showDoc n <>
  (if n == 1 then " iteration gives " else " iterations give ") <>
  showDoc t <> " (" <>
  showPicos5 (word64ToDouble (t `quot` n)) <> "/run) " <>
  "mean: " <> showPicos5 mean <> ", sd: " <> showPicos5 sd <>
  " (" <> stringToDoc (printf "%.2f%%" (100*sd/mean)) <> ")" <>
  Doc (\ !menv ->
         if meHasRTSStats menv then
           printf " alloc: %d copied: %d max: %d" a c m
         else
           "") <>
  "\n"

-- | Show picoseconds, fitting number in 5 characters.
showPicos5 :: Double -> Doc
showPicos5 t
  | t < 10     = f $ printf "%.3f ps" t
  | t < 100    = f $ printf "%.2f ps" t
  | t < 1000   = f $ printf "%.1f ps" t
  | t < 999e1  = f $ printf "%.3f ns" (t / 1e3)
  | t < 999e2  = f $ printf "%.2f ns" (t / 1e3)
  | t < 999e3  = f $ printf "%.1f ns" (t / 1e3)
  | t < 999e4  = print_mu "%.3f %cs"
  | t < 999e5  = print_mu "%.2f %cs"
  | t < 999e6  = print_mu "%.1f %cs"
  | t < 999e7  = f $ printf "%.3f ms" (t / 1e9)
  | t < 999e8  = f $ printf "%.2f ms" (t / 1e9)
  | t < 999e9  = f $ printf "%.1f ms" (t / 1e9)
  | t < 999e10 = f $ printf "%.3f s " (t / 1e12)
  | t < 999e11 = f $ printf "%.2f s " (t / 1e12)
  | t < 999e12 = f $ printf "%.1f s " (t / 1e12)
  | otherwise  = f $ printf "%4.1f s" (t / 1e12)
  where
    f = fromString
    print_mu fmt = Doc (printf fmt (t / 1e6) . mu)

-- | Show bytes with unit.
showBytes :: Word64 -> String
showBytes i
  | t < 1000                 = printf " %3.0f B" t
  | t < 10189                = printf "%3.1f KB" (t / 1024)
  | t < 1023488              = printf "%3.0f KB" (t / 1024)
  | t < 10433332             = printf "%3.1f MB" (t / 1048576)
  | t < 1048051712           = printf "%3.0f MB" (t / 1048576)
  | t < 10683731149          = printf "%3.1f GB" (t / 1073741824)
  | t < 1073204953088        = printf "%3.0f GB" (t / 1073741824)
  | t < 10940140696372       = printf "%3.1f TB" (t / 1099511627776)
  | t < 1098961871962112     = printf "%3.0f TB" (t / 1099511627776)
  | t < 11202704073084108    = printf "%3.1f PB" (t / 1125899906842624)
  | t < 1125336956889202624  = printf "%3.0f PB" (t / 1125899906842624)
  | t < 11471568970838126592 = printf "%3.1f EB" (t / 1152921504606846976)
  | otherwise                = printf "%3.0f EB" (t / 1152921504606846976)
  where
    t = word64ToDouble i


-- ------------------------------------------------------------------------
-- Matching benchmark names
-- ------------------------------------------------------------------------

isMatched :: MEnv -> String -> Bool
isMatched MEnv{..} fullname = null mePatterns || has_match
  where
    has_match = any is_match mePatterns
    is_match str = case cfgMatchMode meConfig of
      Glob     -> glob str fullname
      IPattern -> substring (map toLower str) (map toLower fullname)
      Pattern  -> substring str fullname
      Prefix   -> str `isPrefixOf` fullname

substring :: String -> String -> Bool
substring pat = any (pat `isPrefixOf`) . tails

-- | Simple, inefficient, and improper glob. Does not support special
-- character class names like @[:alnum:]@, @[:digit:]@, ... etc.
glob :: String -> String -> Bool
glob pat0 = go pat0
  where
    go [] [] = True
    go ('\\':p:ps) (c:cs) | p == c = go ps cs
    go ('?':ps) (_:cs) = go ps cs
    go ['*'] _ = True
    go ('*':ps) cs = any (go ps) (cs : tails cs)
    go ('[':'!':ps) (c:cs) = cclass notElem c ps cs
    go ('[':ps) (c:cs) = cclass elem c ps cs
    go ('{':ps) cs = brace ps cs
    go (p:ps) (c:cs) | p == c = go ps cs
    go _ _ = False

    cclass test c ps cs = lp False [] ps
      where
        lp close acc xs =
          case xs of
            []              -> throw (GlobUnbalancedBracket pat0)
            '\\':x:xs'      -> lp True (x:acc) xs'
            ']':xs' | close -> test c acc && go xs' cs
            x0:'-':']':xs'  -> test c ('-':x0:acc) && go xs' cs
            x0:'-':x1:xs'   -> lp True ([x0 .. x1] ++ acc) xs'
            x:xs'           -> lp True (x:acc) xs'

    brace ps cs = any (\p -> go (p ++ ps') cs) pats
      where
        (pats, ps') = alts (0 :: Int) [] [] ps
        alts depth tmp acc xs = case xs of
          []         -> throw (GlobUnbalancedBrace pat0)
          '\\':x:xs' -> alts depth (x:'\\':tmp) acc xs'
          x:xs'      -> case x of
            '}' | depth == 0 -> (reverse (reverse tmp : acc), xs')
                | otherwise  -> alts (depth - 1) (x:tmp) acc xs'
            '{'              -> alts (depth + 1) (x:tmp) acc xs'
            ',' | depth == 0 -> alts depth [] (reverse tmp : acc) xs'
            _other           -> alts depth (x:tmp) acc xs'


-- ------------------------------------------------------------------------
-- Terminal stuffs
-- ------------------------------------------------------------------------

yellow, white :: Doc -> Doc
yellow = coloredDoc "0;33"
white = coloredDoc "0;37"

boldRed, boldGreen, boldYellow, boldCyan :: Doc -> Doc
boldRed = coloredDoc "1;31"
boldGreen = coloredDoc "1;32"
boldYellow = coloredDoc "1;33"
boldCyan = coloredDoc "1;36"

coloredDoc :: String -> Doc -> Doc
coloredDoc !param (Doc !g) = Doc f
  where
    f !e
      | meUseColor e = "\ESC[" ++ param ++ "m" ++ g e ++ "\ESC[0m"
      | otherwise = g e
{-# INLINABLE coloredDoc #-}

-- | Unit character for microseconds.
mu :: MEnv -> Char
mu menv = if meSupportsUnicode menv then 'μ' else 'u'
{-# INLINE mu #-}

isUnicodeSupported :: IO Bool
isUnicodeSupported = do
  enc <- getLocaleEncoding
  let utf_prefix = take 3 (textEncodingName enc) == "UTF"
#if defined(mingw32_HOST_OS)
  is_65001 <- fmap (== 65001) getConsoleOutputCP
  pure (utf_prefix && is_65001)
#else
  pure utf_prefix
#endif
{-# INLINABLE isUnicodeSupported #-}


-- ------------------------------------------------------------------------
-- Terminal specific string
-- ------------------------------------------------------------------------

-- | Newtype wrapper for 'String' taking t'MEnv', to decide whether to
-- use color and unicode.
newtype Doc = Doc {unDoc :: MEnv -> String}

instance Semigroup Doc where
  Doc !d1 <> Doc !d2 = Doc (\ !e -> d1 e <> d2 e)
  {-# INLINE (<>) #-}

instance Monoid Doc where
  mempty = Doc (const "")
  {-# INLINE mempty #-}

instance IsString Doc where
  fromString = stringToDoc
  {-# INLINE fromString #-}

-- | Lift given 'String' to t'Doc'.
stringToDoc :: String -> Doc
stringToDoc !str = Doc (\ !_ -> str)
{-# INLINE stringToDoc #-}

-- | Convert t'Doc' to 'String'.
docToString :: MEnv -> Doc -> String
docToString !menv !d = unDoc d menv
{-# INLINE docToString #-}

-- | Like 'putStr', but for t'Doc'.
putDoc :: MEnv -> Doc -> IO ()
putDoc !menv = putStr . docToString menv
{-# INLINE putDoc #-}

-- | Apply 'show' and then convert to t'Doc'.
showDoc :: Show a => a -> Doc
showDoc = stringToDoc . show
{-# INLINE showDoc #-}


-- ------------------------------------------------------------------------
-- CSV
-- ------------------------------------------------------------------------

-- XXX: Could use `Data.Map.Map String (Double,Double)'.

type Baseline = [CsvEntry]

data CsvEntry = CsvEntry
  { ceName   :: !String
  , ceMean   :: !Double
  , ceStdDev :: !Double
  }

instance NFData CsvEntry where
  rnf (CsvEntry name mean stddev) = rnf name `seq` mean `seq` stddev `seq` ()
  {-# INLINE rnf #-}

withCsvSettings :: (MEnv -> IO a) -> MEnv -> IO a
withCsvSettings !act menv0@MEnv{meConfig=cfg} = do
  baseline <- maybe mempty (fmap Just . readBaseline) (cfgBaselinePath cfg)
  let menv1 = menv0 {meBaseline = baseline}
  case cfgCsvPath cfg of
    Nothing -> act menv1 {meCsvHandle = Nothing}
    Just path -> withFile path WriteMode $ \hdl -> do
      hSetBuffering hdl LineBuffering
      let extras | meHasRTSStats menv0 = ",Allocated,Copied,Peak Memory"
                 | otherwise = ""
          header = "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB"
      hPutStrLn hdl (header ++ extras)
      act menv1 {meCsvHandle = Just hdl}

putCsvLine :: Bool -> String -> Summary -> Handle -> IO ()
putCsvLine has_gc name summary hdl =
  hPutStrLn hdl (encodeCsv name ++ "," ++ csvSummary has_gc summary)

csvSummary :: Bool -> Summary -> String
csvSummary has_gc (Summary {smMeasurement=m, ..})
  | has_gc    = time ++ "," ++ gc
  | otherwise = time
  where
    time =
      -- Mean, Mean lower bound, Mean upper bound
      show mm ++ "," ++ show ml ++ "," ++ show mh ++ "," ++
      -- Stddev, Stddev lower bound, Stddev upper bound
      show sm ++ "," ++ show sl ++ "," ++ show sh
      where
        Ranged ml mm mh = mapRanged picoToSecD smMean
        Ranged sl sm sh = mapRanged picoToSecD smStdDev
    gc =
      show (measAllocs m) ++ "," ++ show (measCopied m) ++ "," ++
      show (measMaxMem m)

readBaseline :: FilePath -> IO Baseline
readBaseline path = handle handler go
  where
    handler :: SomeException -> IO a
    handler _ = throwIO (CannotReadFile (Just "baseline") path)
    go = readFile path >>= evaluate . force .
         mapMaybe parseCsvEntry . joinQuotedFields . lines

joinQuotedFields :: [String] -> [String]
joinQuotedFields [] = []
joinQuotedFields (x : xs)
  | areQuotesBalanced x = x : joinQuotedFields xs
  | otherwise = case span areQuotesBalanced xs of
    (_, [])      -> [] -- malformed CSV
    (ys, z : zs) -> unlines (x : ys ++ [z]) : joinQuotedFields zs
  where
    areQuotesBalanced = even . length . filter (== '"')

compareVsBaseline :: Maybe Baseline -> Config -> String -> Summary -> Result
compareVsBaseline mb_baseline Config{..} name summary =
  maybe Done comp (mb_baseline >>= find ((== name) . ceName))
  where
    comp (CsvEntry {ceMean=old_mean, ceStdDev=old_stddev})
      | negligible  = Compared Pass Negligible
      | percent < 0 = Compared pf (Faster name (-percent))
      | otherwise   = Compared pf (Slower name percent)
      where
        negligible = abs (mean - old_mean) < min stddev old_stddev
        percent = truncate ((ratio - 1) * 100)
        pf | 1 + cfgFailIfSlower <= ratio = Fail
           | ratio <= 1 - cfgFailIfFaster = Fail
           | otherwise                    = Pass
        ratio = mean / old_mean
        mean = picoToSecD (irMid (smMean summary))
        stddev = picoToSecD (irMid (smStdDev summary))

encodeCsv :: String -> String
encodeCsv xs
  | any (`elem` xs) (",\"\n\r" :: String) = '"' : go xs -- opening quote
  | otherwise = xs
  where
    go []         = ['"'] -- closing quote
    go ('"' : ys) = '"' : '"' : go ys
    go (y : ys)   = y : go ys


-- ------------------------------------------------------------------------
-- Parser
-- ------------------------------------------------------------------------

newtype P a = P {runP :: String -> Maybe (a, String)}

instance Functor P where
  fmap f (P p) = P (p >=> \(a,s') -> pure (f a,s'))
  {-# INLINE fmap #-}

instance Applicative P where
  pure x = P (\s -> Just (x,s))
  {-# INLINE pure #-}
  P f <*> a = P (f >=> \(f',s') -> runP (fmap f' a) s')
  {-# INLINE (<*>) #-}

parseCsvEntry :: String -> Maybe CsvEntry
parseCsvEntry = fmap fst . runP p_csv_entry
  where
    p_csv_entry = CsvEntry <$> p_name <*>
                  (p_double <* p_cell <* p_cell) <*>
                  p_double
    p_cell = P $ \str -> case span (/= ',') str of
      (xs, ',':rest) -> pure (xs, rest)
      _              -> Nothing
    p_name = P $ \str -> case str of
      '"':rest -> decode [] rest
      _        -> runP p_cell str
      where
        decode !acc xs = case xs of
          '"':'"':rest -> decode ('"':acc) rest
          '"':',':rest -> pure (reverse acc, rest)
          x:rest       -> decode (x:acc) rest
          []           -> Nothing
    p_double = P $ \str -> do
      (cell, rest) <- runP p_cell str
      d <- readMaybe cell
      pure (d, rest)
{-# INLINE parseCsvEntry #-}


-- ------------------------------------------------------------------------
-- JSON
-- ------------------------------------------------------------------------

-- The JSON report made by Miniterion differs from the one made by
-- Criterion. Some of the values are missing (e.g., 'y' in regCoeffs,
-- GC related Measurement fields). Hope that the use of the same names
-- will help reusing the JSON parser between Miniterion and Criterion.

withJSONSettings :: (MEnv -> IO a) -> MEnv -> IO a
withJSONSettings !act menv@MEnv{meConfig=Config{..}} =
  -- When HTML report is specified without JSON output, writing JSON
  -- data to a temporary file.
  case cfgJsonPath of
    Just json -> do
      r <- withJSONFile json menv act
      mapM_ (writeReport json) cfgReportPath
      pure r
    Nothing | Just html <- cfgReportPath -> do
      r <- withJSONFile tmpJSONFile menv act
      writeReport tmpJSONFile html
      pure r
    _ -> act menv {meJsonHandle = Nothing}

-- | Temporary file to write JSON data for generating report when the
-- JSON path was not specified.
tmpJSONFile :: FilePath
tmpJSONFile = ".miniterion-tmp.json"
{-# INLINE tmpJSONFile #-}

withJSONFile :: FilePath -> MEnv -> (MEnv -> IO a) -> IO a
withJSONFile !file !menv !act =
  withFile file WriteMode $ \hdl -> do
    hSetBuffering hdl (BlockBuffering Nothing)
    hPutStr hdl $ "[\"miniterion\",\"" ++ VERSION_miniterion ++ "\",["
    act menv {meJsonHandle = Just hdl} `finally` hPutStr hdl "]]"

putJSONObject :: Int -> String -> Double -> Summary -> Handle -> IO ()
putJSONObject !idx !name !ci Summary{..} hdl = do
  when (idx /= 0) $ hPutStr hdl ","
  hPutStr hdl $
    "{\"reportAnalysis\":" ++ analysis ++
    ",\"reportKDEs\":" ++ kdes ++
    ",\"reportKeys\":" ++ keys ++
    ",\"reportMeasured\":" ++ measured ++
    ",\"reportName\":" ++ escapeJSON name ++
    ",\"reportNumber\":" ++ show idx ++
    ",\"reportOutliers\":" ++ outliers ++
    "}"
  where
    analysis =
      "{\"anMean\":" ++ est (mapRanged picoToSecD smMean) ++
      ",\"anOutlierVar\":" ++ variance ++
      ",\"anRegress\":[" ++ reg ++ "]" ++
      ",\"anStdDev\":" ++ est (mapRanged picoToSecD smStdDev) ++
      "}"
      where
        est (Ranged lo mid hi) =
          "{\"estError\":" ++ confInt ++
          ",\"estPoint\":" ++ show mid ++
          "}"
          where
            confInt =
              -- The `hi' value could be NaN for OLS and R^2. Fall
              -- back to 0 in such case.
              "{\"confIntCL\":" ++ show ci ++
              ",\"confIntLDX\":" ++ show (mid - lo) ++
              ",\"confIntUDX\":" ++ show (if isNaN hi then 0 else hi - mid) ++
              "}"
        variance =
          "{\"ovDesc\":\"" ++ ovDesc ++ "\"" ++
          ",\"ovEffect\":\"" ++ show ovEffect ++ "\"" ++
          ",\"ovFraction\":" ++ show ovFraction ++
          "}"
          where
            OutlierVariance{..} = smOutlierVar
        reg =
          "{\"regCoeffs\":" ++ coeffs ++
          ",\"regRSquare\":" ++ est smR2 ++
          ",\"regResponder\":\"time\"" ++
          "}"
          where
            coeffs =
              "{\"iters\":" ++ est (mapRanged picoToSecD smOLS) ++ "}"
    kdes =
      "[{\"kdePDF\":" ++ show (kdPDF smKDEs) ++
      ",\"kdeType\":\"time\"" ++
      ",\"kdeValues\":" ++ show (kdValues smKDEs) ++
      "}]"
    keys =
      -- See 'Criterion.Measurement.Types.measureAccessors_'
      "[\"time\",\"cpuTime\",\"cycles\",\"iters\"" ++
      ",\"allocated\",\"peakMbAllocated\",\"numGcs\",\"bytesCopied\"" ++
      ",\"mutatorWallSeconds\",\"mutatorCpuSeconds\"" ++
      ",\"gcWallSeconds\",\"gcCpuSeconds\"]"
    measured =
      "[" ++ intercalate "," (map meas_to_arr smMeasured) ++ "]"
      where
        meas_to_arr (Measurement n t p a c m) =
          -- time
          "[" ++ show (picoToSecW t) ++ "," ++
          -- cputTime, cycles, iters
          show (picoToSecW p) ++ ",0," ++ show n ++ "," ++
          -- allocated
          (if a == 0 then "null" else show a) ++ "," ++
          -- peakMbAllocated
          (if m == 0 then "null" else show (m `quot` 1000000)) ++ "," ++
          -- numGCs
          "null," ++
          -- bytesCopied
          (if c == 0 then "null" else show c) ++
          -- mutatorWallSeconds, mutatorCpuSeconds, gcWallSeconds, and
          -- gcCpuSeconds
          ",null,null,null,null]"
    outliers =
      "{\"highMild\":" ++ show otHighMild ++
      ",\"highSevere\":" ++ show otHighSevere ++
      ",\"lowMild\":" ++ show otLowMild ++
      ",\"lowSevere\":" ++ show otLowSevere ++
      ",\"samplesSeen\":" ++ show otSamplesSeen ++
      "}"
      where
        Outliers {..} = smOutliers

-- Simplified variant of Criterion.Report.escapeJSON for String
-- instead of Text. Does not escape plus character (@+@) and NULL
-- (@\0@).
escapeJSON :: String -> String
escapeJSON = ('"' :) . foldr f ['"']
  where
    f '\n'     = ("\\n" ++)
    f '\\'     = ("\\\\" ++)
    f '"'      = ("\\\"" ++)
    f '<'      = ("\\u003c" ++)
    f '>'      = ("\\u003e" ++)
    f '&'      = ("\\u0026" ++)
    f '\x2028' = ("\\u2028" ++) -- line separator
    f '\x2029' = ("\\u2029" ++) -- paragraph separator
    f c        = (c:)


-- ------------------------------------------------------------------------
-- HTML report
-- ------------------------------------------------------------------------

-- | Write HTML report from JSON data.
writeReport :: FilePath -- ^ Path of the input JSON file
            -> FilePath -- ^ Path of the HTML output file
            -> IO ()
#if IS_PACKAGE_BUILD
writeReport infile outfile = do
  template_path <- getDataFileName data_template_html
  withFile template_path ReadMode $ \ihdl ->
    withFile outfile WriteMode $ \ohdl ->
      let go = do
            is_eof <- hIsEOF ihdl
            unless is_eof $ do
              line <- hGetLine ihdl
              if trim line == "{{{json}}}"
                then hPutStrLn ohdl . second_element =<< readFile infile
                else hPutStrLn ohdl line
              go
      in  go
  where
    -- Path to the template HTML file for generating report, contains
    -- OS specific path separator. Could be done with (</>) defined in
    -- the 'filepath' package, but using CPP at the moment (using
    -- backslash on Windows, or slash otherwise).
#if defined(mingw32_HOST_OS)
    data_template_html = "data\\template.html"
#else
    data_template_html = "data/template.html"
#endif
    -- Simple white space removal to find embedded JSON mark.
    trim = takeWhile (/= ' ') . dropWhile (== ' ')

    -- Removing pre and post characters to get the second element of
    -- the JSON array. The number of characters before the beginning
    -- of the second element is known in advance
    -- (@["miniterion","w.x.y.z",@, 24 characters). The last closing
    -- bracket is removed with `init'.
    second_element = init . drop 24
#else
writeReport _ _ = do
  me <- getProgName
  putStrLn ("*** Writing HTML report is NOT supported in " <> me)
  putStrLn (me <> " was built with non-packaged version of Miniterion")
#endif


-- ------------------------------------------------------------------------
-- Command line options
-- ------------------------------------------------------------------------

data Opts = O !Config !RunMode

options :: [OptDescr (Opts -> Opts)]
options =
  [ Option ['h'] ["help"]
    (NoArg (\(O c _) -> O c Help))
    "Show this help text"

  , Option ['I'] ["ci"]
    (ReqArg (\str (O c m) -> case readRanged "ci" 1.0e-3 0.999 str of
                Right n  -> O (c {cfgInterval=n}) m
                Left err -> throw err)
     "CI")
    "Confidence interval (default: 0.95)"

  , Option ['L'] ["time-limit"]
    (ReqArg (\str (O c m) -> case readMaybe str :: Maybe Double of
                Just n -> O (c {cfgTimeout = Timeout n}) m
                _      -> throw (InvalidArgument "time-limit" str))
     "SECS")
    "Time limit to run a benchmark\n(default: no timeout)"

  , Option [] ["resamples"]
    (ReqArg (\str (O c m) -> case readRanged "resamples" 1 1000000 str of
                Right n  -> O (c {cfgResamples=n}) m
                Left err -> throw err)
    "COUNT")
    "Number of bootstrap resamples to perform\n(default: 1000)"

  , Option ['o'] ["output"]
    (ReqArg (\str (O c m) -> O (c {cfgReportPath = Just str}) m)
    "FILE")
    "File to write report to"

  , Option [] ["csv"]
    (ReqArg (\str (O c m) -> O (c {cfgCsvPath = Just str}) m)
     "FILE")
    "File to write CSV summary to"

  , Option [] ["json"]
    (ReqArg (\str (O c m) -> O (c {cfgJsonPath = Just str}) m)
    "FILE")
    "File to write JSON summary to"

  , Option [] ["baseline"]
    (ReqArg (\str (O c m) -> O (c {cfgBaselinePath = Just str}) m)
    "FILE")
    "File to read CSV summary from as baseline"

  , Option [] ["fail-if-faster"]
    (ReqArg (\str (O c m) -> case readPositivePercents str of
                Just x -> O (c {cfgFailIfFaster = x}) m
                _      -> throw (InvalidArgument "fail-if-faster" str))
      "NUM")
    (unlines
     ["Upper bound acceptable speed up in percents. If a"
     ,"benchmark is unacceptable faster than baseline (see"
     ,"--baseline), it will be reported as failed"])

  , Option [] ["fail-if-slower"]
    (ReqArg (\str (O c m) -> case readPositivePercents str of
                Just x -> O (c {cfgFailIfSlower = x}) m
                _      -> throw (InvalidArgument "fail-if-slower" str))
      "NUM")
    (unlines
     ["Upper bound acceptable slow down in percents. If a"
     ,"benchmark is unacceptable slower than baseline (see"
     ,"--baseline), it will be reported as failed"])

  , Option [] ["color"]
      (let whens = [("always", Always)
                   ,("auto", Auto)
                   ,("never", Never)]
           match str = isPrefixOf str . fst
       in  ReqArg (\str (O c m) -> case find (match str) whens of
                      Just (_, uc) -> O (c {cfgUseColor = uc}) m
                      _            -> throw (InvalidArgument "color" str))
           "WHEN")
      (unlines
       ["When to use colors, \"auto\", \"always\", or \"never\""
       ,"(default: auto)"])

  , Option ['s'] ["stddev"]
    (ReqArg (\str (O c m) -> case readNonNegativeParcents str of
                Just x -> O (c {cfgRelStdDev = x}) m
                _      -> throw (InvalidArgument "stddev" str))
     "NUM")
    (unlines
     ["Target relative standard deviation of measurement"
     ,"in percents (default: 5)"])

  , Option ['v'] ["verbosity"]
    (ReqArg (\str (O c m) -> case readRanged "verbosity" 0 3 str of
                Right n  -> O (c {cfgVerbosity=n}) m
                Left err -> throw err)
      "INT")
     "Verbosity level (default: 1)"

  , Option ['n'] ["iters"]
    (ReqArg (\str (O c _) -> case readMaybe str :: Maybe Word64 of
                Just n -> O c (DoIter n)
                _      -> throw (InvalidArgument "iters" str))
    "INT")
    "Run benchmarks, don't analyse"

  , Option ['m'] ["match"]
    (let modes = [("glob", Glob)
                 ,("pattern", Pattern)
                 ,("prefix", Prefix)
                 ,("ipattern", IPattern)]
         match str = isPrefixOf str . fst
     in  ReqArg (\str (O c m) -> case find (match str) modes of
                    Just (_, mode) -> O (c {cfgMatchMode = mode}) m
                    _              -> throw (InvalidArgument "match" str))
         "MODE")
    (unlines
     ["How to match benchmark names (\"prefix\", \"glob\","
     ,"\"pattern\" (substring), or \"ipattern\")"])

  , Option ['l'] ["list"]
    (NoArg (\ (O c _) -> O c DoList))
    "List benchmarks"

  , Option [] ["version"]
    (NoArg (\ (O c _ ) -> O c Version))
    "Show version info"
  ]

readRanged :: (Ord a, Read a, Show a)
            => String -> a -> a -> String -> Either MiniterionException a
readRanged lbl !lo !hi !str =
  case readMaybe str of
    Just n | lo <= n && n <= hi -> Right n
    Just n                      -> Left (out_of_range (show n))
    Nothing                     -> Left (InvalidArgument lbl str)
  where
    out_of_range val =
      OutOfRangeArgument lbl val ("(" ++ show lo ++ "," ++ show hi ++ ")")

readNonNegativeParcents :: String -> Maybe Double
readNonNegativeParcents = readPercentsWith (>= 0)

readPositivePercents :: String -> Maybe Double
readPositivePercents = readPercentsWith (> 0)

readPercentsWith :: (Double -> Bool) -> String -> Maybe Double
readPercentsWith test xs = do
  x <- readMaybe xs
  guard (test x)
  pure (x / 100)


-- ------------------------------------------------------------------------
-- Exception
-- ------------------------------------------------------------------------

data MiniterionException
  = InvalidArgument String String
  | OutOfRangeArgument String String String
  | CannotReadFile (Maybe String) String
  | UninitializedEnv [String]
  | GlobUnbalancedBracket String
  | GlobUnbalancedBrace String
  deriving (Show)

instance Exception MiniterionException where
  displayException = displayMiniterionException

displayMiniterionException :: MiniterionException -> String
displayMiniterionException = \case
  InvalidArgument lbl arg ->
    "invalid argument `" ++ arg ++ "'" ++ maybe_label (Just lbl)
  OutOfRangeArgument lbl val rng ->
    val ++ " is outside range " ++ rng ++ maybe_label (Just lbl)
  CannotReadFile mb_lbl path ->
    "cannot read file `" ++ path ++ "'" ++ maybe_label mb_lbl
  UninitializedEnv groups ->
    "uninitialized env" ++
    (if null groups then "" else " under `" ++ groupsToName groups ++ "'") ++
    "\nuse irrefutable pattern in the function taking the env."
  GlobUnbalancedBracket pat ->
    "unbalanced bracket in glob pattern `" ++ pat ++ "'"
  GlobUnbalancedBrace pat ->
    "unbalanced brace in glob pattern `" ++ pat ++ "'"
  where
    maybe_label = maybe "" (\lbl -> " for `--" ++ lbl ++ "'")

handleMiniterionException :: IO a -> IO a
handleMiniterionException =
  handle $ \e -> maybe (throwIO e) complain_and_die (fromException e)
  where
    complain_and_die :: MiniterionException -> IO a
    complain_and_die he = do
      me <- getProgName
      die (me ++ ": " ++ displayException he ++ "\n" ++ briefUsageOf me)


-- ------------------------------------------------------------------------
-- Getting current time
-- ------------------------------------------------------------------------

getPicoSecs :: IO Word64
getPicoSecs = fmap (* 1000) getMonotonicTimeNSec
{-# INLINE getPicoSecs #-}

getCpuPicoSecs :: IO Word64
getCpuPicoSecs = fmap fromIntegral getCPUTime
{-# INLINE getCpuPicoSecs #-}

picoToSecW :: Word64 -> Double
picoToSecW = picoToSecD . word64ToDouble
{-# INLINE picoToSecW #-}

picoToSecD :: Double -> Double
picoToSecD pico = pico / 1e12
{-# INLINE picoToSecD #-}

picoToMicroSecWI :: Word64 -> Int
picoToMicroSecWI pico = word64ToInt (pico `quot` 1000000)
{-# INLINE picoToMicroSecWI #-}


-- ------------------------------------------------------------------------
-- Getting GC info
-- ------------------------------------------------------------------------

getAllocsAndCopied :: Bool -> IO (Word64, Word64, Word64)
getAllocsAndCopied has_rts_stats
  | has_rts_stats = do
    s <- getRTSStats
    pure (allocated_bytes s, copied_bytes s, max_mem_in_use_bytes s)
  | otherwise = pure (0, 0, 0)
{-# INLINABLE getAllocsAndCopied #-}


-- ------------------------------------------------------------------------
-- Measuring
-- ------------------------------------------------------------------------

data Measurement = Measurement
  { measIters   :: !Word64 -- ^ number of iterations
  , measTime    :: !Word64 -- ^ time in picoseconds
  , measCpuTime :: !Word64 -- ^ cpu time in picoseconds
  , measAllocs  :: !Word64 -- ^ allocations in bytes
  , measCopied  :: !Word64 -- ^ copied bytes
  , measMaxMem  :: !Word64 -- ^ max memory in use
  }

-- | Measurement paired with end time.
data Measured = Measured
  { mdMeas     :: !Measurement
  , _mdEndTIme :: !Word64
  }

instance Semigroup Measured where
  Measured !m1 _ <> Measured !m2 !e2 = Measured m3 e2
    where
      on h g = h (g m1) (g m2)
      add_on = on (+)
      m3 = Measurement { measIters = measIters m2
                       , measTime = add_on measTime
                       , measCpuTime = add_on measCpuTime
                       , measAllocs = add_on measAllocs
                       , measCopied = add_on measCopied
                       , measMaxMem = on max measMaxMem
                       }
  {-# INLINE (<>) #-}

data Summary = Summary
  { smMeasurement :: !Measurement -- ^ Last measurement
  , smOLS         :: !Ranged
  , smR2          :: !Ranged
  , smMean        :: !Ranged
  , smStdDev      :: !Ranged
  , smOutlierVar  :: !OutlierVariance
  , smOutliers    :: Outliers
  , smKDEs        :: KDE
  , smMeasured    :: [Measurement]
  }

emptySummary :: Summary
emptySummary = measToSummary Measurement
  { measIters = 0
  , measTime = 0
  , measCpuTime = 0
  , measAllocs = 0
  , measCopied = 0
  , measMaxMem = 0
  }
{-# INLINABLE emptySummary #-}

-- | One millisecond in picoseconds.
oneMillisecond :: Num a => a
oneMillisecond = 1000000000
{-# INLINE oneMillisecond #-}

-- See 'Criterion.Measurement.runBenchmarkable' in the
-- criterion-measurement package.
runLoop :: Semigroup a => Benchmarkable -> Word64 -> (IO () -> IO a) -> IO a
runLoop Benchmarkable{..} n f
  | perRun    = work >>= go (n - 1)
  | otherwise = work
  where
    go 0 result   = pure result
    go !i !result = work >>= go (i - 1) . (<>) result

    count | perRun = 1
          | otherwise = n

    work = do
      e <- allocEnv count
      let clean = cleanEnv count e
          run = runRepeatedly e count
      clean `seq` run `seq` evaluate (rnf e)
      f run `finally` clean
    {-# INLINE work #-}
{-# INLINE runLoop #-}

measure :: MEnv -> Word64 -> Benchmarkable -> IO Measured
measure MEnv{meHasRTSStats=gc} num b =
  runLoop b num $ \act -> do
    performMinorGC
    (start_allocs, start_copied, start_max_mem) <- getAllocsAndCopied gc
    start_time <- getPicoSecs
    start_cpu_time <- getCpuPicoSecs
    act
    end_time <- getPicoSecs
    end_cpu_time <- getCpuPicoSecs
    performMinorGC
    (end_allocs, end_copied, end_max_mem) <- getAllocsAndCopied gc
    let meas = Measurement
          { measIters = num
          , measTime = end_time - start_time
          , measCpuTime = end_cpu_time - start_cpu_time
          , measAllocs = end_allocs - start_allocs
          , measCopied = end_copied - start_copied
          , measMaxMem = max end_max_mem start_max_mem
          }
    pure $ Measured meas end_time

measureUntil :: MEnv -> Benchmarkable -> IO Summary
measureUntil menv@MEnv{meConfig=cfg@Config{..}, ..} b
  | is_once   = fmap (measToSummary . mdMeas) (measure menv 1 b)
  | otherwise = init_and_go
  where
    is_once = isInfinite cfgRelStdDev && 0 < cfgRelStdDev

    -- See Criterion.Measurement.runBenchmark
    init_and_go = do
      runLoop b 1 id
      start_time <- performGC >> getPicoSecs
      go series start_time (Acc 0 0 [])

    go [] !_ !_ = error "measureUntil.go: empty series"
    go (n:ns) start_time acc = do
      Measured m end_time <- measure menv n b
      -- As in tasty-bench, estimating with running mean and running
      -- standard deviation using a fragment of the measurements (4
      -- most recent). Earlier measurements tends to contain noises.
      let (!mean, !sd) = meanAndStdDev 4 (take 4 ts)
            where
              ts = [ word64ToDouble measTime / word64ToDouble measIters
                   | Measurement{..} <- acMeasurements acc' ]
          !is_stddev_in_target_range = sd < cfgRelStdDev * mean
          !is_timeout_soon = case meTimeout of
            Just dur -> dur < (end_time + measTime m * 2) - start_time
            _        -> False
          !acc' = acc { acMeasurements = m : acMeasurements acc
                      , acCount = acCount acc + 1
                      , acValidCount = acValidCount acc +
                                       if threshold < measTime m then 1 else 0
                      }
      debug' menv (formatMeasurement m mean sd)
      warnOnTooLongBenchmark meTimeout start_time end_time
      -- Need at least 4 long enough measurements to get IQR while
      -- computing KDE.
      if 4 <= acValidCount acc' &&
         (is_stddev_in_target_range ||
          is_timeout_soon)
        then do
          let dur = end_time - start_time
          verbose' menv $
            formatBootstrap dur cfgResamples (acValidCount acc') (acCount acc')
          pure $ summarize cfg start_time acc'
        else go ns start_time acc'

-- See 'Criterion.Measurement.{squish,series}' in the package
-- 'criterion-measurement'.
series :: [Word64]
series = squish (unfoldr f 1)
  where
    squish = foldr g []
      where g x xs = x : dropWhile (== x) xs
    f k = Just (truncate l, l)
      where l = k * 1.05 :: Double

measToSummary :: Measurement -> Summary
measToSummary m@(Measurement {measTime=t}) =
  Summary { smMeasurement = m
          , smOLS = toRanged (word64ToDouble t)
          , smR2 = toRanged 1
          , smStdDev = toRanged 0
          , smMean =  toRanged (word64ToDouble t)
          , smKDEs = KDE [] []
          , smMeasured = []
          , smOutlierVar = OutlierVariance Unaffected "no" 0
          , smOutliers = Outliers 0 0 0 0 0
          }
{-# INLINABLE measToSummary #-}

warnOnTooLongBenchmark :: Maybe Word64 -> Word64 -> Word64 -> IO ()
warnOnTooLongBenchmark tout t_start t_now =
  case tout of
    Nothing | t_now - t_start > 100 * 1000000000000 ->
      hPutStrLn stderr $
              "\n" ++
              "This benchmark takes more than 100 seconds.\n" ++
              "Conosider setting --time-limit, if this is\n" ++
              "unexpected (or to silence this warning)."
    _ -> pure ()
{-# INLINABLE warnOnTooLongBenchmark #-}


-- ------------------------------------------------------------------------
-- Accumulator for measureUntil
-- ------------------------------------------------------------------------

data Acc = Acc
  { acCount        :: !Word64 -- ^ Number of measurements
  , acValidCount   :: !Word64 -- ^ Number of measurements longer than threshold
  , acMeasurements :: ![Measurement]
  }

-- | 30 milliseconds in picosecond.
threshold :: Word64
threshold = 30 * oneMillisecond
{-# INLINE threshold #-}

summarize :: Config -> Seed -> Acc -> Summary
summarize Config{..} seed Acc{..} = Summary
  { smMeasurement = case acMeasurements of
                      m:_ -> scale m -- the last measurement
                      _   -> error "summarize: empty measurements"
  , smOLS = ols
  , smR2 = r2
  , smMean = mean
  , smStdDev = stddev
  , smOutlierVar = ov
  , smOutliers = outliers
  , smKDEs = kde
  , smMeasured = measured
  }
  where
    (ols, r2) = bootstrap' (regress nc) acCount xys
    (mean, stddev) = bootstrap' (meanAndStdDev nvc) acValidCount times
    (!ov, outliers) = computeOutliers (irMid stddev) iqr
    kde = computeKDE (irMid stddev) nvc iqr
    measured = reverse acMeasurements

    bootstrap' :: Ord a => ([a] -> (Double, Double)) -> Word64 -> [a]
               -> (Ranged, Ranged)
    bootstrap' = bootstrap2 seed cfgResamples cfgInterval

    !nc = word64ToDouble acCount
    !nvc = word64ToDouble acValidCount
    !iqr = computeIQR nvc times

    -- Filtering out measurements with too short total duration for
    -- `times', since those data are considered imprecise and
    -- unreliable. See 'Criterion.Analysis.analyseSample'.
    (times, xys) = foldl' f ([],[]) measured
      where
        f (!as,!bs) Measurement{..} =
          let i = word64ToDouble measIters
              t = word64ToDouble measTime
              as' = if threshold < measTime then t/i : as else as
          in  (as', (i, t) : bs)
{-# INLINE summarize #-}

scale :: Measurement -> Measurement
scale (Measurement n t p a c m) = Measurement n t' p' a' c' m
  where
    t' = t `quot` n
    p' = p `quot` n
    a' = a `quot` n
    c' = c `quot` n
{-# INLINE scale #-}


-- ------------------------------------------------------------------------
-- Ordered values
-- ------------------------------------------------------------------------

-- | A range of 'Double' values.
data Ranged = Ranged
  { _irLo :: !Double
  , irMid :: !Double
  , _irHi :: !Double
  }

-- | Apply given function to the values in Ranged.
mapRanged :: (Double -> Double) -> Ranged -> Ranged
mapRanged f (Ranged l m h) = Ranged (f l) (f m) (f h)
{-# INLINE mapRanged #-}

-- | Ranged value with identical lo, high, and the body values.
toRanged :: Double -> Ranged
toRanged x = Ranged x x x
{-# INLINE toRanged #-}


-- ------------------------------------------------------------------------
-- Bootstrap
-- ------------------------------------------------------------------------

bootstrap2 :: Seed                      -- ^ Random seed
           -> Word64                    -- ^ Number of resamples
           -> Double                    -- ^ Confidence interval
           -> ([a] -> (Double, Double)) -- ^ Function applied to each resample
           -> Word64                    -- ^ Length of the original list
           -> [a]                       -- ^ The original list
           -> (Ranged, Ranged)
bootstrap2 !seed !nresamp !ci !f !norig orig = (br, cr)
  where
    br = confInterval b ci nresamp bs
    cr = confInterval c ci nresamp cs
    (b, c) = f orig
    (bs, cs) = unzip (resample seed nresamp f norig orig)
{-# INLINE bootstrap2 #-}

confInterval :: Double   -- ^ The point value
             -> Double   -- ^ Interval
             -> Word64   -- ^ Length of the list
             -> [Double] -- ^ The list
             -> Ranged
confInterval !mid !i !n xs = Ranged lo mid hi
  where
    !lo = xs' !! truncate (n' * i')
    !hi = xs' !! ceiling (n' * (1 - i'))
    !i' = (1 - i) / 2
    !n' = word64ToDouble (n - 1)
    xs' = sort xs
{-# INLINE confInterval #-}

resample :: Seed       -- ^ Random seed.
         -> Word64     -- ^ Number of resamples.
         -> ([a] -> b) -- ^ Function applied to each resample.
         -> Word64     -- ^ Length of the original sample.
         -> [a]        -- ^ Original sample.
         -> [b]
resample !seed !nresamp !f !norig orig = go nresamp [] idxs0
  where
    idxs0 = randoms seed norig
    !orig_arr = listArray (0, norig - 1) orig
    go 0 !acc _    = acc
    go n !acc idxs = go (n-1) acc' idxs'
      where
        (is, idxs') = splitAt' norig idxs
        !acc' = let !bs = f [orig_arr ! i | i <- is] in bs : acc
{-# INLINE resample #-}

-- | Simplified version of 'Prelude.splitAt'. The order of the first
-- element of the resulting pair is reversed to make the internal loop
-- tail-recursive.
splitAt' :: Word64 -> [a] -> ([a], [a])
splitAt' = go []
  where
    go !acc 0 xs      = (acc, xs)
    go !acc _ []      = (acc, [])
    go !acc !m (x:xs) = go (x:acc) (m - 1) xs
{-# INLINABLE splitAt' #-}


-- ------------------------------------------------------------------------
-- Analysis
-- ------------------------------------------------------------------------

data KDE = KDE
  { kdValues :: ![Double]
  , kdPDF    :: ![Double]
  }

data OutlierEffect
  = Unaffected
  | Slight
  | Moderate
  | Severe
  deriving (Show)

data OutlierVariance = OutlierVariance
  { ovEffect   :: OutlierEffect
  , ovDesc     :: String
  , ovFraction :: !Double
  }

data Outliers = Outliers
  { otSamplesSeen :: !Word64
  , otLowSevere   :: !Word64
  , otLowMild     :: !Word64
  , otHighMild    :: !Word64
  , otHighSevere  :: !Word64
  }

-- | Interquartile range in seconds.
data IQR = IQR
  { iq1           :: !Double   -- ^ Q1
  , iq3           :: !Double   -- ^ Q3
  , iqR           :: !Double   -- ^ Q3 - Q1
  , iqPseudosigma :: !Double   -- ^ (Q3 - Q1) / 1.349
  , iqSorted      :: ![Double] -- ^ Sorted samples
  }

-- | Mean and standard deviation (unbiased).
meanAndStdDev :: Double           -- ^ Length of the samples
              -> [Double]         -- ^ The samples
              -> (Double, Double) -- ^ (mean, standard deviation)
meanAndStdDev !n xs = (mean, sqrt (ssd / (n - 1)))
  where
    !ssd = sumKBN [square (x - mean) | x <- xs]
    !mean = sumKBN xs / n
{-# INLINABLE meanAndStdDev #-}

-- | Simple linear regression with ordinary least square.
regress :: Double             -- ^ Length of the list
        -> [(Double, Double)] -- ^ List of x and y values
        -> (Double, Double)   -- ^ (coefficient, R²)
regress !n xys = (a, r2)
  where
    -- means
    (!x_sum, !y_sum) = sumKBN xys
    (!x_mean, !y_mean) = (x_sum / n, y_sum / n)

    -- sum of squared deviations and dot product
    (!x_ssd, !sst, !dotp) =
      sumKBN [ (square xd, square yd, xd * yd)
             | (x,y) <- xys
             , let xd = x - x_mean; yd = y - y_mean ]

    -- coefficient and fitted function
    !a = dotp / x_ssd
    -- !b = y_mean - (a * x_mean)
    f x = a * x -- use `a * x + b' instead?

    -- ssr and R^2
    !ssr = sumKBN [square (y - f x) | (x,y) <- xys]
    !r2 = 1 - (ssr / sst)
{-# INLINABLE regress #-}

computeIQR :: Double   -- ^ Number of samples.
           -> [Double] -- ^ The samples.
           -> IQR
computeIQR !n xs = IQR q1 q3 r ps xs'
  where
    q1 = xs' !! truncate (n * 0.25)
    q3 = xs' !! ceiling (n * 0.75)
    r = q3 - q1
    ps = r / 1.349
    xs' = sort [picoToSecD x | x <- xs]
{-# INLINABLE computeIQR #-}

computeOutliers :: Double -- ^ Standard deviation.
                -> IQR    -- ^ Interquartile range.
                -> (OutlierVariance, Outliers)
computeOutliers !s IQR{..} = (ov, otls)
  where
    -- See 'Criterion.Analysis.classifyOutliers'.
    otls = foldr f z iqSorted
      where
        f t
          | t  <= ls  = addOutliers (Outliers 1 1 0 0 0)
          | t  <= lm  = addOutliers (Outliers 1 0 1 0 0)
          | hs <= t   = addOutliers (Outliers 1 0 0 0 1)
          | hm <= t   = addOutliers (Outliers 1 0 0 1 0)
          | otherwise = addOutliers (Outliers 1 0 0 0 0)
          where
            !ls = iq1 - (iqR * 3)
            !lm = iq1 - (iqR * 1.5)
            !hm = iq3 + (iqR * 1.5)
            !hs = iq3 + (iqR * 3)
        z = Outliers 0 0 0 0 0

    -- See 'Criterion.Analysis.outlierVariance'.
    ov = OutlierVariance effect desc frac
      where
        (effect, desc) | frac < 0.01 = (Unaffected, "no")
                       | frac < 0.1  = (Slight,     "a slight")
                       | frac < 0.5  = (Moderate,   "a moderate")
                       | otherwise   = (Severe,     "a severe")
        frac = 1 - min 1 (iqPseudosigma / s_in_seconds)
        s_in_seconds = s / 1e12
{-# INLINABLE computeOutliers #-}

computeKDE :: Double -- ^ Standard deviation.
           -> Double -- ^ Number of samples.
           -> IQR    -- ^ Interquartile range.
           -> KDE
computeKDE !s !n IQR{..} = KDE values density
  where
    -- Dividing 120% of the range to 128 points.
    values = enumFromThenTo lo' (lo'+delta) hi'
      where
        delta = (hi' - lo') / 127
        lo' = lo - r/10
        hi' = hi + r/10
        r = hi - lo
        (lo, hi) = case iqSorted of
          []   -> error "computeKDE: empty list"
          hd:_ -> (hd, iqSorted !! (truncate n - 1))

    -- Using simple Gaussian kernel function and Silverman's rule of
    -- thumb for bandwidth.
    density = [sumKBN [k ((x-xi)/h) | xi<-iqSorted] / (n*h) | x<-values]
      where
        k u = exp (-(u*u/2)) / sqrt (2*pi)
        !h = 0.9 * min s_in_seconds iqPseudosigma * (n ** (-0.2))
        s_in_seconds = picoToSecD s
{-# INLINABLE computeKDE #-}

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers n1 ls1 lm1 hm1 hs1) (Outliers n2 ls2 lm2 hm2 hs2) =
  Outliers (n1+n2) (ls1+ls2) (lm1+lm2) (hm1+hm2) (hs1+hs2)
{-# INLINE addOutliers #-}

square :: Num a => a -> a
square x = x * x
{-# INLINE square #-}


-- ------------------------------------------------------------------------
-- Summation
-- ------------------------------------------------------------------------

-- Kahan–Babuška-Neumaier summation, see the
-- <https://en.wikipedia.org/wiki/Kahan_summation_algorithm#Precision
-- Kahan summation algorithm Wikipedia page>.
--
-- The 'sumKBN' function is used to sum list of Double,
-- (Double,Double), (Double,Double,Double) ... etc, element wise.

sumKBN :: SumKBN a => [a] -> a
sumKBN = unKBN . foldl' add zero
{-# INLINE sumKBN #-}

-- | Type class to describe the element and intermediate state of the
-- summation.
class SumKBN a where
  data KBN a
  zero  :: KBN a
  add   :: KBN a -> a -> KBN a
  unKBN :: KBN a -> a

instance SumKBN Double where
  data KBN Double = SC !Double !Double
  zero = SC 0 0
  {-# INLINE zero #-}

  -- This implementation of `add' is basically same as the `sum kbn'
  -- found in the math-functions package.
  add (SC s c) x = SC s' c'
    where
      s' = s + x
      c' | abs s >= abs x = c + ((s - s') + x)
         | otherwise      = c + ((x - s') + s)
  {-# INLINE add #-}

  unKBN (SC s c) = s + c
  {-# INLINE unKBN #-}

instance SumKBN a => SumKBN (a,a) where
  data KBN (a,a) = SC2 !(KBN a) !(KBN a)
  zero = SC2 zero zero
  {-# INLINE zero #-}
  {-# SPECIALIZE zero :: KBN (Double,Double) #-}

  add (SC2 sx sy) (x,y) = SC2 (add sx x) (add sy y)
  {-# INLINE add #-}
  {-# SPECIALIZE add :: KBN (Double,Double) -> (Double,Double)
                     -> KBN (Double,Double) #-}

  unKBN (SC2 sx sy) = (unKBN sx, unKBN sy)
  {-# INLINE unKBN #-}
  {-# SPECIALIZE unKBN :: KBN (Double,Double) -> (Double,Double) #-}

instance SumKBN a => SumKBN (a,a,a) where
  data KBN (a,a,a) = SC3 !(KBN a) !(KBN a) !(KBN a)
  zero = SC3 zero zero zero
  {-# INLINE zero #-}
  {-# SPECIALIZE zero :: KBN (Double,Double,Double) #-}

  add (SC3 sx sy sz) (x,y,z) = SC3 (add sx x) (add sy y) (add sz z)
  {-# INLINE add #-}
  {-# SPECIALIZE add :: KBN (Double,Double,Double)
                     -> (Double,Double,Double)
                     -> KBN (Double,Double,Double) #-}

  unKBN (SC3 sx sy sz) = (unKBN sx, unKBN sy, unKBN sz)
  {-# INLINE unKBN #-}
  {-# SPECIALIZE unKBN :: KBN (Double,Double,Double)
                       -> (Double,Double,Double) #-}


-- ------------------------------------------------------------------------
-- Random numbers
-- ------------------------------------------------------------------------

-- See the <https://prng.di.unimi.it/xoshiro256plusplus.c C code> by
-- the original author and
-- <https://en.wikipedia.org/wiki/Xorshift#xoshiro256++ Xorshift> page
-- in Wikipedia.

-- | Alias for random seed.
type Seed = Word64

-- | State for xoshiro256++.
data Xoshiro256 = Xoshiro256 !Word64 !Word64 !Word64 !Word64

-- | Infinite list of random numbers.
randoms :: Seed   -- ^ Random seed
        -> Word64 -- ^ Upper bound of generated random value (exclusive)
        -> [Word64]
randoms !seed !ub = drop 1 $ unfoldr f (0, xoshiro256init seed)
  where
    f (!x, !s) = let !x' = x `rem` ub in Just (x', xoshiro256pp s)

xoshiro256pp :: Xoshiro256 -> (Word64, Xoshiro256)
xoshiro256pp (Xoshiro256 s0 s1 s2 s3) = (result, state)
  where
    !result = rotl (s0 + s3) 23 + s0
    !state = Xoshiro256 s0' s1' s2'' s3''
      where
        t = shiftL s1 17
        s2' = s2 `xor` s0
        s3' = s3 `xor` s1
        s1' = s1 `xor` s2'
        s0' = s0 `xor` s3'
        s2'' = s2' `xor` t
        s3'' = rotl s3' 45
    rotl !x !k = shiftL x k .|. shiftR x (64 - k)

xoshiro256init :: Seed -> Xoshiro256
xoshiro256init !seed = Xoshiro256 s0 s1 s2 s3
  where
    (!s0, !seed') = splitmix64 seed
    !s1 = shiftR s0 32
    (!s2, _) = splitmix64 seed'
    !s3 = shiftR s2 32

splitmix64 :: Word64 -> (Word64, Word64)
splitmix64 !s = (r3, r0)
  where
    !r0 = s + 0x9e3779b97f4a7c15
    !r1 = (r0 `xor` shiftR r0 30) * 0xbf58476d1ce4e5b9
    !r2 = (r1 `xor` shiftR r1 27) * 0x94d049bb133111eb
    !r3 = r2 `xor` shiftR r2 31


-- ------------------------------------------------------------------------
-- Converting numbers
-- ------------------------------------------------------------------------

word64ToDouble :: Word64 -> Double
word64ToDouble = fromIntegral
{-# INLINE word64ToDouble #-}

word64ToInt :: Word64 -> Int
word64ToInt = fromIntegral
{-# INLINE word64ToInt #-}


-- ------------------------------------------------------------------------
-- Running function repeatedly
-- ------------------------------------------------------------------------

-- criterion-measurement-0.2.1 uses NOINLINE pragma, gauge-0.2.5 and
-- tasty-bench-0.3.4 use INLINE pragma for following wrapper
-- functions.  At the moment, this module is using NOINLINE. See
-- 'Criterion.Measurement.Types.Internal' in 'criterion-measurement'
-- package.

#if !MIN_VERSION_base(4,15,0)
data SPEC = SPEC
{-# ANN type SPEC ForceSpecConstr #-}
#endif

nf' :: (b -> c) -> (a -> b) -> a -> Word64 -> IO ()
nf' frc = benchLoop SPEC
  where
    -- Explicitly passing `f' and `x' as the arguments of `benchLoop',
    -- so that ghc won't optimize away them. This approach is taken in
    -- tasty-bench. Criterion, as of criterion-measurement 0.2.1,
    -- defines the looping function in a separate module and that
    -- module has -fno-full-laziness GHC_OPTIONS pragma hard coded.
    benchLoop !_ f x n
      | n == 0 = pure ()
      | otherwise = do
          val <- evaluate (f x)
          frc val `seq` benchLoop SPEC f x (n - 1)
{-# NOINLINE nf' #-}

whnf' :: (a -> b) -> a -> Word64 -> IO ()
whnf' = go SPEC
  where
    -- See the comment in `nf'' for explicit `f' and `x'.
    go !_ f x n
      | n == 0 = pure ()
      | otherwise = do
          _ <- evaluate (f x)
          go SPEC f x (n - 1)
{-# NOINLINE whnf' #-}

ioToBench :: (a -> b) -> IO a -> (Word64 -> IO ())
ioToBench frc a = go
  where
    go n
      | n == 0 = pure ()
      | otherwise = do
          val <- a
          frc val `seq` go (n - 1)
{-# NOINLINE ioToBench #-}

ioFuncToBench :: (b -> c) -> (a -> IO b) -> a -> Word64 -> IO ()
ioFuncToBench frc f x = go
  where
    go n
      | n <= 0 = pure ()
      | otherwise = do
          val <- f x
          frc val `seq` go (n - 1)
{-# NOINLINE ioFuncToBench #-}


-- ------------------------------------------------------------------------
-- Windows stuffs
-- ------------------------------------------------------------------------

#if defined(mingw32_HOST_OS)
#  if defined(i386_HOST_ARCH)
foreign import stdcall unsafe "windows.h GetConsoleOutputCP"
  getConsoleOutputCP :: IO Word32
foreign import stdcall unsafe "windows.h SetConsoleOutputCP"
  setConsoleOutputCP :: Word32 -> IO ()
#  else
foreign import ccall unsafe "windows.h GetConsoleOutputCP"
  getConsoleOutputCP :: IO Word32
foreign import ccall unsafe "windows.h SetConsoleOutputCP"
  setConsoleOutputCP :: Word32 -> IO ()
#  endif
#endif
