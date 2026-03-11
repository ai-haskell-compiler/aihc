{-# LANGUAGE CPP #-}
module Main (main) where

-- base
import           Control.Exception  (catch, fromException, throwIO)
import           Data.Functor       (void)
import           Data.IORef         (modifyIORef', newIORef, readIORef)
import           System.Environment (withArgs)
import           System.Exit        (ExitCode (..), exitFailure, exitSuccess)
import           System.Info        (os)
import           System.IO          (stderr, stdout)
import           System.IO.Error    (isDoesNotExistError)

-- directory
import           System.Directory   (removeFile)

-- silently
import           System.IO.Silently (hSilence)

-- tasty
import           Test.Tasty

-- tasty-hunit
import           Test.Tasty.HUnit

-- Internal
import           Miniterion


-- ------------------------------------------------------------------------
-- Main
-- ------------------------------------------------------------------------

main :: IO ()
main = Test.Tasty.defaultMain $
  testGroup "All"
  [ benchmarkable
  , options
  , skipping
  , substr
  , glob
  , csv
  , json
  , report
  , timelimit
#ifdef DEV
  , formatPicos
  , formatBytes
#endif
  ]


-- ------------------------------------------------------------------------
-- Test trees
-- ------------------------------------------------------------------------

benchmarkable :: TestTree
benchmarkable = testGroup "benchmarkable"
  [ testCase "fib" $
    defaultMain'
    [ bgroup "fib-nf"
      [ bench "4" (nf fib 4)
      , bench "8" (nf fib 8) ]
    , bgroup "fib-whnf"
      [ bench "4" (whnf fib 4)
      , bench "8" (whnf fib 8) ]]

  , testCase "wcIO" $
    defaultMain'
    [ bgroup "wcIO"
      [ bench "nfIO" (nfIO (wcIO miniterionDotCabal))
      , bench "whnfIO" (whnfIO (wcIO miniterionDotCabal))
      , bench "nfAppIO" (nfAppIO wcIO miniterionDotCabal)
      , bench "whnfAppIO" (whnfAppIO wcIO miniterionDotCabal) ]]

  , testGroup "env"
    [ testCase "wc with env" $
      defaultMain'
      [ env (readFile miniterionDotCabal) $ \contents ->
          bench "wc" (nf wc contents) ]]

  , testGroup "perBatchEnv"
    [ testCase "wc with perBatchEnv" $
      defaultMain'
      [ bench "wc" $
        perBatchEnv
        (\_ -> readFile miniterionDotCabal)
        (pure . wc) ]]

  , testGroup "perRunEnv"
       ([ testCase "wc with perRunEnv" $
          defaultMainWith'
          ["--stddev", "90"]
          [ bench "wc" $
            perRunEnv (readFile miniterionDotCabal) (pure . wc) ]
        | os == "linux"
        ] <>
        [ testCase "perRunEnv with time limit" $
          defaultMainWith'
          ["-L4", "-s1e-32"]
          [ bench "fib" $
            perRunEnv
            (pure 32)
            (pure . fib) ]])

  , testGroup "iters"
    [ testCase "iter batch bench" $
      defaultMainWith' ["-n", "3"]
      [ bgroup "fib" [ bench "4" (nf fib 4)
                     , bench "8" (nf fib 8) ]]
    , testCase "iter perBatch bench" $
      defaultMainWith' ["-n", "3", "-L", "1"]
      [ bgroup "fib" [ bench "4" (perBatchEnvWithCleanup
                                 pure
                                 (\n e -> (n + e) `seq` pure ())
                                 (\_ -> fib 4 `seq` pure ()))]]
    , testCase "iter perRun bench" $
      defaultMainWith' ["--iters", "3"]
      [ bgroup "fib" [ withRunEnv "16" 16 ifib
                     , withRunEnv "32" 32 ifib]
      ]
    , testCase "iter timeout" $
      shouldExitFailure $
      defaultMainWith' ["--iters", "3000", "-L", "1"]
      [ bgroup "fib" [ bench "36" (nf fib 36) ]
      ]
    , testCase "iter invalid arg" $
      shouldExitFailure $ benchFib4 ["--iter", "foo"]
    ]

  , testGroup "interactive"
    [ testCase "simple function" $
      quietly $ benchmark (nf not True) ]
  ]

withRunEnv :: String -> a -> (a -> a) -> Benchmark
withRunEnv name ini f =
  let alloc = newIORef ini
      cleanup ref = readIORef ref >>= \v -> v `seq` pure ()
      run ref = modifyIORef' ref f
  in  bench name (perRunEnvWithCleanup alloc cleanup run)

options :: TestTree
options = testGroup "options"
  [ testCase "help with long option" $
    emptyMain ["--help"]

  , testCase "help with short option" $
    emptyMain ["-h"]

  , testCase "show version info" $
    emptyMain ["--version"]

  , testCase "listing names with long option" $
    benchFib4 ["--list"]

  , testCase "listing names with short option" $
    benchFib4 ["-l"]

  , testCase "listing names with pattern match" $
    benchNesting ["-l", "-mg", "c/*/*"]

  , testCase "listing name of benchmark using env" $
    benchFib4 ["--list"]

  , testCase "listing name of benchmark using env and pat" $
    shouldExitFailure $ benchWithEnvAndPat ["--list"]

  , testCase "listing name of benchmark using env and irrefultable pat" $
    benchWithEnvAndIrrPat ["--list"]

  , testCase "stddev option" $
    benchFib4 ["--stddev", "20"]

  , testCase "short stddev option" $
    benchFib4 ["-s", "20"]

  , testCase "infinit stddev" $
    benchFib4 ["--stddev", "Infinity"]

  , testCase "invalid stddev arg" $
    shouldExitFailure $ emptyMain ["--stddev", "foo"]

  , testCase "missing stddev arg" $
    shouldExitFailure $ emptyMain ["--stddev"]

  , testCase "invalid time-mode option" $
    shouldExitFailure $ benchFib4 ["--time-mode", "blah"]

  , testCase "invalid timeout option" $
    shouldExitFailure $ benchFib4 ["--time-limit", "foo"]

  , testCase "confidence interval option" $
    benchFib4 ["--ci", "0.1"]

  , testCase "short confidence interval option" $
    benchFib4 ["-I", "0.9"]

  , testCase "too small confidence interval arg" $
    shouldExitFailure $ benchFib4 ["--ci", "1e-9"]

  , testCase "too large confidence interval arg" $
    shouldExitFailure $ benchFib4 ["-I", "100"]

  , testCase "not a number confidence interval arg" $
    shouldExitFailure $ benchFib4 ["--ci", "./out.html"]

  , testCase "resamples option" $
    benchFib4 ["--resamples", "1500"]

  , testCase "too small resamples arg" $
    shouldExitFailure $ benchFib4 ["--resamples", "0"]

  , testCase "too large resamples arg" $
    shouldExitFailure $ benchFib4 ["--resamples", "1000000000"]

  , testCase "color auto" $
    benchFib4 ["--color", "auto"]

  , testCase "color always" $
    benchFib4 ["--color", "always"]

  , testCase "color never" $
    benchFib4 ["--color", "never"]

  , testCase "color invalid arg" $
    shouldExitFailure $ benchFib4 ["--color", "dwim"]

  , testCase "verbosity 0" $
    benchFib4 ["--verbosity", "0"]

  , testCase "verbosity 1" $
    benchFib4 ["-v", "1"]

  , testCase "verbosity 2" $
    benchFib4 ["-v2"]

  , testCase "verbosity 3" $
    benchFib4 ["-v", "3"]

  , testCase "invalid verbosity" $
    shouldExitFailure $ benchFib4 ["--verbosity", "foo"]

  , testCase "out of range verbosity" $
    shouldExitFailure $ benchFib4 ["--verbosity", "100"]

  , testCase "non existing option" $
    shouldExitFailure $ emptyMain ["--no-such-option"]
  ]

skipping :: TestTree
skipping = testGroup "skipping"
  [ testCase "selecting benchmarks" $
    benchNesting ["2"]

  , testCase "selecting benchmarks, skipping group" $
    benchNesting ["c.1.A"]

  , testCase "no matching benchmark" $
    benchNesting ["no-matching-benchmark"]

  , testCase "selecting under env, strict" $
    shouldExitFailure $
    benchNestingEnvStrict ["fiba"]

  , testCase "selecting under env, strict, under group" $
    shouldExitFailure $
    benchNestingEnvStrictGrouped ["fiba"]

  , testCase "selecting under env" $
    benchForMatch ["a"]
  ]

benchNestingEnvStrictGrouped :: [String] -> IO ()
benchNestingEnvStrictGrouped args =
  defaultMainWith' args
  [ bgroup "a"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , bgroup "b"
    [ env (pure (8, ())) $ \(a, _) ->
        bench "fiba" (nf fib a)
    ]
  ]

substr :: TestTree
substr = testGroup "substr"
  [ testCase "substring match (case sensitive)" $
    substr_test ["--match", "pattern", "oob"] "foobar"

  , testCase "substring match (case insensitive)" $
    substr_test ["-m", "ipattern", "oOB"] "foobar"

  , testCase "prefix match" $
    substr_test ["-m", "prefix", "foo"] "foobar"

  , testCase "invalid match mode" $
    shouldExitFailure $
    defaultMainWith'
    ["-m", "no_such_mode"]
    [ bench "foo" (nf fib 8) ]
  ]
  where
    substr_test args str =
      shouldExitFailure $
      defaultMainWith' args
      [ bench "don't match me" (nfIO exit)
      , bench str (nfIO (exitFailure :: IO ()))
      , bench "don't match me either" (nfIO exit)
      ]
    exit :: IO ()
    exit = exitSuccess

glob :: TestTree
glob = testGroup "glob"
  [ testCase "simple pattern" $
    glob_test "foo" "foo"

  , testCase "pattern with '?'" $
    glob_test "f??" "foo"

  , testCase "pattern with '*'" $
    glob_test "*foo" "foo"

  , testCase "pattern with '*' at the end" $
    glob_test "*f*" "foo"

  , testCase "escaping with '\\'" $
    glob_test "*foo\\?" "foo?"

  , testCase "escape after '*'" $
    glob_test "*foo*\\?" "foo foo foo?"

  , testCase "repeated stars" $
    glob_test "*fo**" "foo"

  , testCase "simple bracket" $
    glob_test "[abcdef]oo" "foo"

  , testCase "bracket with range" $
    glob_test "[a-z]oo" "foo"

  , testCase "bracket with negation" $
    glob_test "[!z]oo" "foo"

  , testCase "bracket with negation and range" $
    glob_test "[!a-d]oo" "foo"

  , testCase "bracket with escapes" $
    glob_test "[\\!\\-][\\!\\-]oo" "!-oo"

  , testCase "char class starting with `]'" $
    glob_test "[]f]oo" "]oo"

  , testCase "char class ending with `-'" $
    glob_test "[]-]oo" "-oo"

  , testCase "char class `[!]a-]'" $
    glob_test "[!]a-]oo" "foo"

  , testCase "brace without nesting 1" $
    glob_test "foo{,d}" "foo"

  , testCase "brace without nesting 2" $
    glob_test "foo{,d}" "food"

  , testCase "brace with nesting" $
    glob_test "foo: {quux,buz{,z}/{,cat,dog}}/bar" "foo: buzz/dog/bar"

  , testCase "brace with escape" $
    glob_test "foo{,\\,} bar{,\\,} and buzz" "foo, bar, and buzz"

  , testCase "unbalanced bracket" $
    glob_test "[foo" "foo"

  , testCase "unbalanced brace" $
    glob_test "{bar" "bar"
  ]
  where
    glob_test pat str =
      shouldExitFailure $
      defaultMainWith'
      ["--match=glob", pat]
      [ bench "skip me" (nfIO (exitSuccess :: IO ()))
      , bench str (nfIO (exitFailure :: IO ())) ]

csv :: TestTree
csv = with_csv_cleanup $ testGroup "csv"
  [ testCase writing_slow_csv $
    benchSlowfib ["--csv", "slow.csv"]

  , after_slow_csv $
    testCase "comparing with baseline" $
    benchFastfib ["--baseline", "slow.csv"]

  , testCase "non-existing baseline" $
    shouldExitFailure $ benchFastfib ["--baseline", "nosuch.csv"]

  , testCase writing_quoted_csv $ do
      benchQuotes ["--csv", "quotes.csv", "-L3"]
      benchNames ["--csv", "quotes.csv"]

  , after_quoted_csv $
    testCase "reading baseline containing quotes" $
    benchQuotes ["--baseline", "quotes.csv", "-L3"]

  , testCase writing_fast_csv $
    benchFastfib ["--csv", "fast.csv"]

  , after_fast_csv $
    testCase "fail if slower" $
    shouldExitFailure $
    benchSlowfib ["--baseline", "fast.csv", "--fail-if-slower", "10"]

  , after_fast_csv $
    testCase "fail if slower, with match" $
    shouldExitFailure $
    benchSlowfib ["--baseline", "fast.csv" ,"--fail-if-slower", "10" ,"fib/16"]

  , testCase "fail if slower, invalid arg" $
    shouldExitFailure $ benchSlowfib ["--fail-if-slower", "foo"]

  , after_slow_csv $
    testCase "fail if faster" $
    shouldExitFailure $
    benchFastfib ["--baseline", "slow.csv", "--fail-if-faster", "10"]

  , testCase "fail if faster, invalid arg" $
    shouldExitFailure $
    benchSlowfib ["--fail-if-faster", "foo"]
  ]
  where
    writing_slow_csv = "writing slow.csv"
    writing_fast_csv = "writing fast.csv"
    writing_quoted_csv = "names containing double quotes"
    after_slow_csv = after AllSucceed writing_slow_csv
    after_fast_csv = after AllSucceed writing_fast_csv
    after_quoted_csv = after AllSucceed writing_quoted_csv
    csv_cleanup _ = do
      removeFile "slow.csv"
      removeFile "fast.csv"
      removeFile "quotes.csv"
      `catch` \e -> case fromException e of
        Just ioe | isDoesNotExistError ioe -> pure ()
        _                                  -> throwIO e
    with_csv_cleanup = withResource (pure ()) csv_cleanup . const

json :: TestTree
json = with_json_cleanup $ testGroup "json"
  [ testCase "write json" $
    benchNames ["--json", "mini.json"]

  , testCase "run once and write json" $
    benchNames ["-s", "Infinity", "--json", "mini.json"]

  , testCase "writing json with timed out result" $
    shouldExitFailure $
    benchFib32 ["-s", "0", "-L", "0.1", "--json", "mini.json"]
  ]
  where
    json_cleanup _ = removeFile "mini.json"
    with_json_cleanup = withResource (pure ()) json_cleanup . const

report :: TestTree
report = with_report_cleanup $ testGroup "report"
  [ testCase "write report and json" $
    benchNames ["--output", "test1.html", "--json", "test.json"]
  , testCase "write report without json" $
    benchNames ["-o", "test2.html"]
  ]
  where
    report_cleanup _ = mapM_ removeFile ["test1.html", "test2.html"]
    with_report_cleanup = withResource (pure ()) report_cleanup . const

timelimit :: TestTree
timelimit = testGroup "timeout"
  [ testCase "time limit, long name" $
    shouldExitFailure $
    benchFib32 ["--time-limit", "1e-6", "--stddev", "1e-9"]

  , testCase "time limit, short name" $
    shouldExitFailure $
    benchFib32 ["-L", "1e-9", "--stddev", "1e-32"]

  , testCase "time limit, return before the limit" $
    benchFib32 ["-L", "5", "--stddev", "10"]

  , testCase "invalid time limit arg" $
    shouldExitFailure $
    benchFib32 ["--time-limit", "foo"]

  ]

#ifdef DEV
formatPicos :: TestTree
formatPicos = testGroup "format picos"
  [ testCase "pico seconds" $ do
      assertPicos 1 "1.000 ps"
      assertPicos 12 "12.00 ps"
      assertPicos 123 "123.0 ps"

  , testCase "nano seconds" $ do
      assertPicos 1234 "1.234 ns"
      assertPicos 12345 "12.34 ns"
      assertPicos 123456 "123.5 ns"

  , testCase "micro seconds" $ do
      e <- getDefaultMEnv defaultConfig
      assertPicos 1234567 ("1.235 " ++ [mu e] ++ "s")
      assertPicos 12345678 ("12.35 " ++ [mu e] ++ "s")
      assertPicos 123456789 ("123.5 " ++ [mu e] ++ "s")

  , testCase "milli seconds" $ do
      assertPicos 1234567890 "1.235 ms"
      assertPicos 12345678901 "12.35 ms"
      assertPicos 123456789012 "123.5 ms"

  , testCase "seconds" $ do
      assertPicos 1234567890123 "1.235 s "
      assertPicos 12345678901234 "12.35 s "
      assertPicos 123456789012345 "123.5 s "
      assertPicos 1234567890123456 "1234.6 s"
  ]
  where
    assertPicos n str = do
      menv <- getDefaultMEnv defaultConfig
      let picos = docToString menv (showPicos5 n)
      assertEqual (show n) str picos

formatBytes :: TestTree
formatBytes = testGroup "format bytes"
  [ testCase "bytes" $ do
      assertBytes 999 " 999 B"

  , testCase "kilobytes" $ do
      assertBytes 10188 "9.9 KB"
      assertBytes 1023487 "999 KB"

  , testCase "megabytes" $ do
      assertBytes 1043331 "1.0 MB"
      assertBytes 1048051711 "999 MB"

  , testCase "gigabytes" $ do
      assertBytes 10683731148 "9.9 GB"
      assertBytes 1073204953087 "999 GB"

  , testCase "terabytes" $ do
      assertBytes 10940140696371 "9.9 TB"
      assertBytes 1098961871962111 "999 TB"

  , testCase "petabytes" $ do
      assertBytes 11202704073084106 "9.9 PB"
      assertBytes 1125336956889202623 "999 PB"

  , testCase "exabytes" $ do
      assertBytes 11471568970838124590 "9.9 EB"
      assertBytes maxBound " 16 EB"
  ]
  where
    assertBytes n str = assertEqual (show n) str (showBytes n)
#endif


-- ------------------------------------------------------------------------
-- Auxiliary
-- ------------------------------------------------------------------------

defaultMain' :: [Benchmark] -> IO ()
defaultMain' = defaultMainWith' []

defaultMainWith' :: [String] -> [Benchmark] -> IO ()
defaultMainWith' args = quietly . withArgs args' . Miniterion.defaultMain
  where
#if linux_HOST_OS
    -- Running the tests in CI for other os than Linux is slow, using
    -- higher value for stddev to make it faster to terminate.
    args' = args
#else
    args' = "--stddev=20" : args
#endif

quietly :: IO a -> IO a
quietly = hSilence [stdout, stderr]

fib :: Int -> Integer
fib n = if n < 2 then toInteger n else fib (n-1) + fib (n-2)

ifib :: Int -> Int
ifib n = if n < 2 then n else ifib (n-1) + ifib (n-2)

fastfib :: Int -> Integer
fastfib n = fibs !! n where
  fibs = 0 : 1 : rest
  rest = case fibs of
           _:tl -> zipWith (+) tl fibs
           []   -> error "impossible happened!"

wc :: String -> Int
wc = length . words

wcIO :: FilePath -> IO Int
wcIO = fmap wc . readFile

shouldExitFailure :: IO a -> IO ()
shouldExitFailure act = void (act >> throwIO ExitSuccess) `catch` \e ->
  case fromException e of
    Just (ExitFailure {}) -> pure ()
    _                     -> throwIO e

emptyMain :: [String] -> IO ()
emptyMain args = defaultMainWith' args []

miniterionDotCabal :: FilePath
miniterionDotCabal = "miniterion.cabal"

benchFib4 :: [String] -> IO ()
benchFib4 args =
  defaultMainWith' args
  [ bgroup "fib"
    [ bench "4" (nf fib 4) ]]

benchWithEnvAndPat :: [String] -> IO ()
benchWithEnvAndPat args =
  defaultMainWith' args
  [ env (pure (3, 4)) $ \ (a, b) ->
      bgroup "fib"
      [ bench "a" (nf fib a)
      , bench "b" (nf fib b) ]]

benchWithEnvAndIrrPat :: [String] -> IO ()
benchWithEnvAndIrrPat args =
  defaultMainWith' args
  [ env (pure (3, 4)) $ \ ~(a, b) ->
      bgroup "fib"
      [ bench "a" (nf fib a)
      , bench "b" (nf fib b) ]]

s, p :: Benchmark
s = bench "succ" (nf (succ :: Int -> Int) 1)
p = bench "pred" (nf (pred :: Int -> Int) 1)

benchNesting :: [String] -> IO ()
benchNesting args =
  defaultMainWith' args
  [ bgroup "a" [s, p]
  , bgroup "b"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , bgroup "c"
    [ bgroup "1"
      [ bgroup "A" [s, p] ]
    , bgroup "2"
      [ bgroup "B" [s, p] ]]]

benchNestingEnvStrict :: [String] -> IO ()
benchNestingEnvStrict args =
  defaultMainWith' args
  [ bgroup "a"
    [ bgroup "1" [s, p]
    , bgroup "2" [s, p] ]
  , env (pure (1, 2)) $ \ (a, b) ->
      bgroup "b"
      [ bench "fiba" (nf fib a)
      , bench "fibb" (nf fib b) ]]

benchForMatch :: [String] -> IO ()
benchForMatch args =
  defaultMainWith' args
  [ bgroup "a"
    [ bgroup "a1" [s, p]
    , bgroup "a2" [s, p] ]
  , env (pure ()) $ \_ ->
      bgroup "b" [s, p] ]

benchSlowfib :: [String] -> IO ()
benchSlowfib args =
  defaultMainWith' args
  [ bgroup "fib"
    [ bench "4" (nf fib 4)
    , bench "8" (nf fib 8)
    , bench "16" (nf fib 16) ]]

benchFib32 :: [String] -> IO ()
benchFib32 args =
  defaultMainWith' args
  [ bgroup "fib"
    [ bench "32" (nf fib 32) ]]

benchFastfib :: [String] -> IO ()
benchFastfib args =
  defaultMainWith' args
  [ bgroup "fib"
    [ bench "4" (nf fib 4)
    , bench "8" (nf fastfib 8)
    , bench "16" (nf fastfib 16) ]]

benchQuotes :: [String] -> IO ()
benchQuotes args =
  defaultMainWith' args
  [ bgroup "group \"one\""
    [ bgroup "a" [s, p]
    , bgroup  "b" [s, p] ]
  , bgroup "group two"
    [ bench "\"a\"" (nf fromEnum 'a')
    , bench "\"b\"" (nf fromEnum 'z')]
  , bgroup "group three"
    [ bench "'\"'" (nf fromEnum '"')
    , bench "\"'\"" (nf fromEnum '\'')]
  ]

benchNames :: [String] -> IO ()
benchNames args =
  defaultMainWith' args
  [ bgroup "names"
    [ bench "containing \"double quotes\"" (nf fromEnum 'a')
    , bench "containing 'single quotes'" (nf fromEnum 'b')
    , bench "containing\nnew\nlines" (nf fromEnum 'c')
    , bench "containing lt, gt, amp <>&" (nf fromEnum 'd')
    , bench "containing \\\\ back slashes \\\\" (nf fromEnum 'e')]
    , bench "containing unbalanced \" double quote" (nf fromEnum 'f')
  ]
