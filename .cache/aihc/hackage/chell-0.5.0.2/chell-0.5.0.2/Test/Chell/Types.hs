module Test.Chell.Types
  ( Test,
    test,
    testName,
    TestOptions,
    defaultTestOptions,
    testOptionSeed,
    testOptionTimeout,
    TestResult (TestPassed, TestSkipped, TestFailed, TestAborted),
    Failure,
    failure,
    failureLocation,
    failureMessage,
    Location,
    location,
    locationFile,
    locationModule,
    locationLine,
    Suite,
    suite,
    suiteName,
    suiteTests,
    SuiteOrTest,
    skipIf,
    skipWhen,
    runTest,
    handleJankyIO,
  )
where

import Control.Exception (Handler (..), SomeException, catches, throwIO)
import Control.Exception qualified
import System.Timeout (timeout)

-- | A 'Test' is, essentially, an IO action that returns a 'TestResult'. Tests
-- are aggregated into suites (see 'Suite').
data Test
  = Test String (TestOptions -> IO TestResult)

instance Show Test where
  showsPrec d (Test name _) = showParen (d > 10) (showString "Test " . shows name)

-- | Define a test, with the given name and implementation.
test :: String -> (TestOptions -> IO TestResult) -> Test
test = Test

-- | Get the name a test was given when it was defined; see 'test'.
testName :: Test -> String
testName (Test name _) = name

-- | Test options are passed to each test, and control details about how the
-- test should be run.
data TestOptions = TestOptions
  { -- | Get the RNG seed for this test run. The seed is generated once, in
    -- 'defaultMain', and used for all tests. It is also logged to reports
    -- using a note.
    --
    -- When using 'defaultMain', users may specify a seed using the
    -- @--seed@ command-line option.
    --
    -- 'testOptionSeed' is a field accessor, and can be used to update
    -- a 'TestOptions' value.
    testOptionSeed :: Int,
    -- | An optional timeout, in milliseconds. Tests which run longer than
    -- this timeout will be aborted.
    --
    -- When using 'defaultMain', users may specify a timeout using the
    -- @--timeout@ command-line option.
    --
    -- 'testOptionTimeout' is a field accessor, and can be used to update
    -- a 'TestOptions' value.
    testOptionTimeout :: Maybe Int
  }
  deriving (Show, Eq)

-- | Default test options.
--
-- >$ ghci
-- >Prelude> import Test.Chell
-- >
-- >Test.Chell> testOptionSeed defaultTestOptions
-- >0
-- >
-- >Test.Chell> testOptionTimeout defaultTestOptions
-- >Nothing
defaultTestOptions :: TestOptions
defaultTestOptions =
  TestOptions
    { testOptionSeed = 0,
      testOptionTimeout = Nothing
    }

-- | The result of running a test.
--
-- To support future extensions to the testing API, any users of this module
-- who pattern-match against the 'TestResult' constructors should include a
-- default case. If no default case is provided, a warning will be issued.
data TestResult
  = -- | The test passed, and generated the given notes.
    TestPassed [(String, String)]
  | -- | The test did not run, because it was skipped with 'skipIf'
    -- or 'skipWhen'.
    TestSkipped
  | -- | The test failed, generating the given notes and failures.
    TestFailed [(String, String)] [Failure]
  | -- | The test aborted with an error message, and generated the given
    -- notes.
    TestAborted [(String, String)] String
  | -- Not exported; used to generate GHC warnings for users who don't
    -- provide a default case.
    TestResultCaseMustHaveDefault
  deriving (Show, Eq)

-- | Contains details about a test failure.
data Failure = Failure
  { -- | If given, the location of the failing assertion, expectation,
    -- etc.
    --
    -- 'failureLocation' is a field accessor, and can be used to update
    -- a 'Failure' value.
    failureLocation :: Maybe Location,
    -- | If given, a message which explains why the test failed.
    --
    -- 'failureMessage' is a field accessor, and can be used to update
    -- a 'Failure' value.
    failureMessage :: String
  }
  deriving (Show, Eq)

-- | An empty 'Failure'; use the field accessors to populate this value.
failure :: Failure
failure = Failure Nothing ""

-- | Contains details about a location in the test source file.
data Location = Location
  { -- | A path to a source file, or empty if not provided.
    --
    -- 'locationFile' is a field accessor, and can be used to update
    -- a 'Location' value.
    locationFile :: String,
    -- | A Haskell module name, or empty if not provided.
    --
    -- 'locationModule' is a field accessor, and can be used to update
    -- a 'Location' value.
    locationModule :: String,
    -- | A line number, or Nothing if not provided.
    --
    -- 'locationLine' is a field accessor, and can be used to update
    -- a 'Location' value.
    locationLine :: Maybe Integer
  }
  deriving (Show, Eq)

-- | An empty 'Location'; use the field accessors to populate this value.
location :: Location
location = Location "" "" Nothing

-- | A suite is a named collection of tests.
--
-- Note: earlier versions of Chell permitted arbitrary nesting of test suites.
-- This feature proved too unwieldy, and was removed. A similar result can be
-- achieved with 'suiteTests'; see the documentation for 'suite'.
data Suite
  = Suite String [Test]
  deriving (Show)

class SuiteOrTest a where
  skipIf_ :: Bool -> a -> a
  skipWhen_ :: IO Bool -> a -> a

instance SuiteOrTest Suite where
  skipIf_ skip s@(Suite name children) =
    if skip
      then Suite name (map (skipIf_ skip) children)
      else s

  skipWhen_ p (Suite name children) =
    Suite name (map (skipWhen_ p) children)

instance SuiteOrTest Test where
  skipIf_ skip t@(Test name _) =
    if skip
      then Test name (\_ -> return TestSkipped)
      else t

  skipWhen_ p (Test name io) =
    Test
      name
      ( \opts ->
          do
            skip <- p
            if skip then return TestSkipped else io opts
      )

-- | Conditionally skip tests. Use this to avoid commenting out tests
-- which are currently broken, or do not work on the current platform.
--
-- @
-- tests :: Suite
-- tests = 'suite' \"tests\"
--    [ test_Foo
--    , 'skipIf' builtOnUnix test_WindowsSpecific
--    , test_Bar
--    ]
-- @
skipIf :: SuiteOrTest a => Bool -> a -> a
skipIf = skipIf_

-- | Conditionally skip tests, depending on the result of a runtime check. The
-- predicate is checked before each test is started.
--
-- @
-- tests :: Suite
-- tests = 'suite' \"tests\"
--    [ test_Foo
--    , 'skipWhen' noNetwork test_PingGoogle
--    , test_Bar
--    ]
-- @
skipWhen :: SuiteOrTest a => IO Bool -> a -> a
skipWhen = skipWhen_

-- | Define a new 'Suite', with the given name and children.
--
-- Note: earlier versions of Chell permitted arbitrary nesting of test suites.
-- This feature proved too unwieldy, and was removed. A similar result can be
-- achieved with 'suiteTests':
--
-- @
-- test_Addition :: Test
-- test_Subtraction :: Test
-- test_Show :: Test
--
-- suite_Math :: Suite
-- suite_Math = 'suite' \"math\"
--    [ test_Addition
--    , test_Subtraction
--    ]
--
-- suite_Prelude :: Suite
-- suite_Prelude = 'suite' \"prelude\"
--    (
--      [ test_Show
--      ]
--      ++ suiteTests suite_Math
--    )
-- @
suite :: String -> [Test] -> Suite
suite = Suite

-- | Get a suite's name. Suite names may be any string, but are typically
-- plain ASCII so users can easily type them on the command line.
--
-- >$ ghci chell-example.hs
-- >Ok, modules loaded: Main.
-- >
-- >*Main> suiteName tests_Math
-- >"math"
suiteName :: Suite -> String
suiteName (Suite name _) = name

-- | Get the full list of tests contained within this 'Suite'. Each test is
-- given its full name within the test hierarchy, where names are separated
-- by periods.
--
-- >$ ghci chell-example.hs
-- >Ok, modules loaded: Main.
-- >
-- >*Main> suiteTests tests_Math
-- >[Test "math.addition",Test "math.subtraction"]
suiteTests :: Suite -> [Test]
suiteTests = go ""
  where
    prefixed prefix str =
      if null prefix
        then str
        else prefix ++ "." ++ str

    go prefix (Suite name children) =
      concatMap (step (prefixed prefix name)) children

    step prefix (Test name io) = [Test (prefixed prefix name) io]

-- | Run a test, wrapped in error handlers. This will return 'TestAborted' if
-- the test throws an exception or times out.
runTest :: Test -> TestOptions -> IO TestResult
runTest (Test _ io) options = handleJankyIO options (io options) (return [])

handleJankyIO :: TestOptions -> IO TestResult -> IO [(String, String)] -> IO TestResult
handleJankyIO opts getResult getNotes =
  do
    let withTimeout =
          case testOptionTimeout opts of
            Just time -> timeout (time * 1000)
            Nothing -> fmap Just

    let hitTimeout = str
          where
            str = "Test timed out after " ++ show time ++ " milliseconds"
            Just time = testOptionTimeout opts

    tried <- withTimeout (try getResult)
    case tried of
      Just (Right ret) -> return ret
      Nothing ->
        do
          notes <- getNotes
          return (TestAborted notes hitTimeout)
      Just (Left err) ->
        do
          notes <- getNotes
          return (TestAborted notes err)

try :: IO a -> IO (Either String a)
try io = catches (fmap Right io) [Handler handleAsync, Handler handleExc]
  where
    handleAsync :: Control.Exception.AsyncException -> IO a
    handleAsync = throwIO

    handleExc :: SomeException -> IO (Either String a)
    handleExc exc = return (Left ("Test aborted due to exception: " ++ show exc))
