module Test.Chell.Main (defaultMain) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State qualified as State
import Control.Monad.Trans.Writer qualified as Writer
import Data.Char (ord)
import Data.List (isPrefixOf)
import Options
import System.Exit (exitFailure, exitSuccess)
import System.IO (IOMode (..), hIsTerminalDevice, hPutStr, hPutStrLn, stderr, stdout, withBinaryFile)
import System.Random (randomIO)
import Test.Chell.Output
import Test.Chell.Types
import Text.Printf (printf)

data MainOptions = MainOptions
  { optVerbose :: Bool,
    optXmlReport :: String,
    optJsonReport :: String,
    optTextReport :: String,
    optSeed :: Maybe Int,
    optTimeout :: Maybe Int,
    optColor :: ColorMode
  }

optionType_ColorMode :: OptionType ColorMode
optionType_ColorMode = optionType "ColorMode" ColorModeAuto parseMode showMode
  where
    parseMode s =
      case s of
        "always" -> Right ColorModeAlways
        "never" -> Right ColorModeNever
        "auto" -> Right ColorModeAuto
        _ -> Left (show s ++ " is not in {\"always\", \"never\", \"auto\"}.")
    showMode mode =
      case mode of
        ColorModeAlways -> "always"
        ColorModeNever -> "never"
        ColorModeAuto -> "auto"

instance Options MainOptions where
  defineOptions =
    pure MainOptions
      <*> defineOption
        optionType_bool
        ( \o ->
            o
              { optionShortFlags = ['v'],
                optionLongFlags = ["verbose"],
                optionDefault = False,
                optionDescription = "Print more output."
              }
        )
      <*> simpleOption
        "xml-report"
        ""
        "Write a parsable report to a given path, in XML."
      <*> simpleOption
        "json-report"
        ""
        "Write a parsable report to a given path, in JSON."
      <*> simpleOption
        "text-report"
        ""
        "Write a human-readable report to a given path."
      <*> simpleOption
        "seed"
        Nothing
        "The seed used for random numbers in (for example) quickcheck."
      <*> simpleOption
        "timeout"
        Nothing
        "The maximum duration of a test, in milliseconds."
      <*> defineOption
        optionType_ColorMode
        ( \o ->
            o
              { optionLongFlags = ["color"],
                optionDefault = ColorModeAuto,
                optionDescription = "Whether to enable color ('always', 'auto', or 'never')."
              }
        )

-- | A simple default main function, which runs a list of tests and logs
-- statistics to stdout.
defaultMain :: [Suite] -> IO ()
defaultMain suites = runCommand $ \opts args ->
  do
    -- validate/sanitize test options
    seed <-
      case optSeed opts of
        Just s -> return s
        Nothing -> randomIO
    timeout <-
      case optTimeout opts of
        Nothing -> return Nothing
        Just t ->
          if toInteger t * 1000 > toInteger (maxBound :: Int)
            then do
              hPutStrLn stderr "Test.Chell.defaultMain: Ignoring --timeout because it is too large."
              return Nothing
            else return (Just t)
    let testOptions =
          defaultTestOptions
            { testOptionSeed = seed,
              testOptionTimeout = timeout
            }

    -- find which tests to run
    let allTests = concatMap suiteTests suites
        tests =
          if null args
            then allTests
            else filter (matchesFilter args) allTests

    -- output mode
    output <-
      case optColor opts of
        ColorModeNever -> return (plainOutput (optVerbose opts))
        ColorModeAlways -> return (colorOutput (optVerbose opts))
        ColorModeAuto ->
          do
            isTerm <- hIsTerminalDevice stdout
            return $
              if isTerm
                then colorOutput (optVerbose opts)
                else plainOutput (optVerbose opts)

    -- run tests
    results <- forM tests $ \t ->
      do
        outputStart output t
        result <- runTest t testOptions
        outputResult output t result
        return (t, result)

    -- generate reports
    let reports = getReports opts

    forM_ reports $ \(path, fmt, toText) ->
      withBinaryFile path WriteMode $ \h ->
        do
          when (optVerbose opts) $
            putStrLn ("Writing " ++ fmt ++ " report to " ++ show path)
          hPutStr h (toText results)

    let stats = resultStatistics results
        (_, _, failed, aborted) = stats
    putStrLn (formatResultStatistics stats)

    if failed == 0 && aborted == 0
      then exitSuccess
      else exitFailure

matchesFilter :: [String] -> Test -> Bool
matchesFilter filters = check
  where
    check t = any (matchName (testName t)) filters
    matchName name f = f == name || isPrefixOf (f ++ ".") name

type Report = [(Test, TestResult)] -> String

getReports :: MainOptions -> [(String, String, Report)]
getReports opts = concat [xml, json, text]
  where
    xml = case optXmlReport opts of
      "" -> []
      path -> [(path, "XML", xmlReport)]
    json = case optJsonReport opts of
      "" -> []
      path -> [(path, "JSON", jsonReport)]
    text = case optTextReport opts of
      "" -> []
      path -> [(path, "text", textReport)]

jsonReport :: [(Test, TestResult)] -> String
jsonReport results = Writer.execWriter writer
  where
    tell = Writer.tell

    writer =
      do
        tell "{\"test-runs\": ["
        commas results tellResult
        tell "]}"

    tellResult (t, result) =
      case result of
        TestPassed notes ->
          do
            tell "{\"test\": \""
            tell (escapeJSON (testName t))
            tell "\", \"result\": \"passed\""
            tellNotes notes
            tell "}"
        TestSkipped ->
          do
            tell "{\"test\": \""
            tell (escapeJSON (testName t))
            tell "\", \"result\": \"skipped\"}"
        TestFailed notes fs ->
          do
            tell "{\"test\": \""
            tell (escapeJSON (testName t))
            tell "\", \"result\": \"failed\", \"failures\": ["
            commas fs $ \f ->
              do
                tell "{\"message\": \""
                tell (escapeJSON (failureMessage f))
                tell "\""
                case failureLocation f of
                  Just loc ->
                    do
                      tell ", \"location\": {\"module\": \""
                      tell (escapeJSON (locationModule loc))
                      tell "\", \"file\": \""
                      tell (escapeJSON (locationFile loc))
                      case locationLine loc of
                        Just line ->
                          do
                            tell "\", \"line\": "
                            tell (show line)
                        Nothing -> tell "\""
                      tell "}"
                  Nothing -> return ()
                tell "}"
            tell "]"
            tellNotes notes
            tell "}"
        TestAborted notes msg ->
          do
            tell "{\"test\": \""
            tell (escapeJSON (testName t))
            tell "\", \"result\": \"aborted\", \"abortion\": {\"message\": \""
            tell (escapeJSON msg)
            tell "\"}"
            tellNotes notes
            tell "}"
        _ -> return ()

    escapeJSON =
      concatMap
        ( \c ->
            case c of
              '"' -> "\\\""
              '\\' -> "\\\\"
              _ | ord c <= 0x1F -> printf "\\u%04X" (ord c)
              _ -> [c]
        )

    tellNotes notes =
      do
        tell ", \"notes\": ["
        commas notes $ \(key, value) ->
          do
            tell "{\"key\": \""
            tell (escapeJSON key)
            tell "\", \"value\": \""
            tell (escapeJSON value)
            tell "\"}"
        tell "]"

    commas xs block = State.evalStateT (commaState xs block) False
    commaState xs block = forM_ xs $ \x ->
      do
        let tell' = lift . Writer.tell
        needComma <- State.get
        if needComma
          then tell' "\n, "
          else tell' "\n  "
        State.put True
        lift (block x)

xmlReport :: [(Test, TestResult)] -> String
xmlReport results = Writer.execWriter writer
  where
    tell = Writer.tell

    writer =
      do
        tell "<?xml version=\"1.0\" encoding=\"utf8\"?>\n"
        tell "<report xmlns='urn:john-millikin:chell:report:1'>\n"
        mapM_ tellResult results
        tell "</report>"

    tellResult (t, result) =
      case result of
        TestPassed notes ->
          do
            tell "\t<test-run test='"
            tell (escapeXML (testName t))
            tell "' result='passed'>\n"
            tellNotes notes
            tell "\t</test-run>\n"
        TestSkipped ->
          do
            tell "\t<test-run test='"
            tell (escapeXML (testName t))
            tell "' result='skipped'/>\n"
        TestFailed notes fs ->
          do
            tell "\t<test-run test='"
            tell (escapeXML (testName t))
            tell "' result='failed'>\n"
            forM_ fs $ \f ->
              do
                tell "\t\t<failure message='"
                tell (escapeXML (failureMessage f))
                case failureLocation f of
                  Just loc ->
                    do
                      tell "'>\n"
                      tell "\t\t\t<location module='"
                      tell (escapeXML (locationModule loc))
                      tell "' file='"
                      tell (escapeXML (locationFile loc))
                      case locationLine loc of
                        Just line ->
                          do
                            tell "' line='"
                            tell (show line)
                        Nothing -> return ()
                      tell "'/>\n"
                      tell "\t\t</failure>\n"
                  Nothing -> tell "'/>\n"
            tellNotes notes
            tell "\t</test-run>\n"
        TestAborted notes msg ->
          do
            tell "\t<test-run test='"
            tell (escapeXML (testName t))
            tell "' result='aborted'>\n"
            tell "\t\t<abortion message='"
            tell (escapeXML msg)
            tell "'/>\n"
            tellNotes notes
            tell "\t</test-run>\n"
        _ -> return ()

    escapeXML =
      concatMap
        ( \c ->
            case c of
              '&' -> "&amp;"
              '<' -> "&lt;"
              '>' -> "&gt;"
              '"' -> "&quot;"
              '\'' -> "&apos;"
              _ -> [c]
        )

    tellNotes notes = forM_ notes $ \(key, value) ->
      do
        tell "\t\t<note key=\""
        tell (escapeXML key)
        tell "\" value=\""
        tell (escapeXML value)
        tell "\"/>\n"

textReport :: [(Test, TestResult)] -> String
textReport results = Writer.execWriter writer
  where
    tell = Writer.tell

    writer =
      do
        forM_ results tellResult
        let stats = resultStatistics results
        tell (formatResultStatistics stats)

    tellResult (t, result) =
      case result of
        TestPassed notes ->
          do
            tell (replicate 70 '=')
            tell "\n"
            tell "PASSED: "
            tell (testName t)
            tell "\n"
            tellNotes notes
            tell "\n\n"
        TestSkipped ->
          do
            tell (replicate 70 '=')
            tell "\n"
            tell "SKIPPED: "
            tell (testName t)
            tell "\n\n"
        TestFailed notes fs ->
          do
            tell (replicate 70 '=')
            tell "\n"
            tell "FAILED: "
            tell (testName t)
            tell "\n"
            tellNotes notes
            tell (replicate 70 '-')
            tell "\n"
            forM_ fs $ \f ->
              do
                case failureLocation f of
                  Just loc ->
                    do
                      tell (locationFile loc)
                      case locationLine loc of
                        Just line ->
                          do
                            tell ":"
                            tell (show line)
                        Nothing -> return ()
                      tell "\n"
                  Nothing -> return ()
                tell (failureMessage f)
                tell "\n\n"
        TestAborted notes msg ->
          do
            tell (replicate 70 '=')
            tell "\n"
            tell "ABORTED: "
            tell (testName t)
            tell "\n"
            tellNotes notes
            tell (replicate 70 '-')
            tell "\n"
            tell msg
            tell "\n\n"
        _ -> return ()

    tellNotes notes = forM_ notes $ \(key, value) ->
      do
        tell key
        tell "="
        tell value
        tell "\n"

formatResultStatistics :: (Integer, Integer, Integer, Integer) -> String
formatResultStatistics stats = Writer.execWriter writer
  where
    writer =
      do
        let (passed, skipped, failed, aborted) = stats

        if failed == 0 && aborted == 0
          then Writer.tell "PASS: "
          else Writer.tell "FAIL: "

        let putNum comma n what =
              Writer.tell $
                if n == 1
                  then comma ++ "1 test " ++ what
                  else comma ++ show n ++ " tests " ++ what

        let total = sum [passed, skipped, failed, aborted]

        putNum "" total "run"
        (putNum ", " passed "passed")
        when (skipped > 0) (putNum ", " skipped "skipped")
        when (failed > 0) (putNum ", " failed "failed")
        when (aborted > 0) (putNum ", " aborted "aborted")

resultStatistics :: [(Test, TestResult)] -> (Integer, Integer, Integer, Integer)
resultStatistics results = State.execState state (0, 0, 0, 0)
  where
    state = forM_ results $ \(_, result) -> case result of
      TestPassed {} -> State.modify (\(p, s, f, a) -> (p + 1, s, f, a))
      TestSkipped {} -> State.modify (\(p, s, f, a) -> (p, s + 1, f, a))
      TestFailed {} -> State.modify (\(p, s, f, a) -> (p, s, f + 1, a))
      TestAborted {} -> State.modify (\(p, s, f, a) -> (p, s, f, a + 1))
      _ -> return ()
