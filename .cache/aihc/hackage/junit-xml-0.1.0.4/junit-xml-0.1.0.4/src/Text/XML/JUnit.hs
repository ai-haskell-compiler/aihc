{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- A module for producing JUnit style XML reports, for consumption by CI
-- platforms like Jenkins.
-- Please see the README at <https://github.com/jwoudenberg/junit-xml>.
module Text.XML.JUnit
  ( -- * Writing test reports
    writeXmlReport,

    -- * Test report constructors
    passed,
    skipped,
    failed,
    errored,
    inSuite,

    -- * Adding test report details
    stdout,
    stderr,
    time,
    failureMessage,
    failureStackTrace,
    errorMessage,
    errorStackTrace,

    -- * Helper types
    TestReport,
    TestSuite,
  )
where

import Data.Function ((&))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO
import GHC.Exts (fromList)
import qualified Text.XML as XML

-- | This function writes an xml report to the provided path.
--
-- @
--     import Data.Function ((&))
--
--     writeXmlReport "report.xml"
--       [ 'passed' "A passing test"
--           & 'inSuite' "Test suite"
--       , 'failed' "A failing test"
--           & 'inSuite' "Test suite"
--       ]
-- @
writeXmlReport :: FilePath -> [TestSuite] -> IO ()
writeXmlReport out =
  Data.Text.Lazy.IO.writeFile out . XML.renderText XML.def . encode

-- | The report for a single test case.
data TestReport outcome where
  TestReport ::
    Outcome outcome =>
    { testName' :: T.Text,
      outcome' :: outcome,
      stdout' :: Maybe T.Text,
      stderr' :: Maybe T.Text,
      time' :: Maybe Double
    } ->
    TestReport
      outcome

-- | A test report annotated with the test suite it is part of.
data TestSuite
  = TestSuite
      { suiteName :: T.Text,
        testReport :: XML.Element,
        counts :: Counts
      }

-- | Wrap a test report in a suite, allowing it to be added to the list of
-- reports passed to 'writeXmlReports'.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ 'passed' "Passed test"
--           & inSuite "Some test suite"
--       ]
--
-- @
inSuite :: T.Text -> TestReport outcome -> TestSuite
inSuite name test@TestReport {outcome', time'} =
  TestSuite
    { suiteName = name,
      testReport = encodeTestCase test,
      counts = (outcomeCounter outcome') {cumTime = fromMaybe 0 time'}
    }

mapTest :: (a -> a) -> TestReport a -> TestReport a
mapTest f test = test {outcome' = f (outcome' test)}

-- | Add the stdout produced running a test to the report for that test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ 'passed' "A passing test"
--           & stdout "Test ran succesfully!"
--           & 'inSuite' "Test suite"
--       ]
-- @
stdout :: T.Text -> TestReport outcome -> TestReport outcome
stdout log test = test {stdout' = Just log}

-- | Add the stderr produced running a test to the report for that test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ 'failed' "A failing test"
--           & stderr "Expected 4, but got 2."
--           & 'inSuite' "Test suite"
--       ]
-- @
stderr :: T.Text -> TestReport outcome -> TestReport outcome
stderr log test = test {stderr' = Just log}

-- | Add the running time of a test to the report for that test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ 'passed' "A passing test"
--           & time 0.003
--           & 'inSuite' "Test suite"
--       ]
-- @
time :: Double -> TestReport outcome -> TestReport outcome
time seconds test = test {time' = Just seconds}

-- | Create a report for a passing test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ passed "A passing test"
--           & 'stdout' "Test ran succesfully!"
--           & 'stderr' "Warning: don't overcook the vegetables!"
--           & 'time' 0.003
--           & 'inSuite' "Test suite"
--       ]
-- @
passed :: T.Text -> TestReport Passed
passed name =
  TestReport
    { testName' = name,
      outcome' = Passed,
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

-- | Create a report for a skipped test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ skipped "A skipped test"
--           & 'inSuite' "Test suite"
--       ]
-- @
skipped :: T.Text -> TestReport Skipped
skipped name =
  TestReport
    { testName' = name,
      outcome' = Skipped,
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

-- | Create a report for a failed test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ failed "A failing test"
--           & 'stdout' "Running test..."
--           & 'stderr' "Test failed: expected 3 slices of pizza but got one."
--           & 'failureMessage' "Not enough pizza"
--           & 'failureStackTrace' ["pizza", "pizzeria", "italy"]
--           & 'time' 0.08
--           & 'inSuite' "Test suite"
--       ]
-- @
failed :: T.Text -> TestReport Failed
failed name =
  TestReport
    { testName' = name,
      outcome' = Failure Nothing [],
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

-- | Create a report for a test that threw an error.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ errored "A test that threw an error"
--           & 'stdout' "Running test..."
--           & 'stderr' "Unexpected exception: BedTime"
--           & 'errorMessage' "Operation canceled due to BedTimeOut"
--           & 'errorStackTrace' ["bed", "sleep", "night"]
--           & 'time' 0.08
--           & 'inSuite' "Test suite"
--       ]
-- @
errored :: T.Text -> TestReport Errored
errored name =
  TestReport
    { testName' = name,
      outcome' = Error Nothing [],
      stdout' = Nothing,
      stderr' = Nothing,
      time' = Nothing
    }

class Outcome a where
  outcomeToXML :: a -> Maybe XML.Element

  outcomeCounter :: a -> Counts

data Passed = Passed

instance Outcome Passed where
  outcomeToXML _ = Nothing

  outcomeCounter _ = mempty {cumTests = 1}

data Skipped = Skipped

instance Outcome Skipped where
  outcomeToXML _ = Just $ XML.Element "skipped" mempty []

  outcomeCounter _ = mempty {cumSkipped = 1, cumTests = 1}

data Failed
  = Failure
      { -- Warning: newlines in the failure message will look like spaces in the
        -- Jenkins UI!
        failureMessage' :: Maybe T.Text,
        failureStackTrace' :: [T.Text]
      }

instance Outcome Failed where
  outcomeToXML = Just . encodeFailure

  outcomeCounter _ = mempty {cumFailed = 1, cumTests = 1}

-- | Add an error message to the report of a failed test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ failed "A failing test"
--           & failureMessage "Laundromat exceeds noise tolerance."
--           & 'inSuite' "Test suite"
--       ]
-- @
failureMessage :: T.Text -> TestReport Failed -> TestReport Failed
failureMessage msg test =
  mapTest (\outcome -> outcome {failureMessage' = Just msg}) test

-- | Add a stack trace to the report of a failed test.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ failed "A failing test"
--           & failureStackTrace ["ankleClass", "legClass", "limbClass"]
--           & 'inSuite' "Test suite"
--       ]
-- @
failureStackTrace :: [T.Text] -> TestReport Failed -> TestReport Failed
failureStackTrace trace test =
  mapTest (\outcome -> outcome {failureStackTrace' = trace}) test

data Errored
  = Error
      { -- Warning: newlines in the failure message will look like spaces in the
        -- Jenkins UI!
        errorMessage' :: Maybe T.Text,
        errorStackTrace' :: [T.Text]
      }

instance Outcome Errored where
  outcomeToXML = Just . encodeError

  outcomeCounter _ = mempty {cumErrored = 1, cumTests = 1}

-- | Add an error message to the report for a test that threw an exception.
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ errored "A test that threw an error"
--           & errorMessage "Too much Netflix"
--           & 'inSuite' "Test suite"
--       ]
-- @
errorMessage :: T.Text -> TestReport Errored -> TestReport Errored
errorMessage msg test =
  mapTest (\outcome -> outcome {errorMessage' = Just msg}) test

-- | Add a stack trace to a report for a test that threw an exception
--
-- @
--     import Data.Function ((&))
--
--     'writeXmlReport' "report.xml"
--       [ errored "A test that threw an error"
--           & errorStackTrace ["at closeCurtain line 3", "at goToSleep line 8"]
--           & 'inSuite' "Test suite"
--       ]
-- @
errorStackTrace :: [T.Text] -> TestReport Errored -> TestReport Errored
errorStackTrace trace test =
  mapTest (\outcome -> outcome {errorStackTrace' = trace}) test

data Counts
  = Counts
      { cumTests :: Int,
        cumFailed :: Int,
        cumErrored :: Int,
        cumSkipped :: Int,
        cumTime :: Double
      }

instance Semigroup Counts where
  c1 <> c2 =
    Counts
      { cumTests = cumTests c1 + cumTests c2,
        cumFailed = cumFailed c1 + cumFailed c2,
        cumErrored = cumErrored c1 + cumErrored c2,
        cumSkipped = cumSkipped c1 + cumSkipped c2,
        cumTime = cumTime c1 + cumTime c2
      }

instance Monoid Counts where
  mempty = Counts 0 0 0 0 0

encode :: [TestSuite] -> XML.Document
encode suites =
  XML.Document prologue element []
  where
    prologue = XML.Prologue [] Nothing []
    (totalCounts, suiteElements) =
      foldMap
        (fmap (pure . XML.NodeElement) . encodeSuite)
        (NonEmpty.groupAllWith suiteName suites)
    element =
      XML.Element
        "testsuites"
        (fromList (countAttributes totalCounts))
        suiteElements

encodeSuite :: NonEmpty.NonEmpty TestSuite -> (Counts, XML.Element)
encodeSuite suite =
  (suiteCounts, element)
  where
    suiteCounts = foldMap counts suite
    element =
      XML.Element
        "testsuite"
        (fromList $ ("name", suiteName (NonEmpty.head suite)) : countAttributes suiteCounts)
        (NonEmpty.toList (XML.NodeElement . testReport <$> suite))

encodeTestCase :: TestReport a -> XML.Element
encodeTestCase TestReport {testName', outcome', stdout', stderr', time'} =
  XML.Element "testcase" attributes children
  where
    attributes =
      fromList $
        catMaybes
          [ Just $ ("name", testName'),
            (,) "time" . T.pack . show <$> time'
          ]
    children =
      XML.NodeElement
        <$> catMaybes
          [ outcomeToXML outcome',
            XML.Element "system-out" mempty . pure . XML.NodeContent <$> stdout',
            XML.Element "system-err" mempty . pure . XML.NodeContent <$> stderr'
          ]

encodeFailure :: Failed -> XML.Element
encodeFailure failure =
  XML.Element
    "failure"
    (maybe mempty (\v -> fromList [("message", v)]) (failureMessage' failure))
    [XML.NodeContent (T.unlines (failureStackTrace' failure))]

encodeError :: Errored -> XML.Element
encodeError err =
  XML.Element
    "error"
    (maybe mempty (\v -> fromList [("message", v)]) (errorMessage' err))
    [XML.NodeContent (T.unlines (errorStackTrace' err))]

countAttributes :: Counts -> [(XML.Name, T.Text)]
countAttributes counts =
  [ ("tests", T.pack (show (cumTests counts))),
    ("failures", T.pack (show (cumFailed counts))),
    ("errors", T.pack (show (cumErrored counts))),
    ("skipped", T.pack (show (cumSkipped counts))),
    ("time", T.pack (show (cumTime counts)))
  ]
