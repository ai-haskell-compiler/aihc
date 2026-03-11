module Data.Tsv2csv where
import Control.Monad (liftM, when)
import Data.List(intercalate)
import Data.List.Split(splitOn)
import System.IO (hPutStrLn, stderr)
import Test.HUnit

main = do
  ss <- getContents
  putStrLn $ toCsv . fromTsv $ ss

fromTsv :: String -> [[String]]
fromTsv = map (splitOn "\t") . lines

toCsv :: [[String]] -> String
toCsv = unlines . map (intercalate ",") . map (map csvEscape)

csvEscape :: String -> String
csvEscape s
  | ','  `elem` s = s'
  | '\"' `elem` s = s'
  | '\n' `elem` s = s'
  | otherwise     = s
  where s' = ('\"' : s) ++ "\""

------------------------------------------------------------------------
-- Self tests

-- cabal repl
--   runTests

runTests :: IO ()
runTests = do
    Counts c t e f <- runTestTT tests
    when (e > 0 || f > 0) $ hPutStrLn stderr $ "Failure"

alwaysFail = TestCase $ GT @?= EQ
alwaysPass = TestCase $ EQ @?= EQ

fromTsv_oneRow = TestCase $ got @?= expected
  where got = fromTsv tsv
        tsv = "foo\tbar\tzot"
        expected = [["foo", "bar", "zot"]]

fromTsv_twoRows = TestCase $ got @?= expected
  where got = fromTsv tsv
        tsv = "foo\tbar\tzot\nfoofoo\tbarbar\tzotzot"
        expected = [["foo", "bar", "zot"], ["foofoo", "barbar", "zotzot"]]

toCsv_oneRow = TestCase $ got @?= expected
  where got = toCsv xs
        xs = [["foo", "bar", "zot"]]
        expected = "foo,bar,zot\n"

toCsv_twoRows = TestCase $ got @?= expected
  where got = toCsv xs
        xs = [["foo", "bar", "zot"], ["foofoo", "barbar", "zotzot"]]
        expected = "foo,bar,zot\nfoofoo,barbar,zotzot\n"

toCsv_withComma = TestCase $ got @?= expected
  where got = toCsv xs
        xs = [["fo,o", "bar", "z,ot"]]
        expected = "\"fo,o\",bar,\"z,ot\"\n"

toCsv_withQuote = TestCase $ got @?= expected
  where got = toCsv xs
        xs = [["fo\"o", "b\"ar", "zot"]]
        expected = "\"fo\"o\",\"b\"ar\",zot\n"

toCsv_withNewline = TestCase $ got @?= expected
  where got = toCsv xs
        xs = [["fo\no", "b\nar", "zot"]]
        expected = "\"fo\no\",\"b\nar\",zot\n"

toCsv_withTab =  TestCase $ got @?= expected
  where got = toCsv xs
        xs = [["fo\\to", "bar", "zot"]]
        expected = "fo\\to,bar,zot\n"

tests :: Test
tests = TestList [
      TestLabel "Always pass"                  alwaysPass
--    , TestLabel "Always fail"                  alwaysFail
    , TestLabel "fromTsv_oneRow"               fromTsv_oneRow
    , TestLabel "fromTsv_twoRows"              fromTsv_twoRows
    , TestLabel "toCsv_oneRow"                 toCsv_oneRow
    , TestLabel "toCsv_twoRows"                toCsv_twoRows
    , TestLabel "toCsv_withComma"              toCsv_withComma
    , TestLabel "toCsv_withQuote"              toCsv_withQuote
    , TestLabel "toCsv_withNewline"            toCsv_withNewline
    , TestLabel "toCsv_withTab"                toCsv_withTab
  ]
