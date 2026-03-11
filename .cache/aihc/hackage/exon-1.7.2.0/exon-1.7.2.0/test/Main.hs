module Main where

import Exon.Test.BasicTest (test_basic, test_customWhitespace)
import Exon.Test.BuildTest (test_build)
import Exon.Test.NewtypeTest (test_newtype)
import Exon.Test.ParseTest (test_parser)
import Exon.Test.ShowsPrecTest (test_showsPrec)
import Exon.Test.SkipWsTest (test_skipWs)
import Hedgehog (TestT, property, test, withTests)
import Test.Tasty (TestName, TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "build" test_build,
    unitTest "basic" test_basic,
    unitTest "custom whitespace handling" test_customWhitespace,
    unitTest "concat showsPrec fragments" test_showsPrec,
    unitTest "skip whitespace quoter" test_skipWs,
    unitTest "segment newtype conversion" test_newtype,
    unitTest "parser" test_parser
  ]

main :: IO ()
main =
  defaultMain tests
