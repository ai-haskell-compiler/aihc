module Test.ParserBenchReport
  ( parserBenchReportTests,
  )
where

import Aihc.Dev.Parser.Bench.Report (ParserResult (..), renderParserRatioRow)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

parserBenchReportTests :: TestTree
parserBenchReportTests =
  testGroup
    "parser benchmark report"
    [ testCase "renders speed and memory ratios in their intended directions" $ do
        let ghc = ParserResult "GHC (`ghc-lib-parser`)" 200 400 600
            aihc = ParserResult "AIHC" 100 100 300
        assertEqual
          "report row"
          "| AIHC | `2.00x` | `0.25x` | `0.50x` |"
          (renderParserRatioRow ghc aihc)
    ]
