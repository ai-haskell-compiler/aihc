module Test.Native.BlockLayout
  ( tests,
  )
where

import Aihc.Native.BlockLayout
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "native block layout"
    [ testCase "follows preferred successors before source order" $ do
        let blocks =
              [ Block "entry" ["entry instruction"] (Jump "hot"),
                Block "cold" ["cold instruction"] Exit,
                Block "hot" ["hot instruction"] Exit
              ]
        assertEqual "trace order" ["entry", "hot", "cold"] (map blockLabel (layoutBlocks "entry" blocks)),
      testCase "omits jumps to the immediately following block" $ do
        let blocks =
              [ Block "entry" ["entry instruction"] (Jump "hot"),
                Block "cold" ["cold instruction"] Exit,
                Block "hot" ["hot instruction"] Exit
              ]
        assertEqual
          "rendered trace"
          [ "entry:",
            "entry instruction",
            "hot:",
            "hot instruction",
            "cold:",
            "cold instruction"
          ]
          (renderBlocks (<> ":") ("jump " <>) (layoutBlocks "entry" blocks)),
      testCase "retains jumps whose target is not next" $ do
        let blocks =
              [ Block "entry" ["entry instruction"] (Jump "exit"),
                Block "middle" ["middle instruction"] Exit,
                Block "exit" ["exit instruction"] Exit
              ]
        assertEqual
          "explicit non-fallthrough edge"
          [ "entry:",
            "entry instruction",
            "jump exit",
            "middle:",
            "middle instruction",
            "exit:",
            "exit instruction"
          ]
          (renderBlocks (<> ":") ("jump " <>) blocks)
    ]
