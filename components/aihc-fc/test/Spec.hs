module Main (main) where

import FcGolden (updateFcGoldens)
import System.Environment (lookupEnv)
import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcClassConstraintLoweringTest, fcEmptyDataKindTest, fcLintTests, fcPrimitiveLiteralOriginTest)
import Test.Tasty (defaultMain, testGroup)

-- import Test.Fc.Suite (fcEvalFixtureTests, fcGoldenTests)

main :: IO ()
main = do
  update <- lookupEnv "AIHC_UPDATE_FC_GOLDENS"
  case update of
    Just "1" -> updateFcGoldens
    _ -> do
      emptyDataKind <- fcEmptyDataKindTest
      primitiveLiteralOrigin <- fcPrimitiveLiteralOriginTest
      classConstraintLowering <- fcClassConstraintLoweringTest
      defaultMain (testGroup "aihc-fc" [fcLintTests, emptyDataKind, primitiveLiteralOrigin, classConstraintLowering, fcPropertyTests])

-- _ -> do
--   golden <- fcGoldenTests
--   evalFixtures <- fcEvalFixtureTests
--   defaultMain
--     ( testGroup
--         "aihc-fc"
--         [ golden,
--           evalFixtures,
--           fcPropertyTests
--         ]
--     )
