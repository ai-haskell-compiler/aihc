module Main (main) where

import FcGolden (updateFcGoldens)
import System.Environment (lookupEnv)
import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcLintTests)
import Test.Tasty (defaultMain, testGroup)

-- import Test.Fc.Suite (fcEvalFixtureTests, fcGoldenTests)

main :: IO ()
main = do
  update <- lookupEnv "AIHC_UPDATE_FC_GOLDENS"
  case update of
    Just "1" -> updateFcGoldens
    _ -> defaultMain (testGroup "aihc-fc" [fcLintTests, fcPropertyTests])

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
