module Main (main) where

import FcGolden (updateFcGoldens)
import System.Environment (lookupEnv)

-- import Test.Fc.Properties (fcPropertyTests)
-- import Test.Fc.Suite (fcEvalFixtureTests, fcGoldenTests)
-- import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  update <- lookupEnv "AIHC_UPDATE_FC_GOLDENS"
  case update of
    Just "1" -> updateFcGoldens
    _ -> return ()

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
