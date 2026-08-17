module Main (main) where

import FcGolden (updateFcGoldens)
import System.Environment (lookupEnv)
import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcGoldenTests, fcLintTests)
import Test.Fc2.Properties (fc2PropertyTests)
import Test.Fc2.Suite (fc2FixtureTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  update <- lookupEnv "AIHC_UPDATE_FC_GOLDENS"
  case update of
    Just "1" -> updateFcGoldens
    _ -> do
      golden <- fcGoldenTests
      fc2 <- fc2FixtureTests
      defaultMain (testGroup "aihc-fc" [fcLintTests, golden, fcPropertyTests, fc2, fc2PropertyTests])
