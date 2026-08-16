module Main (main) where

import FcGolden (updateFcGoldens)
import System.Environment (lookupEnv)
import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcGoldenTests, fcLintTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = do
  update <- lookupEnv "AIHC_UPDATE_FC_GOLDENS"
  case update of
    Just "1" -> updateFcGoldens
    _ -> do
      golden <- fcGoldenTests
      defaultMain (testGroup "aihc-fc" [fcLintTests, golden, fcPropertyTests])
