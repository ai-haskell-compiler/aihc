module Main (main) where

import Data.Proxy (Proxy (..))
import FcGolden (updateFcGoldens)
import System.Environment (lookupEnv)
import Test.Fc.Properties (fcPropertyTests)
import Test.Fc.Suite (fcDesugarTests, fcEvalFixtureTests, fcEvalTests, fcGoldenTests, fcLoweringTests, fcMainTests, fcMergeTests, fcOptimizationTests)
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)

-- The workspace-wide check passes this option to every suite. FC properties
-- use Hedgehog, but accepting the legacy flag keeps the shared command usable
-- without retaining a QuickCheck dependency.
newtype LegacyQuickCheckTests = LegacyQuickCheckTests Int

instance IsOption LegacyQuickCheckTests where
  defaultValue = LegacyQuickCheckTests 100
  parseValue = fmap LegacyQuickCheckTests . safeRead
  optionName = pure "quickcheck-tests"
  optionHelp = pure "Compatibility option; System FC properties use Hedgehog"

main :: IO ()
main = do
  update <- lookupEnv "AIHC_UPDATE_FC_GOLDENS"
  case update of
    Just "1" -> updateFcGoldens
    _ -> do
      golden <- fcGoldenTests
      evalFixtures <- fcEvalFixtureTests
      defaultMainWithIngredients
        (includingOptions [Option (Proxy :: Proxy LegacyQuickCheckTests)] : defaultIngredients)
        ( testGroup
            "aihc-fc"
            [ golden,
              fcDesugarTests,
              fcEvalTests,
              fcLoweringTests,
              fcMainTests,
              fcMergeTests,
              fcOptimizationTests,
              evalFixtures,
              fcPropertyTests
            ]
        )
