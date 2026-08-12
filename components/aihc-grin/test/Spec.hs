module Main (main) where

import Data.Proxy (Proxy (..))
import Test.Grin.Properties (grinPropertyTests)
import Test.Grin.Suite (grinEvalFixtureTests, grinGoldenTests, grinUnitTests)
import Test.Tasty (defaultIngredients, defaultMainWithIngredients, includingOptions, testGroup)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), safeRead)

-- The shared check passes this option to each suite. Accept it while GRIN properties use Hedgehog.
newtype LegacyQuickCheckTests = LegacyQuickCheckTests Int

instance IsOption LegacyQuickCheckTests where
  defaultValue = LegacyQuickCheckTests 100
  parseValue = fmap LegacyQuickCheckTests . safeRead
  optionName = pure "quickcheck-tests"
  optionHelp = pure "Compatibility option; GRIN properties use Hedgehog"

main :: IO ()
main = do
  goldenFixtures <- grinGoldenTests
  evalFixtures <- grinEvalFixtureTests
  defaultMainWithIngredients
    (includingOptions [Option (Proxy :: Proxy LegacyQuickCheckTests)] : defaultIngredients)
    ( testGroup
        "aihc-grin"
        [ grinUnitTests,
          goldenFixtures,
          evalFixtures,
          grinPropertyTests
        ]
    )
