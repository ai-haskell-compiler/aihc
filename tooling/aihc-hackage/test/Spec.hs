module Main (main) where

import Aihc.Hackage.Stackage (parseSnapshotConstraints)
import Aihc.Hackage.Types (PackageSpec (..))
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main =
  defaultMain . testGroup "aihc-hackage" $
    [ testCase "maps selected installed packages to fixed versions" $ do
        parseSnapshotConstraints "constraints: binary installed, bytestring installed, Cabal installed, unix installed"
          @?= Right
            [ PackageSpec "binary" "0.8.9.3",
              PackageSpec "bytestring" "0.12.2.0",
              PackageSpec "Cabal" "3.14.2.0",
              PackageSpec "unix" "2.8.8.0"
            ],
      testCase "keeps installed for packages without a fixed override" $ do
        parseSnapshotConstraints "constraints: base installed, custom-package installed"
          @?= Right
            [ PackageSpec "base" "installed",
              PackageSpec "custom-package" "installed"
            ],
      QC.testProperty "dummy quickcheck property" prop_dummy
    ]

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True

(@?=) :: (Eq a, Show a) => Either String a -> Either String a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)
