module Main (main) where

import Hedgehog (Property, property, success)
import Test.ExtractHiCompare (extractHiCompareTests)
import Test.Fuzz (fuzzTests)
import Test.ResolvePackage (resolvePackageTests)
import Test.ResolveStackageProgress.PathsModule (resolveStackagePathsModuleTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Test.TcStackageProgress (tcStackageProgressTests)

main :: IO ()
main =
  defaultMain . testGroup "aihc-dev" $
    [ testProperty "Hedgehog options" prop_dummy,
      extractHiCompareTests,
      fuzzTests,
      resolvePackageTests,
      resolveStackagePathsModuleTests,
      tcStackageProgressTests
    ]

prop_dummy :: Property
prop_dummy = property success
