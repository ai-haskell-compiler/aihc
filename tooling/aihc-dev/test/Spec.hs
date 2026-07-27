module Main (main) where

import Test.ExtractHiCompare (extractHiCompareTests)
import Test.Fuzz (fuzzTests)
import Test.ResolvePackage (resolvePackageTests)
import Test.ResolveStackageProgress.PathsModule (resolveStackagePathsModuleTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck qualified as QC
import Test.TcStackageProgress (tcStackageProgressTests)

main :: IO ()
main =
  defaultMain . testGroup "aihc-dev" $
    [ QC.testProperty "dummy quickcheck property" prop_dummy,
      extractHiCompareTests,
      fuzzTests,
      resolvePackageTests,
      resolveStackagePathsModuleTests,
      tcStackageProgressTests
    ]

prop_dummy :: Bool
prop_dummy = True
