module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import           System.Directory
import           System.Exit
import           System.IO
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
-------------------------------------------------------------------------------
import           Test.Tasty.Runners.TAP
-------------------------------------------------------------------------------


main :: IO ()
main = do
  tmpDir <- getTemporaryDirectory
  defaultMain (tests tmpDir)


-------------------------------------------------------------------------------
tests :: FilePath -> TestTree
tests tmpDir = testGroup "Test.Tasty.Runners.TAP"
  [
    let tmpPath = tmpDir ++ "/simple.tap"
    in goldenVsFileDiff "simple.tap"
                        diffCmd
                        (goldenPath "simple.tap")
                        tmpPath
                        (mkSimple tmpPath)
  ]


-------------------------------------------------------------------------------
diffCmd :: FilePath -> FilePath -> [String]
diffCmd ref new = ["diff", "-u", ref, new]


-------------------------------------------------------------------------------
mkSimple :: FilePath -> IO ()
mkSimple tmpPath = bracket (openFile tmpPath WriteMode) hClose $ \h -> do
  defaultMainWithIngredients [tapRunner' h] exampleTests `catch` interceptExit
  where interceptExit :: ExitCode -> IO ()
        interceptExit _ = return ()


-------------------------------------------------------------------------------
exampleTests :: TestTree
exampleTests = testGroup "some example tests"
  [
    testCase "passes" $ assertBool "uh your computer is busted" True
  , testCase "fails" $ assertFailure "this was doomed"
  ]


-------------------------------------------------------------------------------
goldenPath :: FilePath -> FilePath
goldenPath fp = "test/golden/" ++ fp
