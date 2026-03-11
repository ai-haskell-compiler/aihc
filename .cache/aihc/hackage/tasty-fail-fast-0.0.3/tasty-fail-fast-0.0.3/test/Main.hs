module Main
    ( main
    ) where


-------------------------------------------------------------------------------
import           Control.Exception
import           Control.Monad
import           Data.Monoid
import           System.Directory
import           System.Exit
import           System.IO
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients
import           Test.Tasty.Options
import           Test.Tasty.Runners.TAP
-------------------------------------------------------------------------------
import           Test.Tasty.Ingredients.FailFast
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------



main :: IO ()
main = do
  tmpDir <- getTemporaryDirectory
  defaultMain (tests tmpDir)


-------------------------------------------------------------------------------
tests :: FilePath -> TestTree
tests tmpDir  = testGroup "tasty-fail-fast"
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
  let Just runner = tryIngredients [failFast (tapRunner' h)] opts exampleTests
  void runner  `catch` interceptExit
  where interceptExit :: ExitCode -> IO ()
        interceptExit _ = return ()
        opts = setOption (FailFast True) mempty


-------------------------------------------------------------------------------
exampleTests :: TestTree
exampleTests = testGroup "some example tests"
  [
    testCase "trivial pass" (True @?= True)
  , testCase "first failure" (True @?= False)
  , testCase "another pass" (True @?= True)
  , testCase "second failure" (True @?= False)
  , testCase "yet another pass" (True @?= True)
  ]


-------------------------------------------------------------------------------
goldenPath :: FilePath -> FilePath
goldenPath fp = "test/golden/" ++ fp
