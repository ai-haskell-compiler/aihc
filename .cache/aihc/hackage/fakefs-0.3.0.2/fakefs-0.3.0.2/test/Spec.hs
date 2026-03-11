import           Control.Exception        (fromException)
import           Control.Monad.Catch.Pure (runCatchT)
import           Data.Bifunctor           (first)
import qualified Data.Map.Strict          as M
import           Data.Traversable         (for)
import           System.IO.Error          (isDoesNotExistError)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Test.FileSystem.Fake


main :: IO ()
main = hspec $
  describe "Test.FileSystem.Fake" $ do
    describe "readPath and writePath" $
      prop "written files are read as written" $ \(NonEmpty pathAndContents) -> do
        let action =
              for pathAndContents $ \(path, contents) -> do
                writePath path (contents :: String)
                readContents <- readPath path
                return (path, readContents)
            result = runFileSystemM (first show <$> runCatchT action) mempty
        result `shouldBe` (Right pathAndContents, M.fromList pathAndContents)

    describe "readPath" $
      prop "always throws a DoesNotExist error given empty file system" $ \path -> do
        let action = readPath path
            (Left err, afterRun) = runFileSystemM (runCatchT action) mempty
            (Just ioe) = fromException err
        afterRun `shouldBe` (mempty :: M.Map FilePath String)
        ioe `shouldSatisfy` isDoesNotExistError
