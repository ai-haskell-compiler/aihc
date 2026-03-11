module Main where

import Control.Concurrent
import Control.Concurrent.STM hiding (check)
import Control.Exception
import Control.Monad
import Data.FileCache.Internal
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory
import System.FilePath (addTrailingPathSeparator)
import System.IO.Temp
import Test.Hspec

computation :: String -> IO (Either String Int)
computation f = do
  c <- readFile f
  return $
    if length c `mod` 2 == 0
      then Right (length c)
      else Left "odd length"

main :: IO ()
main = withSystemTempDirectory "filecacheXXX.tmp" $ \tempdir -> do
  cache@(FileCache tcache tdirs _ _ _) <- newFileCache :: IO (FileCache Int)
  let indexes = [1, 10, 100, 1000]
      tofilename :: Int -> String
      tofilename i = tempdir ++ "/temp" ++ show i
  forM_ indexes $ \l -> writeFile (tofilename l) (show l)
  let q l = lazyQuery cache (tofilename l) (computation (tofilename l))
      qf l = lazyQuery cache (tofilename l) (throwIO (AssertionFailed "fail"))
  hspec $ describe "Spec" $ do
    it "Should run the actions" $ do
      mapM q indexes `shouldReturn` [Left "odd length", Right 2, Left "odd length", Right 4]
    it "Should have cached the results" $ do
      mapM qf indexes `shouldReturn` [Left "odd length", Right 2, Left "odd length", Right 4]
    it "Should stop watching dropped files" $ do
      removeFile (tofilename 10)
      doesFileExist (tofilename 10) `shouldReturn` False
      threadDelay (1 * 10 ^ (5 :: Int))
      cacheInfo <- readTVarIO tcache
      M.size cacheInfo `shouldBe` 3
    it "Should update the list of watched files per directory" $ do
      dirs <- fmap fst <$> readTVarIO tdirs
      M.toList dirs `shouldBe` [(addTrailingPathSeparator tempdir, foldMap (S.singleton . tofilename) [1, 100, 1000])]
    it "Should stop watching directory without watched files" $ do
      forM_ [1, 100, 1000] $ \i -> do
        removeFile (tofilename i)
        doesFileExist (tofilename i) `shouldReturn` False
      threadDelay (1 * 10 ^ (5 :: Int))
      dirs <- fmap fst <$> readTVarIO tdirs
      dirs `shouldBe` M.empty
    it "Should stop the watch" $ do
      killFileCache cache
      q 1 `shouldReturn` Left "Closed cache"
