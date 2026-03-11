{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import Control.Monad (guard)
import Data.Foldable (toList)
import Data.List (isSuffixOf)
import System.Directory (doesDirectoryExist, doesFileExist,
  getDirectoryContents)
import System.FilePath ((</>))
import Test.DocTest (doctest)

main :: IO ()
main = do
  sources <- findSources "src"
  doctest ("-isrc" : sources)

findSources :: FilePath -> IO [FilePath]
findSources = goDir
  where
    goItem :: FilePath -> FilePath -> IO [FilePath]
    goItem _ ('.':_) = pure []
    goItem parent name = do
      let path = parent </> name
      isDir  <- doesDirectoryExist path
      isFile <- doesFileExist path
      if | isDir     -> goDir path
         | isFile    -> pure $ toList $ goFile path
         | otherwise -> pure []

    goDir :: FilePath -> IO [FilePath]
    goDir path = do
      items <- getDirectoryContents path
      concat <$> traverse (goItem path) items

    goFile :: FilePath -> Maybe FilePath
    goFile path = path <$ guard (".hs" `isSuffixOf` path)
