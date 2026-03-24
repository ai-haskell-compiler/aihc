{-# LANGUAGE OverloadedStrings #-}

-- | Stackage snapshot fetching and parsing.
module StackageProgress.Snapshot
  ( loadStackageSnapshotWithMode,
    parseSnapshotConstraints,
    snapshotCacheFile,
  )
where

import Control.Exception (SomeException, displayException, try)
import Data.Char (isAlphaNum, isSpace)
import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import StackageProgress.Summary (PackageSpec (..))
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
  )
import System.FilePath ((</>))
import System.Process (readProcess)

-- | Load a Stackage snapshot, either from cache or by fetching it.
loadStackageSnapshotWithMode :: String -> Bool -> IO (Either String [PackageSpec])
loadStackageSnapshotWithMode snapshot offline = do
  cacheFile <- snapshotCacheFile snapshot
  hasCache <- doesFileExist cacheFile
  if hasCache
    then do
      cachedBody <- readFile cacheFile
      pure (parseSnapshotConstraints cachedBody)
    else
      if offline
        then pure (Left ("Snapshot missing from cache in offline mode: " ++ snapshot))
        else do
          let url = "https://www.stackage.org/" ++ snapshot ++ "/cabal.config"
          fetched <- try (readProcess "curl" ["-s", "-f", url] "")
          case fetched of
            Left err -> pure (Left (displayException (err :: SomeException)))
            Right body ->
              case parseSnapshotConstraints body of
                Left parseErr -> pure (Left parseErr)
                Right specs -> do
                  writeFile cacheFile body
                  pure (Right specs)

-- | Get the cache file path for a snapshot.
snapshotCacheFile :: String -> IO FilePath
snapshotCacheFile snapshot = do
  base <- getXdgDirectory XdgCache "aihc"
  let dir = base </> "stackage"
      file = sanitizeSnapshotName snapshot ++ "-cabal.config"
  createDirectoryIfMissing True dir
  pure (dir </> file)

sanitizeSnapshotName :: String -> String
sanitizeSnapshotName = map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c || c == '-' || c == '_' = c
      | otherwise = '_'

-- | Parse package constraints from a snapshot's cabal.config.
parseSnapshotConstraints :: String -> Either String [PackageSpec]
parseSnapshotConstraints content = do
  let section = constraintLines (lines content)
      entries = map trim (splitComma (concat section))
      specs = mapMaybe parseConstraint entries
  if null specs
    then Left "No package constraints found"
    else Right specs

constraintLines :: [String] -> [String]
constraintLines ls =
  case break (isPrefixOf "constraints:" . trimLeft) ls of
    (_, []) -> []
    (_, firstRaw : restRaw) ->
      let firstLine = trimLeft firstRaw
          start = [drop 12 firstLine]
          cont = [trimLeft line | line <- takeWhile isConstraintContinuation restRaw]
       in start <> cont

isConstraintContinuation :: String -> Bool
isConstraintContinuation line =
  case line of
    c : _ -> isSpace c
    [] -> False

trimLeft :: String -> String
trimLeft = dropWhile isSpace

parseConstraint :: String -> Maybe PackageSpec
parseConstraint entry
  | null entry = Nothing
  | "--" `isPrefixOf` trim entry = Nothing
  | otherwise =
      case breakOn "==" entry of
        Just (name, ver) -> Just (PackageSpec (trim name) (trim ver))
        Nothing ->
          let ws = words entry
           in case ws of
                -- Snapshot constraints like "base installed" refer to compiler-provided
                -- packages and do not map to downloadable Hackage tarballs.
                [_, "installed"] -> Nothing
                _ -> Nothing

breakOn :: String -> String -> Maybe (String, String)
breakOn needle haystack =
  case findNeedle needle haystack of
    Nothing -> Nothing
    Just i ->
      let (left, right) = splitAt i haystack
       in Just (left, drop (length needle) right)

findNeedle :: String -> String -> Maybe Int
findNeedle needle = go 0
  where
    go _ [] = Nothing
    go i xs
      | needle `isPrefixOf` xs = Just i
      | otherwise = go (i + 1) (drop 1 xs)

splitComma :: String -> [String]
splitComma s =
  case break (== ',') s of
    (chunk, []) -> [chunk]
    (chunk, _ : rest) -> chunk : splitComma rest

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
  where
    dropWhileEnd p = reverse . dropWhile p . reverse
