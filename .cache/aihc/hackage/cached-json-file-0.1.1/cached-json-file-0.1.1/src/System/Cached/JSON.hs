{-# LANGUAGE OverloadedStrings #-}

module System.Cached.JSON (
  getCachedJSON,
  getCachedJSONQuery,
  lookupKey
  )
where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime)
import Network.HTTP.Query
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath

-- | If the local cached json file is new enough then use it,
-- otherwise refresh from the remote url.
getCachedJSON :: (FromJSON a, ToJSON a)
              => String -- ^ subdirectory/program name
              -> FilePath -- ^ filename
              -> String -- ^ json url
              -> NominalDiffTime -- ^ cache duration (minutes)
              -> IO a
getCachedJSON prog jsonfile url =
  getCachedJSONQuery prog jsonfile (webAPIQuery url [])

-- | Similar to getCachedJSON but takes an IO procedure that fetches
-- the remote json data.
getCachedJSONQuery :: (FromJSON a, ToJSON a)
                   => String -- ^ program name
                   -> FilePath -- ^ filename
                   -> IO a -- ^ http query
                   -> NominalDiffTime -- ^ cache duration (minutes)
                   -> IO a
getCachedJSONQuery prog jsonfile webquery minutes = do
  file <- getUserCacheFile prog jsonfile
  exists <- doesFileExist file
  unless exists $ do
    putStrLn $ "Creating " ++ file ++ " ..."
    createDirectoryIfMissing True (takeDirectory file)
  recent <- do
    if exists
      then do
      size <- getFileSize file
      if size == 0
        then return False
        else do
        ts <- getModificationTime file
        t <- getCurrentTime
        return $ diffUTCTime t ts < (minutes * 60)
      else return False
  if recent
    then do
    eObj <- eitherDecode <$> B.readFile file
    case eObj of
      Left err -> error err
      Right obj -> return obj
    else do
    obj <- webquery
    B.writeFile file $ encode obj
    return obj
