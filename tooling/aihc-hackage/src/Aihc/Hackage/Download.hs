{-# LANGUAGE ScopedTypeVariables #-}

-- | Download Hackage packages into the local XDG cache.
module Aihc.Hackage.Download
  ( downloadPackage,
    downloadPackageQuiet,
    downloadPackageWithOptions,
    DownloadOptions (..),
    defaultDownloadOptions,
  )
where

import Aihc.Hackage.Cache (getHackageCacheDir)
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, displayException, try)
import Control.Monad (when)
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client (Manager, httpLbs, newManager, parseRequest, responseBody, responseStatus)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
  )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

-- | Options for downloading a package.
data DownloadOptions = DownloadOptions
  { downloadVerbose :: !Bool,
    downloadAllowNetwork :: !Bool,
    downloadManager :: !(Maybe Manager)
  }

-- | Default download options: verbose, network allowed, no shared manager.
defaultDownloadOptions :: DownloadOptions
defaultDownloadOptions =
  DownloadOptions
    { downloadVerbose = True,
      downloadAllowNetwork = True,
      downloadManager = Nothing
    }

-- | Download a package with default (verbose) logging.
downloadPackage :: PackageSpec -> IO FilePath
downloadPackage = downloadPackageWithOptions defaultDownloadOptions

-- | Download a package without logging.
downloadPackageQuiet :: PackageSpec -> IO FilePath
downloadPackageQuiet = downloadPackageWithOptions defaultDownloadOptions {downloadVerbose = False}

-- | Download a package with the given options.
downloadPackageWithOptions :: DownloadOptions -> PackageSpec -> IO FilePath
downloadPackageWithOptions opts pkg = do
  cacheDir <- getHackageCacheDir
  let pkgDir = cacheDir </> formatPackage pkg
      markerFile = pkgDir </> ".complete"
  markerExists <- doesFileExist markerFile
  if markerExists
    then do
      when (downloadVerbose opts) $
        hPutStrLn stderr ("Cache hit: " ++ formatPackage pkg)
      pure pkgDir
    else
      if not (downloadAllowNetwork opts)
        then ioError (userError ("Package missing from cache in offline mode: " ++ formatPackage pkg))
        else do
          createDirectoryIfMissing True cacheDir
          when (downloadVerbose opts) $
            hPutStrLn stderr ("Downloading " ++ formatPackage pkg ++ " from Hackage...")
          let url =
                "https://hackage.haskell.org/package/"
                  ++ formatPackage pkg
                  ++ "/"
                  ++ formatPackage pkg
                  ++ ".tar.gz"
          manager <- case downloadManager opts of
            Just m -> pure m
            Nothing -> newManager tlsManagerSettings
          tarballBytes <- httpGetLBS manager url
          case tarballBytes of
            Left err -> ioError (userError ("Failed to download " ++ formatPackage pkg ++ ": " ++ err))
            Right lbs -> do
              let entries = Tar.read (GZip.decompress lbs)
              pkgDirExists <- doesDirectoryExist pkgDir
              when pkgDirExists $ removeDirectoryRecursive pkgDir
              Tar.unpack cacheDir entries
              writeFile markerFile ""
              pure pkgDir

-- | Perform an HTTP GET request and return the response body as lazy ByteString.
httpGetLBS :: Manager -> String -> IO (Either String LBS.ByteString)
httpGetLBS manager url = do
  result <- try $ do
    request <- parseRequest url
    response <- httpLbs request manager
    let status = statusCode (responseStatus response)
    if status >= 200 && status < 300
      then pure (Right (responseBody response))
      else pure (Left ("HTTP " ++ show status ++ " for " ++ url))
  case result of
    Left (err :: SomeException) -> pure (Left (displayException err))
    Right r -> pure r
