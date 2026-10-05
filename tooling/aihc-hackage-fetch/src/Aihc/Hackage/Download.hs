-- | Download Hackage packages into the local XDG cache.
module Aihc.Hackage.Download
  ( downloadPackageWithOptions,
    DownloadOptions (..),
    defaultDownloadOptions,
  )
where

import Aihc.Hackage.Cache (getHackageCacheDir)
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Http (httpGet)
import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Monad (when)
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
    downloadAllowNetwork :: !Bool
  }

-- | Default download options: verbose, network allowed.
defaultDownloadOptions :: DownloadOptions
defaultDownloadOptions =
  DownloadOptions
    { downloadVerbose = True,
      downloadAllowNetwork = True
    }

-- | Download a package with the given options.
downloadPackageWithOptions :: DownloadOptions -> PackageSpec -> IO FilePath
downloadPackageWithOptions opts pkg = do
  cacheDir <- getHackageCacheDir
  let pkgDir = cacheDir </> formatPackage pkg
      markerFile = pkgDir </> ".complete"
  markerExists <- doesFileExist markerFile
  if markerExists
    then pure pkgDir
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
          tarballBytes <- httpGet url
          case tarballBytes of
            Left err -> ioError (userError ("Failed to download " ++ formatPackage pkg ++ ": " ++ err))
            Right lbs -> do
              let entries = Tar.read (GZip.decompress lbs)
              pkgDirExists <- doesDirectoryExist pkgDir
              when pkgDirExists $ removeDirectoryRecursive pkgDir
              Tar.unpack cacheDir entries
              writeFile markerFile ""
              pure pkgDir
