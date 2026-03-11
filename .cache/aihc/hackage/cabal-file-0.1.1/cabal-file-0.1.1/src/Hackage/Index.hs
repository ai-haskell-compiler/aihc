{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}

module Hackage.Index (
  getCabal,
  getCabals,
  withCabalFile,
  listPackages,
  packageVersions,
  latestVersion,
  preferredVersions,
  getTimestamp,
  indexFiles,
  getPackageDescription,
  getPackageDescription',
  packageIdOrLatest,
  getFileInfo,
  -- * Re-exports from hackage-security
  FileInfo(..),
  FileLength(..),
  fileInfoSHA256
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Version.Extra (readVersion)
import System.Directory
import System.FilePath
import System.IO.Extra (withTempDir)

import Distribution.Version
#if MIN_VERSION_Cabal(3,0,0)
#else
  hiding (showVersion)
#endif
import Hackage.Security.Client
import qualified Hackage.Security.Client.Repository.Local as Local
import qualified Hackage.Security.Util.Path as Path
--import Hackage.Security.Util.Some
import qualified Hackage.Security.Client.Repository.Cache as Cache
import Hackage.Security.Util.Pretty

import SimpleCabal

-- | Get the contents of the .cabal file for package version
getCabal  :: PackageIdentifier -> IO BL.ByteString
getCabal pkgid =
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} ->
    trusted <$> indexLookupCabal pkgid

-- | Pass a temporary copy of .cabal file to some action
withCabalFile :: PackageIdentifier -> (FilePath -> IO a) -> IO a
withCabalFile pkgid act =
  withTempDir $ \ tmpdir -> do
    bs <- getCabal pkgid
    let filepath = tmpdir </> showPkgId pkgid <.> "cabal"
    BL.writeFile filepath bs
    act filepath

-- | Get two .cabal files at once!
getCabals  :: PackageIdentifier -> PackageIdentifier
           -> IO (BL.ByteString, BL.ByteString)
getCabals pkgid1 pkgid2 =
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} -> do
    bs1 <- trusted <$> indexLookupCabal pkgid1
    bs2 <- trusted <$> indexLookupCabal pkgid2
    return (bs1,bs2)

-- | Get FileInfo metadata for package version source
getFileInfo :: PackageIdentifier -> IO FileInfo
getFileInfo pkgid =
  withLocalRepo $ \rep -> uncheckClientErrors $
      withIndex rep $ \ IndexCallbacks{..} ->
        trusted <$> indexLookupFileInfo pkgid

-- | Get and try to parse the PackageDescription of a package version
getPackageDescription :: PackageIdentifier -> IO (Maybe PackageDescription)
getPackageDescription pkgid =
#if (defined(MIN_VERSION_simple_cabal) && MIN_VERSION_simple_cabal(0,1,2))
  do
  cabal <- getCabal pkgid
  parseFinalPackageDescription [] $ BL.toStrict cabal
#else
  Just <$> withCabalFile pkgid (finalPackageDescription [])
#endif

-- | Get and parse PackageDescription of package version
--
-- Raises an error on failure.
getPackageDescription' :: PackageIdentifier -> IO PackageDescription
getPackageDescription' pkgid = do
  mfpd <- getPackageDescription pkgid
  maybe (error "Failed to parse cabal file") return mfpd

-- | Easy access to the local Hackage repo
withLocalRepo :: (Repository Local.LocalFile -> IO a) -> IO a
withLocalRepo action = do
  home <- getHomeDirectory
  localrepo <- (Path.makeAbsolute . Path.fromFilePath) (home </> ".cabal")
  localcache <- (Path.makeAbsolute . Path.fromFilePath) (home </> ".cabal/packages/hackage.haskell.org")
  Local.withRepository localrepo (cache localcache) hackageRepoLayout hackageIndexLayout logTUF action
  where
    -- FIXME could also support 00-index
    cache localcache = Cache.Cache {
        Cache.cacheRoot   = localcache
      , Cache.cacheLayout = cabalCacheLayout
        { cacheLayoutIndexTar   = Path.rootPath $ Path.fragment "01-index.tar"
        , cacheLayoutIndexIdx   = Path.rootPath $ Path.fragment "01-index.tar.idx"
        , cacheLayoutIndexTarGz = Path.rootPath $ Path.fragment "01-index.tar.gz"}
    }

    logTUF msg = putStrLn $ "# " ++ pretty msg

-- | Get all versions of a package in the index
packageVersions :: PackageName -> IO [Version]
packageVersions pkgname =
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    let pkg = unPackageName pkgname
    return $ L.sort . mapMaybe (extractPkgVersion pkg . second) $ directoryEntries dir
  where
    second (_,b,_) = b

    extractPkgVersion :: String -> IndexPath -> Maybe Version
    extractPkgVersion pkg path =
      if Path.takeExtension path == ".cabal" then
        let namever = (Path.toUnrootedFilePath . Path.unrootPath . Path.takeDirectory) path
        in if takeDirectory namever == pkg
           then Just $ mkVersion' . readVersion $ takeFileName namever
           else Nothing
      else Nothing

-- | Get the preferred-versions metadata for package
preferredVersions :: PackageName -> IO (Maybe BL.ByteString)
preferredVersions pkgname =
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} ->
    fmap indexEntryContent <$> indexLookupFile (IndexPkgPrefs pkgname)

-- | List all files in the Hackage index
-- (.cabal files, metadata .json files, preferred-versions files)
indexFiles :: IO [String]
indexFiles =
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    return $ map dirEntryPath (directoryEntries dir)
  where
    second (_,b,_) = b

    dirEntryPath = Path.toUnrootedFilePath . Path.unrootPath . second

-- | Get the latest version of package from the index
--
-- Note: does not take preferred-versions into account
latestVersion :: PackageName -> IO (Maybe Version)
latestVersion pkgname = do
  versions <- packageVersions pkgname
  if null versions then return Nothing
    else return $ Just $ last versions

-- | Get the index timestamp for (the latest revision of) pkgid
getTimestamp :: PackageIdentifier -> IO (Maybe UTCTime)
getTimestamp pkgid =
  withLocalRepo $ \rep -> uncheckClientErrors $
    withIndex rep $ \ IndexCallbacks{..} ->
    fmap (posixSecondsToUTCTime . realToFrac . indexEntryTime) <$>
    indexLookupFile (IndexPkgCabal pkgid)

-- | Convert a PackageID if unversioned to latest package version
packageIdOrLatest :: PackageIdentifier -> IO PackageIdentifier
packageIdOrLatest pkgid = do
  let name = pkgName pkgid
  if pkgVersion pkgid == nullVersion then do
    mlatest <- latestVersion name
    return $ maybe pkgid (PackageIdentifier name) mlatest
    else return pkgid

-- | List all packages in the index (unsorted for performance)
listPackages :: IO [String]
listPackages =
  withLocalRepo $ \rep -> uncheckClientErrors $ do
    dir <- getDirectory rep
    return $ L.nub $ mapMaybe (extractPkg . second) (directoryEntries dir)
  where
    extractPkg path =
      if Path.takeExtension path == ".cabal" then
        (Just . takeWhile (/= '/') . Path.toUnrootedFilePath . Path.unrootPath) path
        else Nothing

    second (_,b,_) = b
