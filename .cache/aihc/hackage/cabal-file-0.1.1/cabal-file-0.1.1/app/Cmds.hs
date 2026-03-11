module Cmds (
  diffCmd,
  saveCabal,
  showCabal,
  listPkg,
  latestPkg,
  listFiles,
  preferCmd,
  showMetadata,
  dateCabal
  ) where

import Control.Monad
import Control.Monad.Extra
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.List as L
import Distribution.Version (Version)
import SimpleCabal
import SimpleCmd
import System.Directory
import System.FilePath
import System.IO.Extra (withTempDir)

import Hackage.Index

-- FIXME structural diff of PackageDescription
-- FIXME revisions?
diffCmd :: Bool -> String -> Version -> Version -> IO ()
diffCmd whitespace pkg v1 v2 =
  withTempDir $ \ tmpdir -> do
    setCurrentDirectory tmpdir
    let pkgid1 = PackageIdentifier (mkPackageName pkg) v1
        pkgid2 = PackageIdentifier (mkPackageName pkg) v2
    saveCabals pkgid1 pkgid2
    void $ cmdBool "diff" $ ["-w" | not whitespace] ++ ["-u", showPkgId pkgid1 <.> "cabal", showPkgId pkgid2 <.> "cabal"]

--  pkgdesc <- finalPackageDescription [] cabal
  where
    saveCabals :: PackageIdentifier -> PackageIdentifier -> IO ()
    saveCabals pkgid1 pkgid2 = do
      (bs1,bs2) <- getCabals pkgid1 pkgid2
      BL.writeFile (showPkgId pkgid1 <.> "cabal") bs1
      BL.writeFile (showPkgId pkgid2 <.> "cabal") bs2

listPkg :: Maybe PackageName -> IO ()
listPkg (Just pkgname) = do
  versions <- packageVersions pkgname
  mapM_ (putStrLn . showVersion) versions
listPkg Nothing = listPackages >>= mapM_ putStrLn . L.sort

saveCabal :: PackageIdentifier -> IO ()
saveCabal pkgid = do
  pkgver <- packageIdOrLatest pkgid
  getCabal pkgver >>= BL.writeFile (showPkgId pkgid <.> "cabal")

showCabal :: PackageIdentifier -> IO ()
showCabal pkgid = do
  pkgver <- packageIdOrLatest pkgid
  getCabal pkgver >>= BL.putStrLn

showMetadata :: PackageIdentifier -> IO ()
showMetadata pkgid = do
  pkgver <- packageIdOrLatest pkgid
  finfo <- getFileInfo pkgver
  let bytes = show . fileLength . fileInfoLength
  putStrLn $ "Size: " ++ bytes finfo
  whenJust (fileInfoSHA256 finfo) $ \ hash ->
    putStrLn $ "SHA256 " ++ show hash

latestPkg :: PackageName -> IO ()
latestPkg pkgname =
  whenJustM (latestVersion pkgname) (putStrLn . showVersion)

listFiles :: IO ()
listFiles = do
  files <- indexFiles
  mapM_ putStrLn files

preferCmd :: PackageName -> IO ()
preferCmd pkgname =
  whenJustM (preferredVersions pkgname) BL.putStrLn

dateCabal :: PackageIdentifier -> IO ()
dateCabal pkgid = do
  pkgver <- packageIdOrLatest pkgid
  whenJustM (getTimestamp pkgver) print
