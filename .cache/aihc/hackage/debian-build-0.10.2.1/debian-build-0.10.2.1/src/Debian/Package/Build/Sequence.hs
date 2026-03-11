-- |
-- Module      : Debian.Package.Build.Sequence
-- Copyright   : 2014-2016 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- This module provides build-sequence actions.
module Debian.Package.Build.Sequence
       ( origArchive, nativeArchive, sourceDir

       , withCurrentDir, withBaseCurrentDir

       , getBuildDir, removeBuildDir
       , findDebianChanges

       , copyDebianDir

       , rsyncGenOrigSources, rsyncGenNativeSources, rsyncGenSources

       , cabalGenOrigSources, cabalGenSources, cabalAutogenSources

       , genSources

       , findGeneratedSourceDir, findGeneratedSource,
       ) where

import System.FilePath ((</>), takeFileName, takeDirectory, takeBaseName)
import System.Directory
  (getDirectoryContents, doesDirectoryExist, doesFileExist)
import Control.Applicative (pure, (<$>), (<*>), (<|>))
import Control.Monad (when, guard, msum)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

import Debian.Package.Data
  (Hackage, hackageLongName, hackageArchive,
   Source, origArchiveName, nativeArchiveName, sourceDirName, isNative,
   PackageType, takeChangesType,
   HaskellPackage, hackage, package, haskellPackageFromPackage)
import Debian.Package.Build.Monad
  (Build, liftTrace, bracketBuild_, Config (..), askConfig, askBaseDir, askBuildDir)
import Debian.Package.Build.Command
  (chdir, pwd, createDirectoryIfMissing, confirmPath, renameFile, renameDirectory,
   unpack, packInDir', cabalDebian, dpkgParseChangeLog, rawSystem')
import qualified Debian.Package.Build.Cabal as Cabal


-- | Run 'Bulid' action under specified directory.
withCurrentDir :: FilePath -> Build a -> Build a
withCurrentDir dir act = do
  saveDir <- liftIO pwd
  bracketBuild_
    (liftTrace $ chdir dir)
    (liftTrace $ chdir saveDir)
    act

-- | Run 'Build' action under base-directory.
withBaseCurrentDir :: Build a -> Build a
withBaseCurrentDir act = do
  baseDir <- askBaseDir
  withCurrentDir baseDir act

-- | Take build-directory from 'Build' action context.
getBuildDir :: Build FilePath
getBuildDir =  askBuildDir

-- Pass build-directory to 'Build' action.
withBuildDir :: (FilePath -> Build a) -> Build a
withBuildDir f = getBuildDir >>= f

-- | Remove build-directory.
removeBuildDir :: Build ()
removeBuildDir = do
  bldDir <- getBuildDir
  liftTrace $ do
    found <- liftIO $ doesDirectoryExist bldDir
    when found $ rawSystem' "rm" ["-r", bldDir]

-- | Take debian-directory name from 'Build' action context.
debianDirName' :: Build FilePath
debianDirName' =  debianDirName <$> askConfig

-- | Take original source archive name from 'Build' action context.
origArchive :: Source -> Build FilePath
origArchive pkg =
  withBuildDir $ \w -> return $ w </> origArchiveName pkg

-- | Take debian native source archive name from 'Build' action context.
nativeArchive :: Source -> Build FilePath
nativeArchive pkg =
  withBuildDir $ \w -> return $ w </> nativeArchiveName pkg

-- | Take source directory from 'Build' action context.
sourceDir :: Source -> Build FilePath
sourceDir pkg =
  withBuildDir $ \w -> return $ w </> sourceDirName pkg

-- | Action to copy debian directory from base-directory into specified directory.
copyDebianDir :: FilePath -> Build ()
copyDebianDir srcDir = do
  debDN       <- debianDirName'
  baseDir     <- askBaseDir
  liftTrace $ rawSystem' "cp" ["-a", baseDir </> debDN, srcDir </> "."]


-- Setup source directory under build-directory using rsync.
rsyncGenOrigSourceDir :: Source -> Build FilePath
rsyncGenOrigSourceDir pkg = do
  srcDir   <- sourceDir pkg
  debDN    <- debianDirName'
  baseDir  <- askBaseDir
  bldDir   <- getBuildDir
  confEXs  <- sourceExcludes <$> askConfig
  let excludes = [takeFileName d
                 | d <- [bldDir]
                 , baseDir `isPrefixOf` d ]
                 ++ [debDN] ++ confEXs
  liftTrace $ do
    createDirectoryIfMissing srcDir
    rawSystem' "rsync"
      $  ["-auv"]
      ++ ["--exclude=" ++ e | e <- excludes]
      ++ [baseDir </> ".", srcDir </> "." ]
  return srcDir

-- | Setup source directory and original source archive under
--   build-directory using rsync.
rsyncGenOrigSources :: Source -> Build (FilePath, FilePath)
rsyncGenOrigSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  origPath  <- origArchive pkg
  withBuildDir $ liftTrace . packInDir' (takeFileName srcDir) origPath
  copyDebianDir srcDir
  liftTrace $ confirmPath srcDir
  return (origPath, srcDir)

-- | Setup native source directory and native source archive under
--   build-directory using rsync.
rsyncGenNativeSources :: Source -> Build (FilePath, FilePath)
rsyncGenNativeSources pkg = do
  srcDir <- rsyncGenOrigSourceDir pkg
  copyDebianDir srcDir
  nativePath <- nativeArchive pkg
  withBuildDir $ liftTrace . packInDir' (takeFileName srcDir) nativePath
  liftTrace $ confirmPath srcDir
  return (nativePath, srcDir)

-- | Setup debian source directory and source archive.
rsyncGenSources :: Source -> Build (FilePath, FilePath)
rsyncGenSources pkg
  | isNative pkg = rsyncGenNativeSources pkg
  | otherwise    = rsyncGenOrigSources   pkg


-- Setup source archive using Cabal.
cabalGenArchive :: Hackage -> Build FilePath
cabalGenArchive hkg = do
  withBaseCurrentDir . liftTrace $ Cabal.sdist []
  baseDir <- askBaseDir
  let apath = baseDir </> hackageArchive hkg
  liftTrace $ confirmPath apath
  return apath

-- Setup original source archive using Cabal.
cabalGenOrigArchive :: HaskellPackage -> Build FilePath
cabalGenOrigArchive hpkg = do
  origPath <- origArchive $ package hpkg
  apath    <- cabalGenArchive $ hackage hpkg
  liftTrace $ do
    createDirectoryIfMissing $ takeDirectory origPath
    renameFile apath origPath
  return origPath

-- | Setup original source directory and archive using Cabal.
cabalGenOrigSources :: HaskellPackage -> Build (FilePath, FilePath)
cabalGenOrigSources hpkg = do
  origPath <- cabalGenOrigArchive hpkg
  srcDir   <- sourceDir $ package hpkg
  liftTrace $ do
    unpack origPath
    renameDirectory
      (takeDirectory origPath </> hackageLongName (hackage hpkg))
      srcDir
    confirmPath srcDir
  return (origPath, srcDir)

-- | Setup source directory and archive using Cabal.
cabalGenSources :: HaskellPackage -> Build (FilePath, FilePath)
cabalGenSources hpkg = do
  pair@(_, srcDir) <- cabalGenOrigSources hpkg
  copyDebianDir srcDir
  return pair

cabalAutogenDebianDir :: Maybe String   -- ^ May specify revision string
                      -> [String]       -- ^ Optional arguments of cabal-debian command
                      -> Build FilePath -- ^ Generated debian-dir path
cabalAutogenDebianDir mayRev cdArgs =  do
  baseDir  <-  askBaseDir
  let ddName =  "debian"
      tmpDD  =  baseDir </> ddName
  exist <- liftIO $ doesDirectoryExist tmpDD
  when exist (fail $ "Invalid state: directory already exist: " ++ tmpDD)

  debDir   <-  (</> ddName) <$> getBuildDir
  liftTrace $ do
    cabalDebian baseDir mayRev cdArgs
    createDirectoryIfMissing $ takeDirectory debDir
    renameDirectory tmpDD debDir
  return debDir

-- | Setup source directory and archive using Cabal and cabal-debian.
cabalAutogenSources :: String                                       -- ^ Hackge name string
                    -> Maybe String                                 -- ^ May specify revision string
                    -> [String]                                     -- ^ Optional arguments of cabal-debian command
                    -> Build ((FilePath, FilePath), HaskellPackage) -- ^ Result package informations of generated source
cabalAutogenSources hname mayRev cdArgs = do
  {- Fill Setup.hs to make cabal-debian can detect.
     Newer cabal-debian generates `DEB_SETUP_BIN_NAME = cabal' line,
     which causes home directory access errors at build time. -}
  liftIO . Cabal.fillSetupHs =<< askBaseDir
  debDir   <-  cabalAutogenDebianDir mayRev cdArgs
  pkg      <-  liftTrace . dpkgParseChangeLog $ debDir </> "changelog"
  let hpkg =   haskellPackageFromPackage hname pkg
  pair@(_, srcDir)  <-  cabalGenOrigSources hpkg
  liftTrace $ renameDirectory debDir (srcDir </> takeFileName debDir)
  return (pair, hpkg)


findDebianChangeLog :: MaybeT Build FilePath
findDebianChangeLog =  MaybeT $ do
  baseDir  <-  askBaseDir
  debDN    <-  debianDirName'
  let changelog = baseDir </> debDN </> "changelog"
  liftIO $ do
    exist <- doesFileExist changelog
    return $ if exist
             then Just changelog
             else Nothing

-- | Find debian .changes files
findDebianChanges :: Build [(FilePath, PackageType)]
findDebianChanges =  do
  bd <- getBuildDir
  fs <- liftIO $ getDirectoryContents bd
  return $ catMaybes
    [ do ty <- takeChangesType path
         Just (path, ty)
    | path <- map (bd </>) fs
    ]

findCabalDescription :: MaybeT Build FilePath
findCabalDescription =  MaybeT (askBaseDir >>= liftIO . Cabal.findDescriptionFile)

-- | On the fly setup of source directory and archive.
genSources :: Maybe String                                                -- ^ May specify revision string
           -> [String]                                                    -- ^ Optional arguments of cabal-debian command
           -> Build (Maybe ((FilePath, FilePath), Source, Maybe Hackage)) -- ^ Result package informations of generated source
genSources mayRev cdArgs = runMaybeT $
  do clog <- findDebianChangeLog
     src  <- lift . liftTrace $ dpkgParseChangeLog clog
     (do hname <- takeBaseName <$> findCabalDescription
         let hpkg = haskellPackageFromPackage hname src
         p <- lift $ cabalGenSources hpkg
         return (p, src, Just $ hackage hpkg)
      <|>
      do lift $ (,,) <$> rsyncGenSources src <*> pure src <*> pure Nothing)
  <|>
  do hname <- takeBaseName <$> findCabalDescription
     lift $ do
       (p, hpkg) <- cabalAutogenSources hname mayRev cdArgs
       return (p, package hpkg, Just $ hackage hpkg)
  <|>
  do fail "No source generate rule found."

-- | Probe generated source directory path.
findGeneratedSourceDir :: MaybeT Build FilePath
findGeneratedSourceDir = do
  bd  <- lift getBuildDir
  fs  <- liftIO $ getDirectoryContents bd
  msum
    [ do MaybeT . liftIO $ guard <$> doesFileExist (path </> "debian" </> "control")
         pure path
    | f  <- fs
    , f `notElem` [".", ".."]
    , let path = bd </> f
    ]

-- | Probe generated source informations
findGeneratedSource :: MaybeT Build (FilePath, Source, Hackage)
findGeneratedSource = do
  srcDir <- findGeneratedSourceDir
  src    <- lift . liftTrace $ dpkgParseChangeLog $ srcDir </> "debian" </> "changelog"
  hname <- takeBaseName <$> findCabalDescription
  return (srcDir, src, hackage $ haskellPackageFromPackage hname src)
