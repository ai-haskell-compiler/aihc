{-|
Module:    Distribution.AppImage
Copyright: 2020-2025 Gabriele Sales

This module provides a custom build hook for automating the creation of AppImage
bundles.

Internally, it calls the [appimagetool](https://github.com/AppImage/AppImageKit)
and [linuxdeploy](https://github.com/linuxdeploy/linuxdeploy) utilities which
must be already present on the system.
-}

{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

module Distribution.AppImage
  ( AppImage(..)
  , AppDirCustomize
  , appImageBuildHook
  )
where

import           Control.Monad
import           Data.Maybe
import           Data.String
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.Program
import           Distribution.Simple.Program.Types
import           Distribution.Simple.Setup
import           Distribution.Simple.Utils
import           Distribution.System
import           Distribution.Verbosity
import           System.FilePath


data AppImage = AppImage {
  -- | Application name. The AppImage bundle will be produced in
  -- @dist\/build\//appName/.AppImage@ and will contain the executable
  -- /appName/.
  appName         :: String,
  -- | Path to desktop file.
  appDesktop      :: FilePath,
  -- | Application icons.
  appIcons        :: [FilePath],
  -- | Other resources to bundle. Stored in the @\usr\/share\//appName/@
  -- directory inside the image. The first 'FilePath' is on the local system.
  -- The @Maybe 'FilePath'@ is the desired file path relative to
  -- @\usr\/share\//appName/@, or the directoryless filename in the case of
  -- 'Nothing'.
  appResources    :: [(FilePath, Maybe FilePath)],
  -- | Hook to customize the generated @AppDir@ before final packaging.
  appDirCustomize :: Maybe AppDirCustomize
  }

type AppDirCustomize
  = FilePath   -- ^ AppDir path.
 -> Args       -- ^ Other parameters as defined in 'Distribution.Simple.postBuild'.
 -> BuildFlags
 -> PackageDescription
 -> LocalBuildInfo
 -> IO ()


-- | Hook for building AppImage bundles. Does nothing if the OS is not Linux.
--
-- Use this function as a @postBuild@ hook.
appImageBuildHook
  :: [AppImage] -- ^ Applications to build.
  -> Args       -- ^ Other parameters as defined in 'Distribution.Simple.postBuild'.
  -> BuildFlags
  -> PackageDescription
  -> LocalBuildInfo
  -> IO ()
appImageBuildHook apps args flags pkg buildInfo =
  when (buildOS == Linux) $
    mapM_ (makeBundle args flags pkg buildInfo) apps

makeBundle :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> AppImage -> IO ()
makeBundle args flags pkg buildInfo app@AppImage{..} = do
#if MIN_VERSION_Cabal(3,14,0)
  let bdir = interpretSymbolicPathLBI buildInfo (buildDir buildInfo)
#else
  let bdir = buildDir buildInfo
#endif
      verb = fromFlagOrDefault normal (buildVerbosity flags)
  unless (hasExecutable pkg appName) $
    die' verb ("No executable defined for the AppImage bundle: " ++ appName)
  when (null appIcons) $
    die' verb ("No icon defined for the AppImage bundle: " ++ appName)
  withTempDirectory verb bdir "appimage." $ \appDir -> do
    deployExe (bdir </> appName </> appName) app appDir verb
    bundleFiles appResources (appDir </> "usr" </> "share" </> appName) verb
    fromMaybe noCustomization appDirCustomize appDir args flags pkg buildInfo
    bundleApp appDir verb

hasExecutable :: PackageDescription -> String -> Bool
hasExecutable pkg name =
  any (\e -> exeName e == fromString name) (executables pkg)

deployExe :: FilePath -> AppImage -> FilePath -> Verbosity -> IO ()
deployExe exe AppImage{..} appDir verb = do
  prog <- findProg "linuxdeploy" verb
  runProgram verb prog $
    [ "--appdir=" ++ appDir
    , "--executable=" ++ exe
    , "--desktop-file=" ++ appDesktop ] ++
    map ("--icon-file=" ++) appIcons

bundleFiles :: [(FilePath, Maybe FilePath)] -> FilePath -> Verbosity -> IO ()
bundleFiles files dest verb = prepare >> mapM_ (uncurry copy) files
  where
    prepare = createDirectoryIfMissingVerbose verb True dest

    copy file destfile = do
      let fp = dest </> fromMaybe (takeFileName file) destfile
      createDirectoryIfMissingVerbose verb True $ takeDirectory fp
      copyFileVerbose verb file fp

bundleApp :: FilePath -> Verbosity -> IO ()
bundleApp appDir verb = do
  prog <- findProg "appimagetool" verb
  let (wdir, name) = splitFileName appDir
  runProgramInvocation verb $
    (programInvocation prog [name]) { progInvokeCwd = Just wdir }

noCustomization :: AppDirCustomize
noCustomization _ _ _ _ _ = return ()


findProg :: String -> Verbosity -> IO ConfiguredProgram
findProg name verb = do
  found <- findProgramOnSearchPath verb defaultProgramSearchPath name
  case found of
    Nothing        -> die' verb ("Command " ++ name ++ " is not available")
    Just (path, _) -> return (simpleConfiguredProgram name (FoundOnSystem path))
