-- | Package-level processing for Stackage packages.
module StackageProgress.PackageRunner
  ( -- * Running packages
    runPackage,
    runPackageOrThrow,
    packageDependsOnUnsupportedBuildTool,

    -- * Source size calculation
    totalSourceSize,
  )
where

import Aihc.Hackage.VersionResolver (getLatestVersion)
import Control.Exception (IOException, SomeException, displayException, try)
import Data.Maybe (isJust)
import Data.Text (Text)
import HackageSupport
  ( FileInfo (..),
    downloadPackageQuietWithNetwork,
    findPackageBuildToolDependencyNames,
    findTargetFilesFromCabal,
  )
import StackageProgress.CLI (Options (..))
import StackageProgress.FileChecker
  ( FileCheckOptions (..),
    PackageFileSummary (..),
    emptyFileSummary,
    firstFailureMessage,
    foldFilesForPackage,
    getPackageFileErrors,
  )
import StackageProgress.Summary
  ( PackageResult (..),
    PackageSpec (..),
  )
import System.Directory (getFileSize)

unsupportedBuildToolNames :: [Text]
unsupportedBuildToolNames = ["genprimopcode", "grimprimopcode"]

-- | Return whether a package's active Cabal metadata requires an unsupported build tool.
packageDependsOnUnsupportedBuildTool :: Options -> PackageSpec -> IO Bool
packageDependsOnUnsupportedBuildTool opts spec = do
  result <- try (packageDependsOnUnsupportedBuildToolOrThrow opts spec) :: IO (Either SomeException Bool)
  pure $ case result of
    Right depends -> depends
    Left _ -> False

packageDependsOnUnsupportedBuildToolOrThrow :: Options -> PackageSpec -> IO Bool
packageDependsOnUnsupportedBuildToolOrThrow opts spec = do
  versionResult <- resolveSpecVersion spec
  case versionResult of
    Left _ -> pure False
    Right version -> do
      srcDir <- downloadPackageQuietWithNetwork (not (optOffline opts)) (pkgName spec) version
      toolNames <- findPackageBuildToolDependencyNames srcDir
      pure (any (`elem` unsupportedBuildToolNames) toolNames)

-- | Process a package, catching any exceptions.
runPackage :: Options -> PackageSpec -> IO PackageResult
runPackage opts spec = do
  result <- try (runPackageOrThrow opts spec)
  pure $ case result of
    Left err ->
      PackageResult
        { package = spec,
          packageOursOk = False,
          packageHseOk = False,
          packageGhcOk = False,
          packageReason = displayException (err :: SomeException),
          packageGhcError = Nothing,
          packageSourceSize = 0,
          packageFileErrors = []
        }
    Right pkgResult -> pkgResult

-- | Process a package, potentially throwing exceptions.
runPackageOrThrow :: Options -> PackageSpec -> IO PackageResult
runPackageOrThrow opts spec = do
  versionResult <- resolveSpecVersion spec
  case versionResult of
    Left err -> pure (versionFailure err)
    Right version -> runWithVersion version
  where
    runWithVersion :: String -> IO PackageResult
    runWithVersion version = do
      srcDir <- downloadPackageQuietWithNetwork (not (optOffline opts)) (pkgName spec) version
      files <- findTargetFilesFromCabal srcDir
      totalSize <- if optPrintFailedTable opts then totalSourceSize files else pure 0
      let checkOpts =
            FileCheckOptions
              { fileCheckKeepFirstFailure = optPrompt opts || isJust (optGhcErrorsFile opts),
                fileCheckKeepFileErrors = optPrintFailedTable opts,
                fileCheckKeepGhcError = isJust (optGhcErrorsFile opts)
              }
      if null files
        then
          pure
            PackageResult
              { package = spec,
                packageOursOk = True,
                packageHseOk = True,
                packageGhcOk = True,
                packageReason = "",
                packageGhcError = Nothing,
                packageSourceSize = totalSize,
                packageFileErrors = []
              }
        else do
          fileSummary <- foldFilesForPackage checkOpts (optParsers opts) (optVerbose opts) srcDir emptyFileSummary files
          let hseOk = packageFileHseOk fileSummary
              ghcOk = packageFileGhcOk fileSummary
              ghcError = packageFileGhcError fileSummary
              oursOk = packageFileOursOk fileSummary
              errors = getPackageFileErrors fileSummary
              reason
                | fileCheckKeepFirstFailure checkOpts = firstFailureMessage fileSummary
                | otherwise = ""
          if oursOk
            then
              pure
                PackageResult
                  { package = spec,
                    packageOursOk = True,
                    packageHseOk = hseOk,
                    packageGhcOk = ghcOk,
                    packageReason = "",
                    packageGhcError = ghcError,
                    packageSourceSize = totalSize,
                    packageFileErrors = errors
                  }
            else
              pure
                PackageResult
                  { package = spec,
                    packageOursOk = False,
                    packageHseOk = hseOk,
                    packageGhcOk = ghcOk,
                    packageReason = reason,
                    packageGhcError = ghcError,
                    packageSourceSize = totalSize,
                    packageFileErrors = errors
                  }

    versionFailure :: String -> PackageResult
    versionFailure err =
      PackageResult
        { package = spec,
          packageOursOk = False,
          packageHseOk = False,
          packageGhcOk = False,
          packageReason = "failed to resolve latest version for installed package: " ++ err,
          packageGhcError = Nothing,
          packageSourceSize = 0,
          packageFileErrors = []
        }

resolveSpecVersion :: PackageSpec -> IO (Either String String)
resolveSpecVersion spec =
  if pkgVersion spec == "installed"
    then getLatestVersion Nothing (pkgName spec)
    else pure (Right (pkgVersion spec))

-- | Calculate total source size for a list of files.
totalSourceSize :: [FileInfo] -> IO Integer
totalSourceSize infos = sum <$> mapM (safeFileSize . fileInfoPath) infos
  where
    safeFileSize path = do
      r <- try (getFileSize path) :: IO (Either IOException Integer)
      pure $ case r of
        Left _ -> 0
        Right n -> n
