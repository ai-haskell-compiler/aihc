{-# LANGUAGE OverloadedStrings #-}

-- | Package-level processing for Stackage packages.
module StackageProgress.PackageRunner
  ( -- * Options
    PackageRunOptions (..),
    packageRunOptionsFromStackageOptions,

    -- * Running packages
    runPackage,
    runPackageOrThrow,
    packageDependsOnUnsupportedBuildTool,
    packageUsesUnsupportedMetadata,
    packageUsesCustomPreprocessor,

    -- * Source size calculation
    totalSourceSize,
  )
where

import Aihc.Hackage.VersionResolver (getLatestVersion)
import Control.Exception (IOException, SomeException, displayException, try)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import HackageSupport
  ( FileInfo (..),
    downloadPackageQuietWithNetwork,
    findPackageBuildToolDependencyNames,
    findPackageUsesCustomPreprocessor,
    findTargetFilesFromCabal,
  )
import StackageProgress.CLI (Options (..))
import StackageProgress.CLI qualified as StackageCLI
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

-- | Package-level runner options shared by Stackage and Hackage progress tools.
data PackageRunOptions = PackageRunOptions
  { runOptParsers :: ![StackageCLI.Parser],
    runOptOffline :: !Bool,
    runOptPrompt :: !Bool,
    runOptPrintFailedTable :: !Bool,
    runOptGhcErrorsFile :: !(Maybe FilePath),
    runOptVerbose :: !Bool
  }
  deriving (Eq, Show)

packageRunOptionsFromStackageOptions :: Options -> PackageRunOptions
packageRunOptionsFromStackageOptions opts =
  PackageRunOptions
    { runOptParsers = optParsers opts,
      runOptOffline = optOffline opts,
      runOptPrompt = optPrompt opts,
      runOptPrintFailedTable = optPrintFailedTable opts,
      runOptGhcErrorsFile = optGhcErrorsFile opts,
      runOptVerbose = optVerbose opts
    }

unsupportedBuildToolNames :: [Text]
unsupportedBuildToolNames = ["genprimopcode"]

-- | Return whether a package's active Cabal metadata uses any unsupported package-level feature.
packageUsesUnsupportedMetadata :: PackageRunOptions -> PackageSpec -> IO Bool
packageUsesUnsupportedMetadata opts spec = do
  result <- try (packageUsesUnsupportedMetadataOrThrow opts spec) :: IO (Either SomeException Bool)
  pure $ case result of
    Right unsupported -> unsupported
    Left _ -> False

packageUsesUnsupportedMetadataOrThrow :: PackageRunOptions -> PackageSpec -> IO Bool
packageUsesUnsupportedMetadataOrThrow opts spec = do
  fromMaybe False <$> withResolvedPackage opts spec checkPackage
  where
    checkPackage srcDir = do
      toolNames <- findPackageBuildToolDependencyNames srcDir
      usesCustomPreprocessor <- findPackageUsesCustomPreprocessor srcDir
      pure (usesCustomPreprocessor || any (`elem` unsupportedBuildToolNames) toolNames)

-- | Return whether a package's active Cabal metadata requires an unsupported build tool.
packageDependsOnUnsupportedBuildTool :: PackageRunOptions -> PackageSpec -> IO Bool
packageDependsOnUnsupportedBuildTool opts spec = do
  result <- try (packageDependsOnUnsupportedBuildToolOrThrow opts spec) :: IO (Either SomeException Bool)
  pure $ case result of
    Right depends -> depends
    Left _ -> False

packageDependsOnUnsupportedBuildToolOrThrow :: PackageRunOptions -> PackageSpec -> IO Bool
packageDependsOnUnsupportedBuildToolOrThrow opts spec = do
  fromMaybe False <$> withResolvedPackage opts spec checkPackage
  where
    checkPackage srcDir = do
      toolNames <- findPackageBuildToolDependencyNames srcDir
      pure (any (`elem` unsupportedBuildToolNames) toolNames)

-- | Return whether a package's active Cabal metadata requests a custom GHC preprocessor.
packageUsesCustomPreprocessor :: PackageRunOptions -> PackageSpec -> IO Bool
packageUsesCustomPreprocessor opts spec = do
  result <- try (packageUsesCustomPreprocessorOrThrow opts spec) :: IO (Either SomeException Bool)
  pure $ case result of
    Right usesPreprocessor -> usesPreprocessor
    Left _ -> False

packageUsesCustomPreprocessorOrThrow :: PackageRunOptions -> PackageSpec -> IO Bool
packageUsesCustomPreprocessorOrThrow opts spec = do
  fromMaybe False <$> withResolvedPackage opts spec findPackageUsesCustomPreprocessor

withResolvedPackage :: PackageRunOptions -> PackageSpec -> (FilePath -> IO a) -> IO (Maybe a)
withResolvedPackage opts spec action = do
  versionResult <- resolveSpecVersion spec
  case versionResult of
    Left _ -> pure Nothing
    Right version -> do
      srcDir <- downloadPackageQuietWithNetwork (not (runOptOffline opts)) (pkgName spec) version
      Just <$> action srcDir

-- | Process a package, catching any exceptions.
runPackage :: PackageRunOptions -> PackageSpec -> IO PackageResult
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
runPackageOrThrow :: PackageRunOptions -> PackageSpec -> IO PackageResult
runPackageOrThrow opts spec = do
  versionResult <- resolveSpecVersion spec
  case versionResult of
    Left err -> pure (versionFailure err)
    Right version -> runWithVersion version
  where
    runWithVersion :: String -> IO PackageResult
    runWithVersion version = do
      srcDir <- downloadPackageQuietWithNetwork (not (runOptOffline opts)) (pkgName spec) version
      files <- findTargetFilesFromCabal srcDir
      totalSize <- if runOptPrintFailedTable opts then totalSourceSize files else pure 0
      let checkOpts =
            FileCheckOptions
              { fileCheckKeepFirstFailure = runOptPrompt opts || isJust (runOptGhcErrorsFile opts),
                fileCheckKeepFileErrors = runOptPrintFailedTable opts,
                fileCheckKeepGhcError = isJust (runOptGhcErrorsFile opts)
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
          fileSummary <- foldFilesForPackage checkOpts (runOptParsers opts) (runOptVerbose opts) srcDir emptyFileSummary files
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
