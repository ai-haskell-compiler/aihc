-- | Package-level processing for Stackage packages.
module StackageProgress.PackageRunner
  ( -- * Running packages
    runPackage,
    runPackageOrThrow,

    -- * Source size calculation
    totalSourceSize,
  )
where

import Aihc.Hackage.VersionResolver (getLatestVersion)
import Control.Exception (IOException, SomeException, displayException, try)
import HackageSupport
  ( FileInfo (..),
    downloadPackageQuietWithNetwork,
    findTargetFilesFromCabal,
  )
import StackageProgress.CLI (Options (..))
import StackageProgress.FileChecker
  ( PackageFileSummary (..),
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
  versionResult <- resolveVersion
  case versionResult of
    Left errResult -> pure errResult
    Right version -> runWithVersion version
  where
    resolveVersion :: IO (Either PackageResult String)
    resolveVersion =
      if pkgVersion spec == "installed"
        then do
          latestResult <- getLatestVersion Nothing (pkgName spec)
          pure $ case latestResult of
            Left err ->
              Left
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
            Right ver -> Right ver
        else pure (Right (pkgVersion spec))

    runWithVersion :: String -> IO PackageResult
    runWithVersion version = do
      srcDir <- downloadPackageQuietWithNetwork (not (optOffline opts)) (pkgName spec) version
      files <- findTargetFilesFromCabal srcDir
      totalSize <- if optPrintFailedTable opts then totalSourceSize files else pure 0
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
          fileSummary <- foldFilesForPackage (optParsers opts) (optVerbose opts) srcDir emptyFileSummary files
          let hseOk = packageFileHseOk fileSummary
              ghcOk = packageFileGhcOk fileSummary
              ghcError = packageFileGhcError fileSummary
              oursOk = packageFileOursOk fileSummary
              errors = getPackageFileErrors fileSummary
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
                    packageReason = firstFailureMessage fileSummary,
                    packageGhcError = ghcError,
                    packageSourceSize = totalSize,
                    packageFileErrors = errors
                  }

-- | Calculate total source size for a list of files.
totalSourceSize :: [FileInfo] -> IO Integer
totalSourceSize infos = sum <$> mapM (safeFileSize . fileInfoPath) infos
  where
    safeFileSize path = do
      r <- try (getFileSize path) :: IO (Either IOException Integer)
      pure $ case r of
        Left _ -> 0
        Right n -> n
