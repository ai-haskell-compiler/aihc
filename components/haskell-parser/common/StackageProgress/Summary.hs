{-# LANGUAGE BangPatterns #-}

module StackageProgress.Summary
  ( FailedPackage (..),
    PackageResult (..),
    PackageSpec (..),
    RunSummary,
    SummaryOptions (..),
    addPackageResults,
    emptySummary,
    finalizeSummary,
    formatPackage,
    packageParserFailed,
    summaryFailedPackages,
    summaryGhcErrors,
    summarySuccessGhcN,
    summarySuccessHseN,
    summarySuccessOursN,
    summarySucceededPackages,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

data PackageSpec = PackageSpec
  { pkgName :: String,
    pkgVersion :: String
  }
  deriving (Eq, Show)

data PackageResult = PackageResult
  { package :: PackageSpec,
    packageOursOk :: Bool,
    packageHseOk :: Bool,
    packageGhcOk :: Bool,
    packageReason :: String,
    packageGhcError :: Maybe String,
    packageSourceSize :: Integer
  }

data FailedPackage = FailedPackage
  { failedPackageName :: String,
    failedPackageSourceSize :: Integer
  }
  deriving (Eq, Show)

data SummaryOptions = SummaryOptions
  { summaryKeepSucceeded :: Bool,
    summaryKeepFailedPackages :: Bool,
    summaryGhcErrorLimit :: Int
  }

data RunSummary = RunSummary
  { summarySuccessOursN :: !Int,
    summarySuccessHseN :: !Int,
    summarySuccessGhcN :: !Int,
    summarySucceededPackagesRev :: [String],
    summaryFailedPackagesRev :: [FailedPackage],
    summaryGhcErrorsRev :: [(String, String)],
    summaryGhcErrorsStored :: !Int
  }

emptySummary :: RunSummary
emptySummary =
  RunSummary
    { summarySuccessOursN = 0,
      summarySuccessHseN = 0,
      summarySuccessGhcN = 0,
      summarySucceededPackagesRev = [],
      summaryFailedPackagesRev = [],
      summaryGhcErrorsRev = [],
      summaryGhcErrorsStored = 0
    }

addPackageResults :: SummaryOptions -> [PackageResult] -> RunSummary -> RunSummary
addPackageResults opts results summary0 = foldl' (addPackageResult opts) summary0 results
  where
    addPackageResult :: SummaryOptions -> RunSummary -> PackageResult -> RunSummary
    addPackageResult summaryOpts summary result =
      let !oursN = summarySuccessOursN summary + boolToInt (packageOursOk result)
          !hseN = summarySuccessHseN summary + boolToInt (packageHseOk result)
          !ghcN = summarySuccessGhcN summary + boolToInt (packageGhcOk result)
          !pkgLabel = forceString (formatPackage (package result))
          succeededRev =
            if summaryKeepSucceeded summaryOpts && packageOursOk result
              then pkgLabel : summarySucceededPackagesRev summary
              else summarySucceededPackagesRev summary
          failedRev =
            if summaryKeepFailedPackages summaryOpts && packageParserFailed result
              then FailedPackage pkgLabel (packageSourceSize result) : summaryFailedPackagesRev summary
              else summaryFailedPackagesRev summary
          (!ghcStored, ghcErrorsRev) = addGhcErrorIfNeeded summaryOpts summary result pkgLabel
       in RunSummary
            { summarySuccessOursN = oursN,
              summarySuccessHseN = hseN,
              summarySuccessGhcN = ghcN,
              summarySucceededPackagesRev = succeededRev,
              summaryFailedPackagesRev = failedRev,
              summaryGhcErrorsRev = ghcErrorsRev,
              summaryGhcErrorsStored = ghcStored
            }

    addGhcErrorIfNeeded :: SummaryOptions -> RunSummary -> PackageResult -> String -> (Int, [(String, String)])
    addGhcErrorIfNeeded summaryOpts summary result pkgLabel
      | packageGhcOk result = (summaryGhcErrorsStored summary, summaryGhcErrorsRev summary)
      | summaryGhcErrorsStored summary >= summaryGhcErrorLimit summaryOpts = (summaryGhcErrorsStored summary, summaryGhcErrorsRev summary)
      | otherwise =
          let !message = forceString (ghcFailureMessage result)
           in (summaryGhcErrorsStored summary + 1, (pkgLabel, message) : summaryGhcErrorsRev summary)

finalizeSummary :: RunSummary -> RunSummary
finalizeSummary summary =
  summary
    { summarySucceededPackagesRev = reverse (summarySucceededPackagesRev summary),
      summaryFailedPackagesRev = reverse (summaryFailedPackagesRev summary),
      summaryGhcErrorsRev = reverse (summaryGhcErrorsRev summary)
    }

summarySucceededPackages :: RunSummary -> [String]
summarySucceededPackages = summarySucceededPackagesRev

summaryFailedPackages :: RunSummary -> [FailedPackage]
summaryFailedPackages = summaryFailedPackagesRev

summaryGhcErrors :: RunSummary -> [(String, String)]
summaryGhcErrors = summaryGhcErrorsRev

formatPackage :: PackageSpec -> String
formatPackage spec = pkgName spec ++ "-" ++ pkgVersion spec

packageParserFailed :: PackageResult -> Bool
packageParserFailed result = not (packageOursOk result) && packageSourceSize result > 0

ghcFailureMessage :: PackageResult -> String
ghcFailureMessage result =
  case packageGhcError result of
    Just err -> forceString err
    Nothing ->
      let reason = trim (packageReason result)
       in if null reason
            then "GHC check failed without diagnostic details"
            else "No direct GHC diagnostic; package failed before/around GHC check: " ++ forceString reason

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

forceString :: String -> String
forceString value = length value `seq` value
