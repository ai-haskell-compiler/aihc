{-# LANGUAGE BangPatterns #-}

module StackageProgress.Summary
  ( FailedPackage (..),
    PackageResult (..),
    PackageSpec (..),
    PromptCandidate (..),
    RunSummary,
    SummaryOptions (..),
    addPackageResults,
    emptySummary,
    finalizeSummary,
    forceString,
    formatPackage,
    packageParserFailed,
    promptCandidateFromResult,
    renderPrompt,
    selectPromptCandidate,
    summaryFailedPackages,
    summaryGhcErrors,
    summarySuccessGhcN,
    summarySuccessHseN,
    summarySuccessOursN,
    summarySucceededPackages,
  )
where

import Data.Char (isSpace)
import qualified Data.List as List

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

data PromptCandidate = PromptCandidate
  { promptPackageName :: String,
    promptErrorMessage :: String
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
    summarySucceededPackagesAcc :: [String],
    summaryFailedPackagesAcc :: [FailedPackage],
    summaryGhcErrorsAcc :: [(String, String)],
    summaryGhcErrorsStored :: !Int
  }

emptySummary :: RunSummary
emptySummary =
  RunSummary
    { summarySuccessOursN = 0,
      summarySuccessHseN = 0,
      summarySuccessGhcN = 0,
      summarySucceededPackagesAcc = [],
      summaryFailedPackagesAcc = [],
      summaryGhcErrorsAcc = [],
      summaryGhcErrorsStored = 0
    }

addPackageResults :: SummaryOptions -> [PackageResult] -> RunSummary -> RunSummary
addPackageResults opts results summary0 = List.foldl' (addPackageResult opts) summary0 results
  where
    addPackageResult :: SummaryOptions -> RunSummary -> PackageResult -> RunSummary
    addPackageResult summaryOpts summary result =
      let !oursN = summarySuccessOursN summary + boolToInt (packageOursOk result)
          !hseN = summarySuccessHseN summary + boolToInt (packageHseOk result)
          !ghcN = summarySuccessGhcN summary + boolToInt (packageGhcOk result)
          !pkgLabel = forceString (formatPackage (package result))
          succeededRev =
            if summaryKeepSucceeded summaryOpts && packageOursOk result
              then pkgLabel : summarySucceededPackagesAcc summary
              else summarySucceededPackagesAcc summary
          failedRev =
            if summaryKeepFailedPackages summaryOpts && packageParserFailed result
              then FailedPackage pkgLabel (packageSourceSize result) : summaryFailedPackagesAcc summary
              else summaryFailedPackagesAcc summary
          (!ghcStored, ghcErrorsRev) = addGhcErrorIfNeeded summaryOpts summary result pkgLabel
       in RunSummary
            { summarySuccessOursN = oursN,
              summarySuccessHseN = hseN,
              summarySuccessGhcN = ghcN,
              summarySucceededPackagesAcc = succeededRev,
              summaryFailedPackagesAcc = failedRev,
              summaryGhcErrorsAcc = ghcErrorsRev,
              summaryGhcErrorsStored = ghcStored
            }

    addGhcErrorIfNeeded :: SummaryOptions -> RunSummary -> PackageResult -> String -> (Int, [(String, String)])
    addGhcErrorIfNeeded summaryOpts summary result pkgLabel
      | packageGhcOk result = (summaryGhcErrorsStored summary, summaryGhcErrorsAcc summary)
      | summaryGhcErrorsStored summary >= summaryGhcErrorLimit summaryOpts = (summaryGhcErrorsStored summary, summaryGhcErrorsAcc summary)
      | otherwise =
          let !message = forceString (ghcFailureMessage result)
           in (summaryGhcErrorsStored summary + 1, (pkgLabel, message) : summaryGhcErrorsAcc summary)

finalizeSummary :: RunSummary -> RunSummary
finalizeSummary summary =
  summary
    { summarySucceededPackagesAcc = reverse (summarySucceededPackagesAcc summary),
      summaryFailedPackagesAcc = reverse (summaryFailedPackagesAcc summary),
      summaryGhcErrorsAcc = reverse (summaryGhcErrorsAcc summary)
    }

summarySucceededPackages :: RunSummary -> [String]
summarySucceededPackages = summarySucceededPackagesAcc

summaryFailedPackages :: RunSummary -> [FailedPackage]
summaryFailedPackages = summaryFailedPackagesAcc

summaryGhcErrors :: RunSummary -> [(String, String)]
summaryGhcErrors = summaryGhcErrorsAcc

formatPackage :: PackageSpec -> String
formatPackage spec = pkgName spec ++ "-" ++ pkgVersion spec

packageParserFailed :: PackageResult -> Bool
packageParserFailed result = not (packageOursOk result) && packageSourceSize result > 0

promptCandidateFromResult :: PackageResult -> Maybe PromptCandidate
promptCandidateFromResult result
  | packageOursOk result = Nothing
  | otherwise =
      Just
        PromptCandidate
          { promptPackageName = pkgName (package result),
            promptErrorMessage = normalizePromptErrorMessage (packageReason result)
          }

renderPrompt :: PromptCandidate -> String
renderPrompt candidate =
  unlines
    [ "# Error messages:",
      promptErrorMessage candidate,
      "",
      "Re-test by running: nix run .#hackage-tester -- " ++ promptPackageName candidate,
      "Haskell 2010 language report (contains a slightly outdated specification for syntax): docs/haskell2010-language-report.md",
      "",
      "Fix the parsing issue that prevents '" ++ promptPackageName candidate ++ "' from parsing successfully. Remember to include unit tests, golden tests, oracle tests, and quickcheck properties if appropriate. Open a PR when done."
    ]

selectPromptCandidate :: Integer -> [PromptCandidate] -> Maybe PromptCandidate
selectPromptCandidate _ [] = Nothing
selectPromptCandidate seed candidates =
  let n = length candidates
      idx = fromInteger (seed `mod` toInteger n)
   in Just (candidates !! idx)

ghcFailureMessage :: PackageResult -> String
ghcFailureMessage result =
  case packageGhcError result of
    Just err -> forceString err
    Nothing ->
      let reason = trim (packageReason result)
       in if null reason
            then "GHC check failed without diagnostic details"
            else "No direct GHC diagnostic; package failed before/around GHC check: " ++ forceString reason

normalizePromptErrorMessage :: String -> String
normalizePromptErrorMessage raw =
  let message =
        case trim raw of
          "" -> "parse failed without detailed diagnostics"
          trimmed -> trimmed
   in if "PARSE_ERROR:" `List.isPrefixOf` message
        then message
        else "PARSE_ERROR: " ++ message

trim :: String -> String
trim = List.dropWhileEnd isSpace . dropWhile isSpace

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

forceString :: String -> String
forceString value = length value `seq` value
