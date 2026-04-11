{-# LANGUAGE OverloadedStrings #-}

-- | Thin adapter over "Aihc.Hackage" that converts string-typed results
-- to the rich types used by @aihc-parser@ executables and tests.
module HackageSupport
  ( downloadPackage,
    downloadPackageQuiet,
    downloadPackageQuietWithNetwork,
    findTargetFilesFromCabal,
    FileInfo (..),
    readTextFileLenient,
    resolveIncludeBestEffort,
    diagToText,
    prefixCppErrors,
  )
where

import Aihc.Cpp (Diagnostic (..), IncludeKind (..), IncludeRequest (..), Severity (..))
import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Download qualified as HD
import Aihc.Hackage.Types qualified as HT
import Aihc.Hackage.Util qualified as HU
import Aihc.Parser.Syntax qualified as Syntax
import Data.ByteString qualified as BS
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, makeRelative, normalise, splitDirectories, takeDirectory, (</>))

-- | Download a Hackage package with verbose logging.
downloadPackage :: String -> String -> IO FilePath
downloadPackage name version =
  HD.downloadPackage (HT.PackageSpec name version)

-- | Download a Hackage package silently.
downloadPackageQuiet :: String -> String -> IO FilePath
downloadPackageQuiet name version =
  HD.downloadPackageQuiet (HT.PackageSpec name version)

-- | Download a Hackage package, controlling network access.
downloadPackageQuietWithNetwork :: Bool -> String -> String -> IO FilePath
downloadPackageQuietWithNetwork allowNetwork name version =
  HD.downloadPackageWithOptions
    HD.defaultDownloadOptions
      { HD.downloadVerbose = False,
        HD.downloadAllowNetwork = allowNetwork
      }
    (HT.PackageSpec name version)

-- | Rich file info with parser-typed extensions and language.
data FileInfo = FileInfo
  { fileInfoPath :: FilePath,
    fileInfoExtensions :: [Syntax.ExtensionSetting],
    fileInfoCppOptions :: [String],
    fileInfoLanguage :: Maybe Syntax.LanguageEdition,
    fileInfoDependencies :: [Text]
  }
  deriving (Show)

-- | Find target Haskell source files from a @.cabal@ file.
--
-- Delegates to 'Aihc.Hackage.Cabal.collectComponentFiles' and then converts
-- the string-typed results to the rich types used by @aihc-parser@.
findTargetFilesFromCabal :: FilePath -> IO [FileInfo]
findTargetFilesFromCabal extractedRoot = do
  cabalFiles <- HU.findCabalFiles extractedRoot
  cabalFile <-
    case cabalFiles of
      [file] -> pure file
      [] ->
        ioError
          ( userError
              ("No .cabal file found under extracted package root: " ++ extractedRoot)
          )
      files -> pure (HU.chooseBestCabalFile extractedRoot files)
  cabalBytes <- BS.readFile cabalFile
  let (_, parseResult) = runParseResult (parseGenericPackageDescription cabalBytes)
  gpd <-
    case parseResult of
      Right parsed -> pure parsed
      Left (_, errs) ->
        ioError
          ( userError
              ("Failed to parse cabal file " ++ cabalFile ++ ": " ++ show errs)
          )
  rawFiles <- HC.collectComponentFiles gpd (takeDirectory cabalFile)
  pure (map convertFileInfo rawFiles)

-- | Convert a string-typed 'HC.FileInfo' to the rich-typed local 'FileInfo'.
convertFileInfo :: HC.FileInfo -> FileInfo
convertFileInfo raw =
  FileInfo
    { fileInfoPath = HC.fileInfoPath raw,
      fileInfoExtensions = mapMaybe (Syntax.parseExtensionSettingName . T.pack) (HC.fileInfoExtensions raw),
      fileInfoCppOptions = HC.fileInfoCppOptions raw,
      fileInfoLanguage = HC.fileInfoLanguage raw >>= Syntax.parseLanguageEdition . T.pack,
      fileInfoDependencies = HC.fileInfoDependencies raw
    }

readTextFileLenient :: FilePath -> IO Text
readTextFileLenient = HU.readTextFileLenient

resolveIncludeBestEffort :: FilePath -> FilePath -> IncludeRequest -> IO (Maybe BS.ByteString)
resolveIncludeBestEffort packageRoot currentFile req = do
  firstExisting <- firstExistingPath (includeCandidates packageRoot currentFile req)
  case firstExisting of
    Nothing -> pure Nothing
    Just includeFile -> Just <$> BS.readFile includeFile

includeCandidates :: FilePath -> FilePath -> IncludeRequest -> [FilePath]
includeCandidates packageRoot currentFile req =
  map normalise $ nub [dir </> includePath req | dir <- searchDirs]
  where
    includeDir = takeDirectory (includeFrom req)
    sourceRelDir = takeDirectory (makeRelative packageRoot currentFile)
    packageAncestors = ancestorDirs sourceRelDir
    localRoots =
      [ takeDirectory currentFile,
        packageRoot </> sourceRelDir,
        packageRoot </> includeDir
      ]
    systemRoots =
      [ packageRoot </> "include",
        packageRoot </> "includes",
        packageRoot </> "cbits",
        packageRoot
      ]
    searchDirs =
      case includeKind req of
        IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots
        IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  case filter (not . null) (splitDirectories path) of
    [] -> []
    parts ->
      [ foldl (</>) "." (take n parts)
      | n <- [length parts, length parts - 1 .. 1]
      ]

firstExistingPath :: [FilePath] -> IO (Maybe FilePath)
firstExistingPath [] = pure Nothing
firstExistingPath (candidate : rest) = do
  let path = if isAbsolute candidate then candidate else normalise candidate
  exists <- doesFileExist path
  if exists
    then pure (Just path)
    else firstExistingPath rest

diagToText :: Diagnostic -> Text
diagToText diag =
  T.pack (diagFile diag)
    <> ":"
    <> T.pack (show (diagLine diag))
    <> ": "
    <> sev
    <> ": "
    <> diagMessage diag
  where
    sev =
      case diagSeverity diag of
        Warning -> "warning"
        Error -> "error"

prefixCppErrors :: Maybe Text -> Text -> Text
prefixCppErrors cppMsg msg =
  case cppMsg of
    Nothing -> msg
    Just cppText -> "cpp diagnostics:\n" <> cppText <> "\n" <> msg
