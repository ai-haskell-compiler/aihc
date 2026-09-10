-- |
-- Module      : Aihc.PackagePlan.Source
-- Description : Load and parse one module of a planned package
--
-- Reads a source file the way the compiler front end does: BOM stripping,
-- bird-track unliterating, CPP with the emulated GHC's @MIN_VERSION_*@
-- macros, and parsing with the extensions from the cabal file and the module
-- header.
module Aihc.PackagePlan.Source
  ( ParsedInterfaceFile (..),
    ModuleDeps (..),
    moduleDepsDigest,
    parseInterfaceFile,
    parseInterfaceBytes,
  )
where

import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (DependencyVersions, compilerCppHeader, cppMacrosFromOptions, injectSyntheticCppMacros)
import Aihc.PackagePlan.Diagnostic (DiagnosticSourceMap, cppDiagnosticValue, diagnosticSourceMap, parseDiagnosticValue)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (..),
    ExtensionSetting (..),
    LanguageEdition (..),
    Module (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.List (nub, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Numeric (showHex)
import System.Directory (doesFileExist)
import System.FilePath (makeRelative, normalise, splitDirectories, takeDirectory, takeExtension, (</>))

-- | One loaded source file: its path, the parsed module, the source lines
-- for diagnostics, the parse and CPP diagnostics, the effective extensions,
-- the source text after preprocessing, and everything the parse depended on.
-- The text is what the parser saw, so offsets in the module's spans index
-- into it directly.
data ParsedInterfaceFile
  = ParsedInterfaceFile !FilePath Module !DiagnosticSourceMap [Aeson.Value] [Aeson.Value] [Extension] !Text !ModuleDeps

-- | Everything outside the compiler itself that decides how one module
-- parses. Two builds that agree on every component read the same source text
-- under the same extensions, so they produce the same 'Module' and a build
-- keyed on these may reuse the artifacts of the earlier one.
--
-- Keys are relative to the package root and values are content digests, so
-- the same package unpacked in two directories yields equal dependencies.
data ModuleDeps = ModuleDeps
  { -- | A digest of the module's own bytes, before any normalization.
    moduleDepsSource :: !Text,
    -- | A digest of the cabal-level settings that shape the parse: the
    -- component's default extensions and language edition, and, for a module
    -- that runs CPP, its @cpp-options@ and the names of its dependencies as
    -- well. A module that does not run CPP depends on the extensions alone.
    moduleDepsConfig :: !Text,
    -- | Every @#include "header.h"@ that resolved to a file of the package,
    -- keyed by its path relative to the package root and valued by a digest
    -- of its raw bytes. A @#include \<header.h\>@ names a header of the
    -- environment, not of the package, and is not tracked. Empty for a
    -- module that does not run CPP.
    moduleDepsIncludes :: !(Map FilePath Text),
    -- | The versions the module's @MIN_VERSION_*@ macros report, restricted
    -- to the packages it depends on. Empty for a module that does not run
    -- CPP.
    moduleDepsVersions :: !DependencyVersions
  }
  deriving (Eq, Show)

-- | One digest standing for the whole dependency set, for a caller that
-- keys a build stamp on it.
moduleDepsDigest :: ModuleDeps -> Text
moduleDepsDigest deps =
  digestChunks
    ( [TE.encodeUtf8 (moduleDepsSource deps), TE.encodeUtf8 (moduleDepsConfig deps)]
        <> concat
          [ [BS8.pack path, TE.encodeUtf8 digest]
          | (path, digest) <- M.toAscList (moduleDepsIncludes deps)
          ]
        <> [BS8.pack (show (M.toAscList (moduleDepsVersions deps)))]
    )

-- | A hex SHA-256 over length-prefixed fields, so that no two different
-- field lists share a digest.
digestChunks :: [BS.ByteString] -> Text
digestChunks =
  T.pack . concatMap hex . BS.unpack . SHA256.hash . BS.concat . concatMap field
  where
    field bytes = [BS8.pack (show (BS.length bytes) <> ":"), bytes]
    hex byte = let value = showHex byte "" in replicate (2 - length value) '0' <> value

parseInterfaceFile :: FilePath -> DependencyVersions -> HackageCabal.FileInfo -> IO ParsedInterfaceFile
parseInterfaceFile packageRoot versions fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  parseInterfaceBytes packageRoot versions fileInfo bytes

-- | Parse the same source bytes that the caller uses for its input hash.
parseInterfaceBytes :: FilePath -> DependencyVersions -> HackageCabal.FileInfo -> BS.ByteString -> IO ParsedInterfaceFile
parseInterfaceBytes packageRoot versions fileInfo bytes = do
  let normalized = normalizeSource path (TE.decodeUtf8With lenientDecode bytes)
      cabalExtSettings = mapMaybe (parseExtensionSettingName . T.pack) (HackageCabal.fileInfoExtensions fileInfo)
      -- The cabal settings come first and the module's own pragmas after, so
      -- a @{-# LANGUAGE NoCPP #-}@ turns off a @default-extensions: CPP@ the
      -- same way a later setting wins anywhere else.
      cppEnabled = foldl applyCppSetting False (cabalExtSettings <> headerExtensionSettings (readModuleHeaderPragmas normalized))
  (source, cppDiagnostics, includes) <-
    if cppEnabled
      then preprocessInterfaceSource packageRoot versions fileInfo normalized
      else pure (normalized, [], M.empty)
  let headerPragmas = readModuleHeaderPragmas source
      allExtSettings = cabalExtSettings <> headerExtensionSettings headerPragmas
      language =
        headerLanguageEdition headerPragmas
          `orElse` (HackageCabal.fileInfoLanguage fileInfo >>= parseLanguageEdition . T.pack)
      extensions = effectiveExtensions (fromMaybe Haskell98Edition language) allExtSettings
      cfg = defaultConfig {parserSourceName = path, parserExtensions = extensions}
      (parseErrs, modu) = parseModule cfg source
      parseDiagnostics = map (parseDiagnosticValue path) parseErrs
      sourceLines = diagnosticSourceMap path source
      dependencies = HackageCabal.fileInfoDependencies fileInfo
      deps =
        ModuleDeps
          { moduleDepsSource = digestChunks [bytes],
            moduleDepsConfig =
              digestChunks
                ( map BS8.pack (HackageCabal.fileInfoExtensions fileInfo)
                    <> [BS8.pack (show (HackageCabal.fileInfoLanguage fileInfo))]
                    <> [ BS8.pack (show (HackageCabal.fileInfoCppOptions fileInfo, sort dependencies))
                       | cppEnabled
                       ]
                ),
            moduleDepsIncludes = includes,
            moduleDepsVersions =
              if cppEnabled then M.restrictKeys versions (S.fromList dependencies) else M.empty
          }
  pure (ParsedInterfaceFile path modu sourceLines parseDiagnostics cppDiagnostics extensions source deps)
  where
    path = HackageCabal.fileInfoPath fileInfo

    orElse (Just value) _ = Just value
    orElse Nothing fallback = fallback

    applyCppSetting enabled setting =
      case setting of
        EnableExtension CPP -> True
        DisableExtension CPP -> False
        _ -> enabled

-- | Preprocess one module and report, besides the output and the
-- diagnostics, every package header it included.
preprocessInterfaceSource :: FilePath -> DependencyVersions -> HackageCabal.FileInfo -> Text -> IO (Text, [Aeson.Value], Map FilePath Text)
preprocessInterfaceSource packageRoot versions fileInfo source = do
  drive M.empty (Cpp.preprocess cppConfig (TE.encodeUtf8 injectedSource))
  where
    path = HackageCabal.fileInfoPath fileInfo
    cppOptions = HackageCabal.fileInfoCppOptions fileInfo
    injectedSource = injectSyntheticCppMacros path cppOptions versions (HackageCabal.fileInfoDependencies fileInfo) source
    cppConfig =
      Cpp.defaultConfig
        { Cpp.configInputFile = path,
          Cpp.configMacros = cppMacrosFromOptions cppOptions
        }

    drive includes step =
      case step of
        Cpp.Done result ->
          pure (Cpp.resultOutput result, map cppDiagnosticValue (Cpp.resultDiagnostics result), includes)
        Cpp.NeedInclude req k -> do
          resolved <- resolveInclude packageRoot (HackageCabal.fileInfoIncludeDirs fileInfo) path req
          case resolved of
            -- A header the package ships is an input of this module: record
            -- it so that editing the header rebuilds every module that reads
            -- it. A system header and a synthesized compiler header are not:
            -- they belong to the environment, which the package identity
            -- covers.
            IncludeFromFile file content
              | IncludeLocalHeader <- includeForm req ->
                  drive (M.insert (normalise (makeRelative packageRoot file)) (digestChunks [content]) includes) (k (Just content))
            IncludeFromFile _ content -> drive includes (k (Just content))
            IncludeFromCompiler content -> drive includes (k (Just content))
            IncludeMissing -> drive includes (k Nothing)

-- | Whether an include was written as @#include "header.h"@, which names a
-- header of the package, or as @#include <header.h>@, which names one of the
-- environment.
data IncludeForm = IncludeLocalHeader | IncludeSystemHeader

includeForm :: Cpp.IncludeRequest -> IncludeForm
includeForm req =
  case Cpp.includeKind req of
    Cpp.IncludeLocal -> IncludeLocalHeader
    Cpp.IncludeSystem -> IncludeSystemHeader

-- | Where the content of an @#include@ came from.
data ResolvedInclude
  = IncludeFromFile !FilePath !BS.ByteString
  | IncludeFromCompiler !BS.ByteString
  | IncludeMissing

resolveInclude :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> IO ResolvedInclude
resolveInclude packageRoot includeDirs currentFile req =
  findFirst (includeCandidates packageRoot includeDirs currentFile req)
  where
    findFirst [] =
      pure (maybe IncludeMissing (IncludeFromCompiler . TE.encodeUtf8) (compilerCppHeader (normalise (Cpp.includePath req))))
    findFirst (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then IncludeFromFile candidate <$> BS.readFile candidate
        else findFirst rest

includeCandidates :: FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> [FilePath]
includeCandidates packageRoot includeDirs currentFile req =
  map normalise $
    nub
      [ dir </> Cpp.includePath req
      | dir <- searchDirs
      ]
  where
    includeDir = takeDirectory (Cpp.includeFrom req)
    sourceRelDir = takeDirectory (makeRelative packageRoot currentFile)
    packageAncestors = ancestorDirs sourceRelDir
    localRoots =
      [ takeDirectory currentFile,
        packageRoot </> sourceRelDir,
        packageRoot </> includeDir
      ]
    systemRoots =
      includeDirs
        <> [ packageRoot </> "include",
             packageRoot </> "includes",
             packageRoot </> "cbits",
             packageRoot
           ]
    searchDirs =
      case Cpp.includeKind req of
        Cpp.IncludeLocal -> localRoots <> map (packageRoot </>) packageAncestors <> systemRoots
        Cpp.IncludeSystem -> systemRoots <> localRoots <> map (packageRoot </>) packageAncestors

ancestorDirs :: FilePath -> [FilePath]
ancestorDirs path =
  case filter (not . null) (splitDirectories path) of
    [] -> []
    parts ->
      [ foldl (</>) "." (take n parts)
      | n <- [length parts, length parts - 1 .. 1]
      ]

normalizeSource :: FilePath -> Text -> Text
normalizeSource path source =
  let withoutBom = T.dropWhile (== '\xfeff') source
   in if takeExtension path == ".lhs"
        then T.unlines (unlitBird (T.lines withoutBom))
        else withoutBom

unlitBird :: [Text] -> [Text]
unlitBird =
  map $ \line ->
    case T.uncons line of
      Just ('>', rest) -> rest
      _ -> ""
