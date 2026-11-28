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
import Aihc.Hackage.Cpp (DependencyVersions, cppMacrosFromOptions, injectSyntheticCppMacros)
import Aihc.Hackage.Release (BootLibrary (..), emulatedGhc, lookupBootLibrary)
import Aihc.PackagePlan.Diagnostic (Diagnostic (..), DiagnosticSourceMap, diagnosticSourceMap)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (..),
    ImportDecl (..),
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
import System.Environment (lookupEnv)
import System.FilePath (addTrailingPathSeparator, isRelative, makeRelative, normalise, splitDirectories, splitSearchPath, takeDirectory, takeExtension, (</>))

-- | One loaded source file. 'parsedFileSource' is what the parser saw, so
-- offsets in the module's spans index into it directly.
data ParsedInterfaceFile = ParsedInterfaceFile
  { parsedFilePath :: !FilePath,
    parsedFileModule :: Module,
    -- | The lines of the file by the path and line a span names. Lazy: a
    -- consumer that renders no excerpt never builds it.
    parsedFileSourceLines :: DiagnosticSourceMap,
    parsedFileParseDiagnostics :: [Diagnostic],
    parsedFileCppDiagnostics :: [Diagnostic],
    parsedFileExtensions :: [Extension],
    parsedFileSource :: !Text,
    parsedFileDeps :: !ModuleDeps
  }

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
    -- | Each resolved package header, with its relative path and content digest.
    -- This includes dependency headers and both CPP include forms.
    -- Compiler headers belong to the environment identity.
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

parseInterfaceFile :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> IO ParsedInterfaceFile
parseInterfaceFile headerDir packageRoot versions fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  parseInterfaceBytes headerDir packageRoot versions fileInfo bytes

-- | Parse the same source bytes that the caller uses for its input hash.
parseInterfaceBytes :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> BS.ByteString -> IO ParsedInterfaceFile
parseInterfaceBytes headerDir packageRoot versions fileInfo bytes = do
  let normalized = relativeLinePragmas packageRoot (normalizeSource path (TE.decodeUtf8With lenientDecode bytes))
      -- The extensions of the source as it stands, which decide whether CPP
      -- runs on it. A module turns CPP off with @{-# LANGUAGE NoCPP #-}@ the
      -- same way it turns off any other extension the cabal file enabled.
      normalizedExtensions = extensionsOf normalized
      cppEnabled = CPP `elem` normalizedExtensions
  (source, cppDiagnostics, includes) <-
    if cppEnabled
      then preprocessInterfaceSource headerDir packageRoot versions fileInfo normalized
      else pure (normalized, [], M.empty)
  -- The preprocessor may add or remove pragmas, so the extensions are
  -- computed once more from its output. Without it the source is unchanged
  -- and so is the set.
  let extensions = if cppEnabled then extensionsOf source else normalizedExtensions
      cfg = defaultConfig {parserSourceName = path, parserExtensions = extensions}
      (parseErrs, parsedModu) = parseModule cfg source
      modu = standinImportPackages parsedModu
      parseDiagnostics = map (uncurry (ParseDiagnostic path)) parseErrs
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
  pure
    ParsedInterfaceFile
      { parsedFilePath = HackageCabal.fileInfoPath fileInfo,
        parsedFileModule = modu,
        parsedFileSourceLines = sourceLines,
        parsedFileParseDiagnostics = parseDiagnostics,
        parsedFileCppDiagnostics = cppDiagnostics,
        parsedFileExtensions = extensions,
        parsedFileSource = source,
        parsedFileDeps = deps
      }
  where
    -- The name of the file in spans and diagnostics. It does not depend on
    -- the directory of the package.
    path = HackageCabal.fileInfoName fileInfo

    cabalExtSettings = mapMaybe (parseExtensionSettingName . T.pack) (HackageCabal.fileInfoExtensions fileInfo)

    -- The extensions one text compiles under: the cabal file's settings with
    -- the pragmas of that text applied to them.
    extensionsOf text =
      let headerPragmas = readModuleHeaderPragmas text
          language =
            headerLanguageEdition headerPragmas
              `orElse` (HackageCabal.fileInfoLanguage fileInfo >>= parseLanguageEdition . T.pack)
       in effectiveExtensions (fromMaybe Haskell98Edition language) (cabalExtSettings <> headerExtensionSettings headerPragmas)

    orElse (Just value) _ = Just value
    orElse Nothing fallback = fallback

-- | Preprocess one module and report, besides the output and the
-- diagnostics, every package header it included.
preprocessInterfaceSource :: FilePath -> FilePath -> DependencyVersions -> HackageCabal.FileInfo -> Text -> IO (Text, [Diagnostic], Map FilePath Text)
preprocessInterfaceSource headerDir packageRoot versions fileInfo source = do
  environmentDirs <- cpathDirectories
  drive environmentDirs M.empty (Cpp.preprocess cppConfig (TE.encodeUtf8 injectedSource))
  where
    -- The file is read from its path. CPP names it by its name, in its
    -- @#line@ directives and in @__FILE__@.
    path = HackageCabal.fileInfoPath fileInfo
    name = HackageCabal.fileInfoName fileInfo
    cppOptions = HackageCabal.fileInfoCppOptions fileInfo
    injectedSource = injectSyntheticCppMacros name cppOptions versions (HackageCabal.fileInfoDependencies fileInfo) source
    cppConfig =
      Cpp.defaultConfig
        { Cpp.configInputFile = name,
          Cpp.configMacros = M.mapKeys TE.encodeUtf8 (M.map TE.encodeUtf8 (cppMacrosFromOptions cppOptions))
        }

    drive environmentDirs includes step =
      case step of
        Cpp.Done result ->
          pure (TE.decodeUtf8With lenientDecode (Cpp.resultOutput result), map CppDiagnostic (Cpp.resultDiagnostics result), includes)
        Cpp.NeedInclude req k -> do
          resolved <- resolveInclude headerDir environmentDirs packageRoot (HackageCabal.fileInfoIncludeDirs fileInfo) path req
          case resolved of
            -- A package can include a dependency header with either CPP form.
            IncludeFromFile file content ->
              let header = includeName packageRoot req file
               in drive environmentDirs (M.insert header (digestChunks [content]) includes) (k (Just (namedInclude req header content)))
            IncludeFromCompiler content -> drive environmentDirs includes (k (Just (namedInclude req (normalise (Cpp.includePath req)) content)))
            IncludeMissing -> drive environmentDirs includes (k Nothing)

-- | The name of an included file in the dependencies of a module. A file of
-- the package is named by its path relative to the package root. Any other
-- file, for example a header of a dependency, is named by its path relative
-- to the include directory it was found in, which is the path in the
-- @#include@ line. Thus the name does not depend on the directory of the
-- package or of its dependencies.
includeName :: FilePath -> Cpp.IncludeRequest -> FilePath -> FilePath
includeName packageRoot req file
  | isRelative relative = normalise relative
  | otherwise = normalise (Cpp.includePath req)
  where
    relative = makeRelative packageRoot file

-- | The content of an included file, with a @#line@ directive that gives
-- the file its name when CPP would name it differently. CPP names a file
-- of an @#include "..."@ line by the directory of the including file, also
-- when the file is in an include directory. The directive makes @__FILE__@
-- and the source spans of the file name the file that was really read.
namedInclude :: Cpp.IncludeRequest -> FilePath -> BS.ByteString -> BS.ByteString
namedInclude req header content
  | normalise cppName == header = content
  | otherwise = BS8.pack ("#line 1 " <> show header <> "\n") <> content
  where
    cppName =
      case Cpp.includeKind req of
        Cpp.IncludeLocal -> takeDirectory (Cpp.includeFrom req) </> Cpp.includePath req
        Cpp.IncludeSystem -> Cpp.includePath req

-- | Make the file of each @LINE@ pragma relative to the package root when it
-- is in the package. A preprocessor such as hsc2hs, alex or happy names its
-- input file in these pragmas, and aihc gives it the input by its full path.
-- The pragmas name the source file of each span, so without this change the
-- spans, and the program text that shows them, depend on the directory of
-- the package.
relativeLinePragmas :: FilePath -> Text -> Text
relativeLinePragmas packageRoot source
  | not (T.pack "{-# LINE " `T.isInfixOf` source) = source
  | otherwise = T.unlines (map relativeLine (T.lines source))
  where
    root = T.pack (addTrailingPathSeparator (normalise packageRoot))
    relativeLine line =
      case T.breakOn (T.pack "\"") line of
        (before, quoted)
          | T.pack "{-# LINE " `T.isPrefixOf` T.stripStart before,
            Just file <- T.stripPrefix (T.pack "\"") quoted >>= T.stripPrefix root ->
              before <> T.pack "\"" <> file
        _ -> line

-- | The include directories of the @CPATH@ environment variable. The C
-- compiler searches them after the directories of its @-I@ options, for
-- each language. GHC runs the C preprocessor over a Haskell source, so the
-- headers of these directories are available to a Haskell source too. An
-- empty item names the current directory in the C compiler, but a package
-- build does not depend on the current directory, so it is not kept.
cpathDirectories :: IO [FilePath]
cpathDirectories = do
  value <- lookupEnv "CPATH"
  pure [directory | directory <- maybe [] splitSearchPath value, not (null directory)]

-- | Where the content of an @#include@ came from.
data ResolvedInclude
  = IncludeFromFile !FilePath !BS.ByteString
  | IncludeFromCompiler !BS.ByteString
  | IncludeMissing

resolveInclude :: FilePath -> [FilePath] -> FilePath -> [FilePath] -> FilePath -> Cpp.IncludeRequest -> IO ResolvedInclude
resolveInclude headerDir environmentDirs packageRoot includeDirs currentFile req =
  findFirst (includeCandidates packageRoot includeDirs currentFile req)
  where
    -- A header of the package comes first.  The headers of the compiler
    -- come next, from the directory that the C compiles also read. The
    -- directories of @CPATH@ answer the rest, as they do in the C compiler.
    compilerHeader = headerDir </> normalise (Cpp.includePath req)
    findFirst [] = do
      exists <- doesFileExist compilerHeader
      if exists
        then IncludeFromCompiler <$> BS.readFile compilerHeader
        else findEnvironment environmentDirs
    findFirst (candidate : rest) = do
      exists <- doesFileExist candidate
      if exists
        then IncludeFromFile candidate <$> BS.readFile candidate
        else findFirst rest
    findEnvironment [] = pure IncludeMissing
    findEnvironment (directory : rest) = do
      let candidate = normalise (directory </> Cpp.includePath req)
      exists <- doesFileExist candidate
      if exists
        then IncludeFromFile candidate <$> BS.readFile candidate
        else findEnvironment rest

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
        then T.unlines (unlit (T.lines withoutBom))
        else withoutBom

-- | Remove the literate markup of a source, as GHC's unlit does. The code
-- of a literate source is in bird-track lines (@> code@) and in the lines
-- between a @\\begin{code}@ line and a @\\end{code}@ line. A line that
-- starts with @#@ stays as it is, because CPP reads it after this step.
-- Each other line becomes empty, so the line numbers do not change.
unlit :: [Text] -> [Text]
unlit = go False
  where
    go _ [] = []
    go inCode (line : rest)
      | inCode && "\\end{code}" `T.isPrefixOf` line = "" : go False rest
      | inCode = line : go True rest
      | "\\begin{code}" `T.isPrefixOf` line = "" : go True rest
      | otherwise = birdLine line : go False rest
    birdLine line =
      case T.uncons line of
        Just ('>', code) -> code
        Just ('#', _) -> line
        _ -> ""

-- | Change the package of each package-qualified import of a boot library
-- to the standin of that library. A @build-depends@ entry on a boot
-- library names its standin in the plan, and an import such as
-- @import "template-haskell" Language.Haskell.TH@ must do the same.
standinImportPackages :: Module -> Module
standinImportPackages modu =
  modu {moduleImports = map standinImport (moduleImports modu)}
  where
    standinImport importDecl =
      importDecl {importDeclPackage = standinPackage <$> importDeclPackage importDecl}
    standinPackage name =
      maybe name (T.pack . bootLibraryStandin) (lookupBootLibrary (T.unpack name) emulatedGhc)
