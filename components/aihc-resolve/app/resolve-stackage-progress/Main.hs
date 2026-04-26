{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Aihc.Cpp (Config (..), IncludeKind (..), IncludeRequest (..), Result (..), Step (..), preprocess)
import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Cpp (cppMacrosFromOptions, injectSyntheticCppMacros, minVersionMacroNamesFromDeps)
import Aihc.Hackage.Download (DownloadOptions (..), defaultDownloadOptions, downloadPackageWithOptions)
import Aihc.Hackage.Stackage (loadStackageSnapshot)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util (chooseBestCabalFile, findCabalFiles, readTextFileLenient)
import Aihc.Hackage.VersionResolver (getLatestVersion)
import Aihc.Parser (ParserConfig (..), parseModule)
import Aihc.Parser qualified as Parser
import Aihc.Parser.Lex (readModuleHeaderPragmas)
import Aihc.Parser.Syntax
  ( Extension (CPP),
    ExtensionSetting (..),
    LanguageEdition (..),
    Module,
    ModuleHeaderPragmas (..),
    NameType (..),
    SourceSpan (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    mkQualifiedName,
    mkUnqualifiedName,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Aihc.Resolve (ModuleExports, ResolveError (..), ResolveResult (..), ResolvedName (..), Scope (..), extractInterface, resolveWithDeps)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (mplus)
import Control.Monad qualified
import Data.ByteString qualified as BS
import Data.Char (isAlphaNum, isUpper, toLower)
import Data.List (nub, partition, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Maybe qualified
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import GHC.Conc (getNumProcessors)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath (makeRelative, normalise, takeDirectory, takeExtension, (</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStrLn, stderr, stdout)

-- ---------------------------------------------------------------------------
-- CLI
-- ---------------------------------------------------------------------------

data Options = Options
  { optSnapshot :: String,
    optJobs :: Int,
    optOffline :: Bool,
    optTopFailures :: Int
  }

defaultOptions :: Options
defaultOptions =
  Options
    { optSnapshot = "lts-24.33",
      optJobs = 0,
      optOffline = False,
      optTopFailures = 10
    }

parseOptions :: [String] -> IO Options
parseOptions = go defaultOptions
  where
    go opts [] = pure opts
    go opts ("--snapshot" : s : rest) = go opts {optSnapshot = s} rest
    go opts ("--jobs" : n : rest) = go opts {optJobs = read n} rest
    go opts ("--offline" : rest) = go opts {optOffline = True} rest
    go opts ("--top" : n : rest) = go opts {optTopFailures = read n} rest
    go _ (flag : _) = do
      hPutStrLn stderr ("Unknown flag: " ++ flag)
      hPutStrLn stderr "Usage: resolve-stackage-progress [--snapshot SNAP] [--jobs N] [--offline] [--top N]"
      exitFailure

-- ---------------------------------------------------------------------------
-- Package status
-- ---------------------------------------------------------------------------

data PackageStatus
  = PkgSuccess !ModuleExports
  | PkgFailed !String
  | PkgSkipped

-- ---------------------------------------------------------------------------
-- Phase 1: download all packages + collect dep info
-- ---------------------------------------------------------------------------

data PackageInfo = PackageInfo
  { piSrcDir :: FilePath,
    piFiles :: [HC.FileInfo],
    piSnapshotDeps :: [Text]
  }

collectPackageInfo :: Bool -> PackageSpec -> Set Text -> IO (Text, PackageInfo)
collectPackageInfo offline spec snapshotNames = do
  version <-
    if pkgVersion spec == "installed"
      then do
        v <- getLatestVersion Nothing (pkgName spec)
        case v of
          Left err -> ioError (userError ("version resolve failed for " ++ pkgName spec ++ ": " ++ err))
          Right ver -> pure ver
      else pure (pkgVersion spec)
  let spec' = spec {pkgVersion = version}
  srcDir <-
    downloadPackageWithOptions
      defaultDownloadOptions
        { downloadVerbose = False,
          downloadAllowNetwork = not offline
        }
      spec'
  cabalFiles <- findCabalFiles srcDir
  (rawFiles, snapshotDeps) <- case cabalFiles of
    [] -> pure ([], [])
    [cf] -> extractFromCabal cf srcDir snapshotNames
    cfs -> extractFromCabal (chooseBestCabalFile srcDir cfs) srcDir snapshotNames
  pure (T.pack (pkgName spec), PackageInfo srcDir rawFiles snapshotDeps)

extractFromCabal :: FilePath -> FilePath -> Set Text -> IO ([HC.FileInfo], [Text])
extractFromCabal cabalFile srcDir snapshotNames = do
  cabalBytes <- BS.readFile cabalFile
  let (_, parseResult) = runParseResult (parseGenericPackageDescription cabalBytes)
  case parseResult of
    Left _ -> pure ([], [])
    Right gpd -> do
      rawFiles <- HC.collectComponentFiles gpd (takeDirectory cabalFile)
      let allDeps = concatMap HC.fileInfoDependencies rawFiles
          snapDeps = filter (`Set.member` snapshotNames) allDeps
      -- Restrict files to those under srcDir (sanity check)
      let validFiles = filter (\f -> srcDir `isPrefixOf` HC.fileInfoPath f) rawFiles
      pure (validFiles, snapDeps)
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- ---------------------------------------------------------------------------
-- Topological ordering (Kahn's BFS producing layers)
-- ---------------------------------------------------------------------------

kahnLayers :: Map Text [Text] -> [[Text]]
kahnLayers depGraph =
  let nodeSet = Map.keysSet depGraph
      -- Filter each package's deps to only those that are also nodes in the graph.
      filteredDeps = Map.map (filter (`Set.member` nodeSet)) depGraph
      -- in-degree[X] = number of X's own deps still unprocessed (starts as dep count).
      inDegree = Map.map length filteredDeps
      -- Reverse graph: dep -> [packages that depend on it], for efficient decrement.
      revGraph =
        Map.foldlWithKey'
          (\acc pkg deps -> foldl' (\a d -> Map.insertWith (++) d [pkg] a) acc deps)
          Map.empty
          filteredDeps
      go :: Map Text Int -> [[Text]]
      go indegrees
        | Map.null indegrees = []
        | otherwise =
            let zeros = [n | (n, d) <- Map.toList indegrees, d == 0]
             in case zeros of
                  [] -> [Map.keys indegrees] -- cycle: emit remaining as final layer
                  _ ->
                    let indegrees' =
                          foldl'
                            ( \acc n ->
                                foldl'
                                  (flip (Map.adjust (subtract 1)))
                                  (Map.delete n acc)
                                  (Map.findWithDefault [] n revGraph)
                            )
                            indegrees
                            zeros
                     in zeros : go indegrees'
   in go inDegree

-- ---------------------------------------------------------------------------
-- Phase 2: resolve packages layer by layer
-- ---------------------------------------------------------------------------

hasFailedDep :: Map Text PackageStatus -> Map Text [Text] -> Text -> Bool
hasFailedDep completed depGraph pkg =
  any isFailure [Map.findWithDefault PkgSkipped dep completed | dep <- deps]
  where
    deps = Map.findWithDefault [] pkg depGraph
    isFailure (PkgFailed _) = True
    isFailure PkgSkipped = True
    isFailure _ = False

gatherDepExports :: Text -> Map Text PackageStatus -> Map Text [Text] -> ModuleExports
gatherDepExports pkg completed depGraph =
  foldl'
    Map.union
    Map.empty
    [ iface
    | dep <- Map.findWithDefault [] pkg depGraph,
      Just (PkgSuccess iface) <- [Map.lookup dep completed]
    ]

processLayer ::
  Options ->
  [Text] ->
  Map Text PackageInfo ->
  Map Text PackageStatus ->
  Map Text [Text] ->
  IO (Map Text PackageStatus)
processLayer opts layer infos completed depGraph = do
  let (toSkip, toProcess) = partition (hasFailedDep completed depGraph) layer
  let withSkips = foldl' (\m p -> Map.insert p PkgSkipped m) completed toSkip
  if null toProcess
    then pure withSkips
    else do
      let workerCount = max 1 (min (optJobs opts) (length toProcess))
      queue <- newChan
      mapM_ (writeChan queue . Just) toProcess
      replicateM_ workerCount (writeChan queue Nothing)
      resultVar <- newMVar withSkips
      let worker = do
            next <- readChan queue
            case next of
              Nothing -> pure ()
              Just pkg -> do
                current <- readMVar resultVar
                let depExports = gatherDepExports pkg current depGraph
                    info = Map.findWithDefault (PackageInfo "" [] []) pkg infos
                status <- resolveOnePackage (optOffline opts) pkg info depExports
                modifyMVar_ resultVar (pure . Map.insert pkg status)
                worker
      replicateConcurrently_ workerCount worker
      readMVar resultVar

processLayers ::
  Options ->
  [[Text]] ->
  Map Text PackageInfo ->
  Map Text [Text] ->
  Map Text PackageStatus ->
  IO (Map Text PackageStatus)
processLayers opts layers infos depGraph = go layers
  where
    go [] acc = pure acc
    go (layer : rest) acc = do
      acc' <- processLayer opts layer infos acc depGraph
      go rest acc'

-- ---------------------------------------------------------------------------
-- Core resolution
-- ---------------------------------------------------------------------------

resolveOnePackage :: Bool -> Text -> PackageInfo -> ModuleExports -> IO PackageStatus
resolveOnePackage offline pkg info depExports = do
  result <- try (resolveOnePackageOrThrow offline pkg info depExports)
  pure $ case result of
    Left (e :: SomeException) -> PkgFailed (displayException e)
    Right status -> status

resolveOnePackageOrThrow :: Bool -> Text -> PackageInfo -> ModuleExports -> IO PackageStatus
resolveOnePackageOrThrow _offline _pkg info depExports = do
  let rawFiles = piFiles info
  if null rawFiles
    then pure (PkgSuccess depExports)
    else do
      parseResults <- mapM (parseFileInfo (piSrcDir info)) rawFiles
      let (errs, pairs) = partitionEithers parseResults
      case errs of
        (e : _) -> pure (PkgFailed e)
        [] -> do
          let modules = map fst pairs
              srcTexts = Map.fromList [(path, src) | (_, (path, src)) <- pairs]
              resolveResult = resolveWithDeps depExports modules
              !annCount = sum (map (length . snd) (resolvedAnnotations resolveResult))
          _ <- evaluate annCount
          case resolveErrors resolveResult of
            [] -> pure (PkgSuccess (extractInterface resolveResult))
            resolveErrs -> pure (PkgFailed (unlines (map (renderResolveError srcTexts) resolveErrs)))

renderResolveError :: Map FilePath Text -> ResolveError -> String
renderResolveError srcTexts (ResolveResolutionError errSpan _ _ msg) =
  renderSpanHeader errSpan ++ renderSourceSnippet srcTexts errSpan ++ "  " ++ msg ++ "."
renderResolveError _ (ResolveNotImplemented msg) = "not implemented: " ++ msg

renderSpanHeader :: SourceSpan -> String
renderSpanHeader NoSourceSpan = "<unknown location>\n"
renderSpanHeader ss =
  sourceSpanSourceName ss ++ ":" ++ show (sourceSpanStartLine ss) ++ ":" ++ show (sourceSpanStartCol ss) ++ ":\n"

-- | Extract the source line containing 'offset' by scanning byte-by-byte.
-- Mirrors Aihc.Parser.extractSourceLineByOffset / renderSourceReference.
-- The line/column stored in 'SourceSpan' may be wrong after CPP '#line' pragmas,
-- so we derive the actual source line from the byte offset instead.
extractLineAtOffset :: BS.ByteString -> Int -> String
extractLineAtOffset bytes offset =
  let anchor = max 0 (min (BS.length bytes) offset)
      start = scanBack anchor
      end = scanFwd anchor
   in T.unpack (TE.decodeUtf8 (BS.take (end - start) (BS.drop start bytes)))
  where
    scanBack i
      | i <= 0 = 0
      | BS.index bytes (i - 1) == 10 = i -- '\n'
      | otherwise = scanBack (i - 1)
    scanFwd i
      | i >= BS.length bytes = BS.length bytes
      | BS.index bytes i == 10 = i
      | otherwise = scanFwd (i + 1)

renderSourceSnippet :: Map FilePath Text -> SourceSpan -> String
renderSourceSnippet _ NoSourceSpan = ""
renderSourceSnippet srcTexts ss =
  case Map.lookup (sourceSpanSourceName ss) srcTexts of
    Nothing -> ""
    Just src ->
      let bytes = TE.encodeUtf8 src
          startLine = sourceSpanStartLine ss
          startCol = sourceSpanStartCol ss
          endLine = sourceSpanEndLine ss
          endCol = sourceSpanEndCol ss
          lineText = extractLineAtOffset bytes (sourceSpanStartOffset ss)
          lineNumStr = show startLine
          pad = replicate (length lineNumStr) ' '
          caretStart = startCol - 1
          caretLen
            | endLine == startLine = max 1 (endCol - startCol)
            | otherwise = max 1 (length lineText - caretStart)
          carets = replicate caretLen '^'
          -- Carets sit directly under the token: indent = line-number gutter width + caretStart.
          caretIndent = length lineNumStr + 3 + caretStart
       in pad
            ++ " |\n"
            ++ lineNumStr
            ++ " | "
            ++ lineText
            ++ "\n"
            ++ replicate caretIndent ' '
            ++ carets
            ++ "\n"

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (Left a : rest) = let (as, bs) = partitionEithers rest in (a : as, bs)
partitionEithers (Right b : rest) = let (as, bs) = partitionEithers rest in (as, b : bs)

-- | Strip BOM and unliterate .lhs files (bird-track and LaTeX styles).
-- Must be applied before CPP and before parsing, matching CppSupport.normalizeSourceForParser.
normalizeSource :: FilePath -> Text -> Text
normalizeSource filePath src
  | map toLower (takeExtension filePath) /= ".lhs" = stripBom src
  | otherwise =
      let ls = T.lines (stripBom src)
       in if any (\l -> T.strip l == "\\begin{code}") ls
            then T.unlines (unlitLatex False ls)
            else T.unlines (map unlitBird ls)
  where
    stripBom t = Data.Maybe.fromMaybe t (T.stripPrefix "\xfeff" t)
    unlitBird line = case T.stripPrefix ">" line of
      Just rest -> Data.Maybe.fromMaybe rest (T.stripPrefix " " rest)
      Nothing -> ""
    unlitLatex _ [] = []
    unlitLatex inCode (l : ls)
      | T.strip l == "\\begin{code}" = "" : unlitLatex True ls
      | T.strip l == "\\end{code}" = "" : unlitLatex False ls
      | inCode = l : unlitLatex inCode ls
      | otherwise = "" : unlitLatex inCode ls

-- Returns (Module, (filePath, processedSource)) on success.
parseFileInfo :: FilePath -> HC.FileInfo -> IO (Either String (Module, (FilePath, Text)))
parseFileInfo pkgRoot fi = do
  rawSrc <- readTextFileLenient path
  -- Strip BOM and unliterate .lhs before anything else.
  let normalized = normalizeSource path rawSrc
  -- Preprocess CPP if enabled in the cabal file or in the file's own LANGUAGE pragmas.
  let cabalExtSettings = mapMaybe (parseExtensionSettingName . T.pack) (HC.fileInfoExtensions fi)
      isCppEnable (EnableExtension CPP) = True
      isCppEnable _ = False
      cppEnabledGlobally = any isCppEnable cabalExtSettings
      cppEnabledInFile = any isCppEnable (headerExtensionSettings (readModuleHeaderPragmas normalized))
  src <-
    if cppEnabledGlobally || cppEnabledInFile
      then runCpp normalized
      else pure normalized
  -- Read in-file {-# LANGUAGE ... #-} pragmas and merge with cabal-file extensions.
  let headerPragmas = readModuleHeaderPragmas src
      allExtSettings = cabalExtSettings ++ headerExtensionSettings headerPragmas
      lang =
        headerLanguageEdition headerPragmas
          `mplus` (HC.fileInfoLanguage fi >>= parseLanguageEdition . T.pack)
      exts = effectiveExtensions (Data.Maybe.fromMaybe Haskell98Edition lang) allExtSettings
      cfg = Parser.defaultConfig {parserSourceName = path, parserExtensions = exts}
      (parseErrs, modu) = parseModule cfg src
  pure $
    if null parseErrs
      then Right (modu, (path, src))
      else Left (T.unpack (T.unwords (map snd parseErrs)))
  where
    path = HC.fileInfoPath fi
    runCpp normalizedSrc = do
      let cppOpts = HC.fileInfoCppOptions fi
          minMacros = minVersionMacroNamesFromDeps (HC.fileInfoDependencies fi)
          injected = injectSyntheticCppMacros cppOpts minMacros normalizedSrc
          cppCfg =
            Cpp.defaultConfig
              { configInputFile = path,
                configMacros = cppMacrosFromOptions cppOpts
              }
      driveIO (preprocess cppCfg (TE.encodeUtf8 injected))
    driveIO (Done r) = pure (resultOutput r)
    driveIO (NeedInclude req k) = do
      content <- resolveInclude pkgRoot path req
      driveIO (k content)

-- | Resolve a CPP include request by searching candidate paths under the package root.
-- Mirrors HackageSupport.resolveIncludeBestEffort.
resolveInclude :: FilePath -> FilePath -> IncludeRequest -> IO (Maybe BS.ByteString)
resolveInclude pkgRoot currentFile req = do
  let candidates = includeCandidates pkgRoot currentFile req
  findFirst candidates
  where
    findFirst [] = pure Nothing
    findFirst (p : ps) = do
      exists <- doesFileExist p
      if exists then Just <$> BS.readFile p else findFirst ps

includeCandidates :: FilePath -> FilePath -> IncludeRequest -> [FilePath]
includeCandidates pkgRoot currentFile req =
  nub ([normalise (dir </> includePath req) | dir <- searchDirs])
  where
    includeDir = takeDirectory (includeFrom req)
    sourceRelDir = takeDirectory (makeRelative pkgRoot currentFile)
    localRoots =
      [ takeDirectory currentFile,
        pkgRoot </> sourceRelDir,
        pkgRoot </> includeDir
      ]
    systemRoots =
      [ pkgRoot </> "include",
        pkgRoot </> "includes",
        pkgRoot </> "cbits",
        pkgRoot
      ]
    searchDirs = case includeKind req of
      IncludeLocal -> localRoots ++ systemRoots
      IncludeSystem -> systemRoots ++ localRoots

-- ---------------------------------------------------------------------------
-- Reporting
-- ---------------------------------------------------------------------------

reportResults :: Int -> Map Text PackageStatus -> IO ()
reportResults topN results = do
  let allList = Map.toList results
      succeeded = [pkg | (pkg, PkgSuccess _) <- allList]
      failed = [(pkg, msg) | (pkg, PkgFailed msg) <- allList]
      skipped = [pkg | (pkg, PkgSkipped) <- allList]
      total = Map.size results
      successN = length succeeded
      failedN = length failed
      skippedN = length skipped
  putStrLn ""
  putStrLn "Name resolution results:"
  putStrLn $ "  Resolved: " ++ show successN ++ " / " ++ show total ++ " (" ++ show (pct successN total) ++ "%)"
  putStrLn $ "  Failed:   " ++ show failedN ++ " (parse or resolver errors)"
  putStrLn $ "  Skipped:  " ++ show skippedN ++ " (dep had failure)"
  let n = min topN failedN
  Control.Monad.when (n > 0) $ do
    putStrLn ""
    putStrLn $ "Top " ++ show n ++ " failing packages:"
    let sorted = take n (sortOn (length . snd) failed)
    mapM_ printFailure sorted
  if successN == total then exitSuccess else exitFailure
  where
    printFailure (pkg, msg) = do
      putStrLn $ "  " ++ T.unpack pkg ++ ":"
      mapM_ (\l -> putStrLn ("    " ++ l)) (take 5 (lines msg))

pct :: Int -> Int -> Int
pct _ 0 = 100
pct n total = (n * 100) `div` total

-- ---------------------------------------------------------------------------
-- Progress
-- ---------------------------------------------------------------------------

putProgressLine :: Int -> Int -> IO ()
putProgressLine done total = do
  putStr ("\r" ++ show done ++ "/" ++ show total ++ " processed")
  hFlush stdout

-- ---------------------------------------------------------------------------
-- Boot package shims
-- ---------------------------------------------------------------------------

-- | Build a ResolvedTopLevel name for a boot shim entry.
bootResolve :: Text -> Text -> ResolvedName
bootResolve modName n =
  ResolvedTopLevel (mkQualifiedName (mkUnqualifiedName nt n) (Just modName))
  where
    nt = case T.uncons n of
      Nothing -> NameConId
      Just (c, _)
        | c == ':' -> NameConSym
        | not (isAlphaNum c) && c /= '_' && c /= '\'' -> NameVarSym
        | isUpper c -> NameConId
        | otherwise -> NameVarId

bootScope :: Text -> [Text] -> [Text] -> Scope
bootScope modName termNames typeNames =
  Scope
    { scopeTerms = Map.fromList [(n, bootResolve modName n) | n <- termNames],
      scopeTypes = Map.fromList [(n, bootResolve modName n) | n <- typeNames],
      scopeQualifiedModules = Map.empty
    }

bootPackageExports :: Text -> ModuleExports
bootPackageExports pkg = case pkg of
  "base" -> baseExports
  "ghc-prim" -> ghcPrimExports
  "integer-gmp" -> integerExports
  "integer-simple" -> integerExports
  "ghc-bignum" -> integerExports
  "time" -> timeExports
  "bytestring" -> bytestringExports
  "text" -> textExports
  "containers" -> containersExports
  "array" -> arrayExports
  "deepseq" -> deepseqExports
  "binary" -> binaryExports
  "transformers" -> transformersExports
  "mtl" -> mtlExports
  "stm" -> stmExports
  "unix" -> unixExports
  "Win32" -> win32Exports
  "directory" -> directoryExports
  "filepath" -> filepathExports
  "process" -> processExports
  "network" -> networkExports
  "random" -> randomExports
  "parsec" -> parsecExports
  "pretty" -> prettyExports
  _ -> Map.empty

baseExports :: ModuleExports
baseExports =
  Map.fromList
    [ ("Prelude", preludeScope),
      ("Data.Function", dataFunctionScope),
      ("Data.List", dataListScope),
      ("Data.Maybe", dataMaybeScope),
      ("Data.Char", dataCharScope),
      ("Data.String", dataStringScope),
      ("Data.Bool", dataBoolScope),
      ("Data.Int", dataIntScope),
      ("Data.Word", dataWordScope),
      ("Data.Tuple", dataTupleScope),
      ("Data.Either", dataEitherScope),
      ("Data.Ord", dataOrdScope),
      ("Data.Bits", dataBitsScope),
      ("Data.IORef", dataIoRefScope),
      ("Data.Typeable", dataTypeableScope),
      ("Data.Data", dataDataScope),
      ("Data.Kind", dataKindScope),
      ("Control.Monad", controlMonadScope),
      ("Control.Monad.IO.Class", controlMonadIoClassScope),
      ("Control.Exception", controlExceptionScope),
      ("System.IO", systemIoScope),
      ("System.IO.Error", systemIoErrorScope),
      ("System.Exit", systemExitScope),
      ("System.Environment", systemEnvironmentScope),
      ("GHC.Exts", ghcExtsScope),
      ("GHC.Types", ghcTypesScope),
      ("GHC.Generics", ghcGenericsScope),
      ("GHC.Base", ghcBaseScope),
      ("GHC.Show", ghcShowScope),
      ("GHC.Read", ghcReadScope),
      ("GHC.Enum", ghcEnumScope),
      ("GHC.Num", ghcNumScope),
      ("GHC.Real", ghcRealScope),
      ("GHC.Float", ghcFloatScope),
      ("GHC.IO", ghcIoScope),
      ("GHC.IORef", ghcIoRefScope),
      ("GHC.ST", ghcStScope),
      ("GHC.STRef", ghcStRefScope),
      ("GHC.Arr", ghcArrScope),
      ("GHC.Exception", ghcExceptionScope),
      ("GHC.Conc", ghcConcScope),
      ("GHC.MVar", ghcMVarScope),
      ("GHC.Foreign", ghcForeignScope),
      ("GHC.Stable", ghcStableScope),
      ("GHC.Ptr", ghcPtrScope),
      ("GHC.ForeignPtr", ghcForeignPtrScope),
      ("GHC.Weak", ghcWeakScope),
      ("GHC.Fingerprint", ghcFingerprintScope),
      ("GHC.Word", ghcWordScope),
      ("GHC.Int", ghcIntScope),
      ("GHC.Unicode", ghcUnicodeScope),
      ("GHC.TypeLits", ghcTypeLitsScope),
      ("GHC.TypeNats", ghcTypeNatsScope),
      ("GHC.OverloadedLabels", ghcOverloadedLabelsScope),
      ("GHC.Records", ghcRecordsScope),
      ("Data.Proxy", dataProxyScope),
      ("Data.Coerce", dataCoerceScope),
      ("Data.Type.Equality", dataTypeEqualityScope),
      ("Data.Type.Coercion", dataTypeCoercionScope),
      ("Data.Void", dataVoidScope),
      ("Data.Fixed", dataFixedScope),
      ("Data.Complex", dataComplexScope),
      ("Data.Ratio", dataRatioScope),
      ("Numeric", numericScope),
      ("Text.Read", textReadScope),
      ("Text.Show", textShowScope),
      ("Control.Applicative", controlApplicativeScope),
      ("Control.Arrow", controlArrowScope),
      ("Control.Category", controlCategoryScope),
      ("Control.DeepSeq", controlDeepSeqScope),
      ("System.IO.Unsafe", systemIoUnsafeScope),
      ("Foreign.Storable", foreignStorableScope),
      ("Foreign.Ptr", foreignPtrScope),
      ("Foreign.ForeignPtr", foreignForeignPtrScope),
      ("Foreign.ForeignPtr.Unsafe", foreignForeignPtrUnsafeScope),
      ("Foreign.Marshal.Alloc", foreignMarshalAllocScope),
      ("Foreign.Marshal.Array", foreignMarshalArrayScope),
      ("Foreign.Marshal.Utils", foreignMarshalUtilsScope),
      ("Foreign.Marshal.Error", foreignMarshalErrorScope),
      ("Foreign.C.Types", foreignCTypesScope),
      ("Foreign.C.String", foreignCStringScope),
      ("Foreign.C.Error", foreignCErrorScope),
      ("Foreign", foreignScope),
      ("Data.STRef", dataStRefScope),
      ("Control.ST", controlStScope),
      ("Control.Concurrent", controlConcurrentScope),
      ("Control.Concurrent.MVar", controlMVarScope),
      ("Control.Concurrent.Chan", controlChanScope),
      ("Control.Concurrent.QSem", controlQSemScope),
      ("Control.Concurrent.QSemN", controlQSemNScope),
      ("Control.Concurrent.STM", controlStmScope),
      ("Control.Concurrent.STM.TVar", controlStmTVarScope),
      ("Control.Concurrent.STM.TMVar", controlStmTMVarScope),
      ("Control.Concurrent.STM.TChan", controlStmTChanScope),
      ("Control.Concurrent.STM.TQueue", controlStmTQueueScope),
      ("Control.Concurrent.STM.TBQueue", controlStmTBQueueScope),
      ("Control.Concurrent.Async", controlConcurrentAsyncScope),
      ("Data.Foldable", dataFoldableScope),
      ("Data.Traversable", dataTraversableScope),
      ("Data.Bifunctor", dataBifunctorScope),
      ("Data.Bitraversable", dataBitraversableScope),
      ("Data.Bifoldable", dataBifoldableScope),
      ("Data.Semigroup", dataSemigroupScope),
      ("Data.Monoid", dataMonoidScope),
      ("Data.Functor", dataFunctorScope),
      ("Data.Functor.Identity", dataFunctorIdentityScope),
      ("Data.Functor.Const", dataFunctorConstScope),
      ("Data.Functor.Compose", dataFunctorComposeScope),
      ("Data.Functor.Classes", dataFunctorClassesScope),
      ("Data.Functor.Contravariant", dataFunctorContravariantScope),
      ("Data.List.NonEmpty", dataListNonEmptyScope),
      ("Data.Map", dataMapScope),
      ("Data.Map.Strict", dataMapScope),
      ("Data.Map.Lazy", dataMapScope),
      ("Data.Set", dataSetScope),
      ("Data.Sequence", dataSequenceScope),
      ("Data.HashMap.Strict", dataHashMapScope),
      ("Data.HashMap.Lazy", dataHashMapScope),
      ("Data.HashSet", dataHashSetScope),
      ("Data.IntMap", dataIntMapScope),
      ("Data.IntMap.Strict", dataIntMapScope),
      ("Data.IntMap.Lazy", dataIntMapScope),
      ("Data.IntSet", dataIntSetScope),
      ("Data.Array", dataArrayScope),
      ("Data.Ix", dataIxScope),
      ("Data.Map.Internal", dataMapScope),
      ("Numeric.Natural", naturalScope),
      ("Data.Tuple.Solo", dataTupleSoloScope),
      ("GHC.Tuple", ghcTupleScope),
      ("Text.Printf", textPrintfScope),
      ("Data.Version", dataVersionScope),
      ("System.Posix.Types", systemPosixTypesScope),
      ("Foreign.Concurrent", foreignConcurrentScope),
      ("Data.List.Split", dataListSplitScope),
      ("Data.Unique", dataUniqueScope),
      ("System.Mem.Weak", systemMemWeakScope),
      ("System.Mem", systemMemScope),
      ("System.Mem.StableName", systemMemStableNameScope),
      ("GHC.Desugar", ghcDesugarScope),
      ("System.CPUTime", systemCpuTimeScope)
    ]

preludeScope :: Scope
preludeScope =
  bootScope
    "Prelude"
    -- terms (values and constructors)
    [ "id",
      "const",
      "flip",
      "curry",
      "uncurry",
      "fst",
      "snd",
      "head",
      "tail",
      "last",
      "init",
      "null",
      "length",
      "map",
      "filter",
      "foldr",
      "foldr1",
      "foldl",
      "foldl1",
      "concat",
      "concatMap",
      "reverse",
      "take",
      "drop",
      "takeWhile",
      "dropWhile",
      "span",
      "break",
      "splitAt",
      "lookup",
      "elem",
      "notElem",
      "zip",
      "zipWith",
      "unzip",
      "zip3",
      "zipWith3",
      "unzip3",
      "words",
      "unwords",
      "lines",
      "unlines",
      "show",
      "read",
      "print",
      "readLn",
      "readIO",
      "putStr",
      "putStrLn",
      "putChar",
      "getChar",
      "getLine",
      "getContents",
      "interact",
      "readFile",
      "writeFile",
      "appendFile",
      "error",
      "errorWithoutStackTrace",
      "undefined",
      "seq",
      "return",
      "pure",
      "fail",
      "mapM",
      "mapM_",
      "sequence",
      "sequence_",
      "not",
      "even",
      "odd",
      "gcd",
      "lcm",
      "fromIntegral",
      "toInteger",
      "fromInteger",
      "realToFrac",
      "floor",
      "ceiling",
      "round",
      "truncate",
      "abs",
      "signum",
      "negate",
      "recip",
      "sqrt",
      "logBase",
      "exp",
      "log",
      "pi",
      "sin",
      "cos",
      "tan",
      "asin",
      "acos",
      "atan",
      "atan2",
      "sinh",
      "cosh",
      "tanh",
      "asinh",
      "acosh",
      "atanh",
      "max",
      "min",
      "succ",
      "pred",
      "compare",
      "maximum",
      "minimum",
      "sum",
      "product",
      "and",
      "or",
      "any",
      "all",
      "otherwise",
      "div",
      "mod",
      "quot",
      "rem",
      "divMod",
      "quotRem",
      "ioError",
      "userError",
      "catch",
      "throwIO",
      "evaluate",
      "fmap",
      "traverse",
      "sequenceA",
      "for",
      "forM",
      "foldMap",
      "fold",
      "foldMap'",
      "liftA",
      "liftA2",
      "liftA3",
      "mconcat",
      "mempty",
      "mappend",
      "void",
      "when",
      "unless",
      "forever",
      "guard",
      "toEnum",
      "fromEnum",
      "showsPrec",
      "showList",
      "showParen",
      "showString",
      "shows",
      "readsPrec",
      "readList",
      "readParen",
      "reads",
      "lex",
      "scanl",
      "scanl1",
      "scanr",
      "scanr1",
      "iterate",
      "repeat",
      "replicate",
      "cycle",
      "uncurry",
      "curry",
      "asTypeOf",
      "until",
      "maybe",
      "either",
      "getSolo",
      "MkSolo",
      -- operators
      ".",
      "$",
      "$!",
      "&&",
      "||",
      "++",
      "!!",
      "<$>",
      "<*>",
      ">>=",
      ">>",
      "=<<",
      "<>",
      "<|>",
      "==",
      "/=",
      "<",
      ">",
      "<=",
      ">=",
      "+",
      "-",
      "*",
      "/",
      "^",
      "**",
      "^^",
      "<*",
      "*>",
      "<$",
      -- constructors
      "True",
      "False",
      "Just",
      "Nothing",
      "Left",
      "Right",
      "EQ",
      "LT",
      "GT",
      ":"
    ]
    -- types and classes
    [ "Bool",
      "Char",
      "Int",
      "Integer",
      "Word",
      "Float",
      "Double",
      "String",
      "IO",
      "Maybe",
      "Either",
      "Ordering",
      "FilePath",
      "IOError",
      "IOMode",
      "Eq",
      "Ord",
      "Num",
      "Show",
      "Read",
      "Enum",
      "Bounded",
      "Integral",
      "Fractional",
      "Floating",
      "Real",
      "RealFrac",
      "RealFloat",
      "Functor",
      "Applicative",
      "Monad",
      "MonadFail",
      "Foldable",
      "Traversable",
      "Semigroup",
      "Monoid",
      "ShowS",
      "ReadS",
      "Solo"
    ]

dataFunctionScope :: Scope
dataFunctionScope =
  bootScope
    "Data.Function"
    ["fix", "on", "&", "applyWhen", "flip", "id", "const", "."]
    []

dataListScope :: Scope
dataListScope =
  bootScope
    "Data.List"
    [ -- Re-exported from Prelude (for qualified access as L.xxx)
      "map",
      "filter",
      "head",
      "tail",
      "last",
      "init",
      "null",
      "length",
      "foldl",
      "foldl'",
      "foldr",
      "foldr1",
      "foldl1",
      "concat",
      "concatMap",
      "reverse",
      "take",
      "drop",
      "takeWhile",
      "dropWhile",
      "span",
      "break",
      "splitAt",
      "zip",
      "zipWith",
      "unzip",
      "lookup",
      "elem",
      "notElem",
      "zip3",
      "zipWith3",
      "unzip3",
      "scanl",
      "scanl1",
      "scanr",
      "scanr1",
      "iterate",
      "repeat",
      "replicate",
      "cycle",
      "maximum",
      "minimum",
      "sum",
      "product",
      "and",
      "or",
      "any",
      "all",
      -- Data.List specific
      "sort",
      "sortBy",
      "sortOn",
      "nub",
      "nubBy",
      "group",
      "groupBy",
      "partition",
      "isPrefixOf",
      "isSuffixOf",
      "isInfixOf",
      "isSubsequenceOf",
      "intercalate",
      "intersperse",
      "transpose",
      "subsequences",
      "permutations",
      "foldl'",
      "find",
      "findIndex",
      "findIndices",
      "elemIndex",
      "elemIndices",
      "splitAt",
      "stripPrefix",
      "tails",
      "inits",
      "unfoldr",
      "scanl'",
      "maximumBy",
      "minimumBy",
      "deleteBy",
      "insertBy",
      "unionBy",
      "intersectBy",
      "union",
      "intersect",
      "delete",
      "\\\\",
      "lookup",
      "dropWhileEnd",
      "takeWhileEnd",
      "mapAccumL",
      "mapAccumR",
      "isPrefixOf",
      "isSuffixOf",
      "isInfixOf",
      "stripPrefix",
      "stripSuffix",
      "genericLength",
      "genericTake",
      "genericDrop",
      "genericSplitAt",
      "genericIndex",
      "genericReplicate",
      "singleton"
    ]
    []

dataMaybeScope :: Scope
dataMaybeScope =
  bootScope
    "Data.Maybe"
    [ "fromMaybe",
      "isJust",
      "isNothing",
      "fromJust",
      "catMaybes",
      "mapMaybe",
      "listToMaybe",
      "maybeToList",
      "maybe",
      "Just",
      "Nothing"
    ]
    ["Maybe"]

dataCharScope :: Scope
dataCharScope =
  bootScope
    "Data.Char"
    [ "isAlpha",
      "isAlphaNum",
      "isDigit",
      "isHexDigit",
      "isOctDigit",
      "isLower",
      "isUpper",
      "isSpace",
      "isPunctuation",
      "isLetter",
      "toLower",
      "toUpper",
      "toTitle",
      "digitToInt",
      "intToDigit",
      "ord",
      "chr",
      "isAscii",
      "isLatin1",
      "isPrint",
      "isControl",
      "generalCategory"
    ]
    ["GeneralCategory"]

dataStringScope :: Scope
dataStringScope =
  bootScope
    "Data.String"
    ["fromString", "lines", "words", "unlines", "unwords"]
    ["IsString", "String"]

dataBoolScope :: Scope
dataBoolScope =
  bootScope
    "Data.Bool"
    ["bool", "not", "otherwise", "True", "False"]
    ["Bool"]

dataIntScope :: Scope
dataIntScope =
  bootScope
    "Data.Int"
    []
    ["Int", "Int8", "Int16", "Int32", "Int64"]

dataWordScope :: Scope
dataWordScope =
  bootScope
    "Data.Word"
    []
    ["Word", "Word8", "Word16", "Word32", "Word64"]

dataTupleScope :: Scope
dataTupleScope =
  bootScope
    "Data.Tuple"
    ["fst", "snd", "swap", "curry", "uncurry"]
    []

dataEitherScope :: Scope
dataEitherScope =
  bootScope
    "Data.Either"
    [ "either",
      "fromLeft",
      "fromRight",
      "isLeft",
      "isRight",
      "lefts",
      "rights",
      "partitionEithers",
      "Left",
      "Right"
    ]
    ["Either"]

dataOrdScope :: Scope
dataOrdScope =
  bootScope
    "Data.Ord"
    ["comparing", "Down", "clamp"]
    ["Down", "Ordering"]

dataBitsScope :: Scope
dataBitsScope =
  bootScope
    "Data.Bits"
    [ ".&.",
      ".|.",
      "xor",
      "complement",
      "shift",
      "rotate",
      "shiftL",
      "shiftR",
      "rotateL",
      "rotateR",
      "bit",
      "testBit",
      "setBit",
      "clearBit",
      "complementBit",
      "popCount",
      "zeroBits",
      "bitSize",
      "bitSizeMaybe",
      "isSigned",
      "unsafeShiftL",
      "unsafeShiftR"
    ]
    ["Bits", "FiniteBits"]

dataIoRefScope :: Scope
dataIoRefScope =
  bootScope
    "Data.IORef"
    [ "newIORef",
      "readIORef",
      "writeIORef",
      "modifyIORef",
      "modifyIORef'",
      "atomicModifyIORef",
      "atomicModifyIORef'",
      "atomicWriteIORef",
      "mkWeakIORef"
    ]
    ["IORef"]

dataTypeableScope :: Scope
dataTypeableScope =
  bootScope
    "Data.Typeable"
    [ "typeOf",
      "typeRep",
      "typeOf1",
      "cast",
      "eqT",
      "gcast",
      "showsTypeRep",
      "rnfTypeRep",
      "withTypeable",
      "Proxy",
      "Typeable",
      "TypeRep",
      "TyCon"
    ]
    ["Typeable", "TypeRep", "TyCon"]

dataDataScope :: Scope
dataDataScope =
  bootScope
    "Data.Data"
    [ "gmapT",
      "gmapQ",
      "gmapQl",
      "gmapQr",
      "gmapQi",
      "gmapM",
      "gmapMp",
      "gmapMo",
      "toConstr",
      "fromConstr",
      "dataTypeOf",
      "constrFields",
      "constrIndex",
      "isAlgType",
      "Data",
      "mkDataType",
      "mkConstr",
      "mkIntType",
      "mkFloatType",
      "mkStringType",
      "mkNoRepType",
      "repConstr",
      "cast",
      "gcast"
    ]
    ["Data", "DataType", "Constr", "ConstrRep", "DataRep", "Fixity"]

dataKindScope :: Scope
dataKindScope =
  bootScope
    "Data.Kind"
    []
    ["Type", "Constraint", "FUN"]

controlMonadScope :: Scope
controlMonadScope =
  bootScope
    "Control.Monad"
    [ "void",
      "when",
      "unless",
      "forM",
      "forM_",
      "mapM",
      "mapM_",
      "sequence",
      "sequence_",
      "forever",
      "guard",
      "replicateM",
      "replicateM_",
      "msum",
      "mfilter",
      "foldM",
      "foldM_",
      "zipWithM",
      "zipWithM_",
      "filterM",
      "join",
      ">=>",
      "<=<",
      "ap",
      "liftM",
      "liftM2",
      "liftM3",
      "liftM4",
      "liftM5",
      "foldrM",
      "foldlM",
      "mapAndUnzipM",
      "zipWithM",
      "zipWithM_",
      "MonadPlus",
      "mzero",
      "mplus",
      "return",
      "fail"
    ]
    ["Monad", "MonadPlus"]

controlMonadIoClassScope :: Scope
controlMonadIoClassScope =
  bootScope
    "Control.Monad.IO.Class"
    ["liftIO"]
    ["MonadIO"]

controlExceptionScope :: Scope
controlExceptionScope =
  bootScope
    "Control.Exception"
    [ "catch",
      "catchJust",
      "handle",
      "handleJust",
      "try",
      "tryJust",
      "evaluate",
      "throw",
      "throwIO",
      "throwTo",
      "bracket",
      "bracket_",
      "bracketOnError",
      "finally",
      "onException",
      "SomeException",
      "IOException",
      "Exception",
      "displayException",
      "toException",
      "fromException",
      "assert",
      "mask",
      "mask_",
      "uninterruptibleMask",
      "uninterruptibleMask_",
      "getMaskingState",
      "interruptible",
      "allowInterrupt"
    ]
    ["Exception", "SomeException", "IOException", "MaskingState"]

systemIoScope :: Scope
systemIoScope =
  bootScope
    "System.IO"
    [ "hSetBuffering",
      "hGetBuffering",
      "hFlush",
      "hClose",
      "hPutStr",
      "hPutStrLn",
      "hPutChar",
      "hGetLine",
      "hGetContents",
      "hIsEOF",
      "hSetEncoding",
      "hGetEncoding",
      "hIsTerminalDevice",
      "hSetNewlineMode",
      "stdin",
      "stdout",
      "stderr",
      "withFile",
      "openFile",
      "hReady",
      "hWaitForInput",
      "hSetPosn",
      "hGetPosn",
      "hTell",
      "hSeek",
      "hFileSize",
      "hSetFileSize",
      "hIsOpen",
      "hIsClosed",
      "hIsReadable",
      "hIsWritable",
      "hIsSeekable",
      "hIsBlockDevice",
      "hIsCharDevice",
      "hIsPipe",
      "hPrint",
      "Handle",
      "IOMode",
      "BufferMode",
      "SeekMode",
      "localeEncoding",
      "utf8",
      "utf8_bom",
      "utf16",
      "utf16le",
      "utf16be",
      "utf32",
      "utf32le",
      "utf32be",
      "latin1"
    ]
    ["Handle", "IOMode", "BufferMode", "SeekMode", "HandlePosn", "TextEncoding"]

systemIoErrorScope :: Scope
systemIoErrorScope =
  bootScope
    "System.IO.Error"
    [ "ioError",
      "userError",
      "mkIOError",
      "annotateIOError",
      "isAlreadyExistsError",
      "isDoesNotExistError",
      "isAlreadyInUseError",
      "isFullError",
      "isEOFError",
      "isIllegalOperation",
      "isPermissionError",
      "isUserError",
      "ioeGetErrorType",
      "ioeGetLocation",
      "ioeGetErrorString",
      "ioeGetHandle",
      "ioeGetFileName",
      "catchIOError",
      "tryIOError"
    ]
    ["IOError", "IOErrorType"]

systemExitScope :: Scope
systemExitScope =
  bootScope
    "System.Exit"
    ["exitSuccess", "exitFailure", "exitWith", "die", "ExitSuccess", "ExitFailure"]
    ["ExitCode"]

systemEnvironmentScope :: Scope
systemEnvironmentScope =
  bootScope
    "System.Environment"
    [ "getArgs",
      "getProgName",
      "getExecutablePath",
      "lookupEnv",
      "getEnvironment",
      "withArgs",
      "withProgName"
    ]
    []

ghcExtsScope :: Scope
ghcExtsScope =
  bootScope
    "GHC.Exts"
    [ "inline",
      "lazy",
      "oneShot",
      "seq",
      "pseq",
      "par",
      "build",
      "augment",
      "breakpoint",
      "breakpointCond",
      "coerce",
      "Any",
      "TYPE",
      "SPEC",
      "fromList",
      "toList",
      "IsList",
      "fromListN",
      "Constraint",
      "Type",
      "RuntimeRep",
      "Int#",
      "Word#",
      "Char#",
      "Float#",
      "Double#",
      "Addr#"
    ]
    [ "TYPE",
      "Constraint",
      "Type",
      "RuntimeRep",
      "Levity",
      "LiftedRep",
      "UnliftedRep",
      "TupleRep",
      "SumRep",
      "BoxedRep",
      "IntRep",
      "WordRep",
      "Int8Rep",
      "Int16Rep",
      "Int32Rep",
      "Int64Rep",
      "Word8Rep",
      "Word16Rep",
      "Word32Rep",
      "Word64Rep",
      "FloatRep",
      "DoubleRep",
      "AddrRep",
      "VecRep",
      "VecCount",
      "VecElem",
      "ZeroBitRep",
      "ZeroBitType",
      "IsList",
      "IsString"
    ]

ghcTypesScope :: Scope
ghcTypesScope =
  bootScope
    "GHC.Types"
    [ "True",
      "False",
      "I#",
      "C#",
      "F#",
      "D#",
      "isTrue#",
      "SPEC",
      "coercionToken#"
    ]
    [ "Bool",
      "Char",
      "Int",
      "Word",
      "Float",
      "Double",
      "Ordering",
      "IO",
      "TYPE",
      "Type",
      "Constraint",
      "RuntimeRep",
      "Levity",
      "VecCount",
      "VecElem",
      "LiftedRep",
      "UnliftedRep",
      "ZeroBitRep",
      "ZeroBitType",
      "List"
    ]

ghcGenericsScope :: Scope
ghcGenericsScope =
  bootScope
    "GHC.Generics"
    [ "from",
      "to",
      "from1",
      "to1",
      "M1",
      "K1",
      "U1",
      "V1",
      "Par1",
      "Rec1",
      "L1",
      "R1",
      "unM1",
      "unK1",
      "unPar1",
      "unRec1",
      "selName",
      "selSourceUnpackedness",
      "selSourceStrictness",
      "selDecidedStrictness",
      "conName",
      "conFixity",
      "conIsRecord",
      "datatypeName",
      "moduleName",
      "packageName",
      "isNewtype"
    ]
    [ "Generic",
      "Generic1",
      "Rep",
      "Rep1",
      "D1",
      "C1",
      "S1",
      "K1",
      "M1",
      "U1",
      "V1",
      "Par1",
      "Rec1",
      "L1",
      "R1",
      ":+:",
      ":*:",
      ":.:",
      "MetaData",
      "MetaCons",
      "MetaSel",
      "Associativity",
      "Fixity",
      "FixityI",
      "SourceUnpackedness",
      "SourceStrictness",
      "DecidedStrictness"
    ]

ghcBaseScope :: Scope
ghcBaseScope =
  bootScope
    "GHC.Base"
    [ "map",
      "append",
      "foldr",
      "build",
      "augment",
      "otherwise",
      "id",
      "const",
      ".",
      "flip",
      "$",
      "seq",
      "returnIO",
      "bindIO",
      "thenIO",
      "failIO",
      "void",
      "coerce",
      "maxInt",
      "minInt"
    ]
    ["Type", "Constraint", "Functor", "Applicative", "Monad", "MonadFail", "Alternative", "MonadPlus"]

ghcShowScope :: Scope
ghcShowScope =
  bootScope
    "GHC.Show"
    [ "show",
      "showsPrec",
      "showList",
      "showParen",
      "showString",
      "showChar",
      "shows",
      "showListWith",
      "showSpace",
      "intToDigit"
    ]
    ["Show", "ShowS"]

ghcReadScope :: Scope
ghcReadScope =
  bootScope
    "GHC.Read"
    ["read", "reads", "readMaybe", "readEither", "readsPrec", "readList", "readParen", "lex", "readPrec"]
    ["Read", "ReadS", "ReadPrec", "Lexeme"]

ghcEnumScope :: Scope
ghcEnumScope =
  bootScope
    "GHC.Enum"
    [ "toEnum",
      "fromEnum",
      "succ",
      "pred",
      "enumFrom",
      "enumFromThen",
      "enumFromTo",
      "enumFromThenTo",
      "minBound",
      "maxBound"
    ]
    ["Enum", "Bounded"]

ghcNumScope :: Scope
ghcNumScope =
  bootScope
    "GHC.Num"
    ["+", "-", "*", "negate", "abs", "signum", "fromInteger"]
    ["Num", "Integer"]

ghcRealScope :: Scope
ghcRealScope =
  bootScope
    "GHC.Real"
    [ "toRational",
      "fromRational",
      "fromIntegral",
      "realToFrac",
      "toInteger",
      "quot",
      "rem",
      "div",
      "mod",
      "divMod",
      "quotRem",
      "even",
      "odd",
      "gcd",
      "lcm",
      "^",
      "^^",
      "numerator",
      "denominator",
      "%"
    ]
    ["Real", "Integral", "Fractional", "RealFrac", "Rational", "Ratio"]

ghcFloatScope :: Scope
ghcFloatScope =
  bootScope
    "GHC.Float"
    [ "sqrt",
      "log",
      "exp",
      "logBase",
      "sin",
      "cos",
      "tan",
      "asin",
      "acos",
      "atan",
      "atan2",
      "sinh",
      "cosh",
      "tanh",
      "asinh",
      "acosh",
      "atanh",
      "pi",
      "floor",
      "ceiling",
      "round",
      "truncate",
      "floatRadix",
      "floatDigits",
      "floatRange",
      "decodeFloat",
      "encodeFloat",
      "exponent",
      "significand",
      "scaleFloat",
      "isNaN",
      "isInfinite",
      "isDenormalized",
      "isNegativeZero",
      "isIEEE",
      "atan2"
    ]
    ["Float", "Double", "Floating", "RealFloat"]

ghcIoScope :: Scope
ghcIoScope =
  bootScope
    "GHC.IO"
    ["unsafePerformIO", "unsafeDupablePerformIO", "unsafeInterleaveIO", "catch", "throwIO"]
    ["IO", "IOException"]

ghcIoRefScope :: Scope
ghcIoRefScope =
  bootScope
    "GHC.IORef"
    ["newIORef", "readIORef", "writeIORef", "modifyIORef", "atomicModifyIORef", "atomicWriteIORef"]
    ["IORef"]

ghcStScope :: Scope
ghcStScope =
  bootScope
    "GHC.ST"
    ["runST", "fixST", "unsafeInterleaveST", "ST"]
    ["ST", "STRep"]

ghcStRefScope :: Scope
ghcStRefScope =
  bootScope
    "GHC.STRef"
    ["newSTRef", "readSTRef", "writeSTRef", "modifySTRef", "modifySTRef'"]
    ["STRef"]

ghcArrScope :: Scope
ghcArrScope =
  bootScope
    "GHC.Arr"
    ["listArray", "array", "bounds", "indices", "elems", "assocs", "//"]
    ["Array", "STArray", "Ix"]

ghcExceptionScope :: Scope
ghcExceptionScope =
  bootScope
    "GHC.Exception"
    ["throw", "catch", "SomeException", "toException", "fromException", "displayException"]
    ["Exception", "SomeException", "SomeAsyncException"]

ghcConcScope :: Scope
ghcConcScope =
  bootScope
    "GHC.Conc"
    [ "forkIO",
      "killThread",
      "threadDelay",
      "myThreadId",
      "throwTo",
      "isCurrentThreadBound",
      "forkOn",
      "forkOnWithUnmask",
      "threadStatus",
      "threadCapability",
      "newMVar",
      "takeMVar",
      "putMVar",
      "readMVar",
      "modifyMVar_",
      "modifyMVar",
      "tryTakeMVar",
      "tryPutMVar",
      "isEmptyMVar",
      "withMVar",
      "swapMVar",
      "newEmptyMVar",
      "newTVar",
      "readTVar",
      "writeTVar",
      "modifyTVar",
      "modifyTVar'",
      "readTVarIO",
      "atomically",
      "retry",
      "orElse",
      "check",
      "STM",
      "TVar",
      "ThreadId",
      "MVar",
      "getNumCapabilities",
      "setNumCapabilities",
      "getNumProcessors"
    ]
    ["ThreadId", "MVar", "TVar", "STM", "BlockReason", "ThreadStatus"]

ghcMVarScope :: Scope
ghcMVarScope =
  bootScope
    "GHC.MVar"
    [ "newMVar",
      "takeMVar",
      "putMVar",
      "readMVar",
      "modifyMVar_",
      "modifyMVar",
      "tryTakeMVar",
      "tryPutMVar",
      "isEmptyMVar",
      "withMVar",
      "swapMVar",
      "newEmptyMVar",
      "mkWeakMVar"
    ]
    ["MVar"]

ghcForeignScope :: Scope
ghcForeignScope =
  bootScope
    "GHC.Foreign"
    [ "peekCString",
      "peekCStringLen",
      "newCString",
      "newCStringLen",
      "withCString",
      "withCStringLen"
    ]
    []

ghcStableScope :: Scope
ghcStableScope =
  bootScope
    "GHC.Stable"
    [ "makeStablePtr",
      "deRefStablePtr",
      "freeStablePtr",
      "castStablePtrToPtr",
      "castPtrToStablePtr"
    ]
    ["StablePtr"]

ghcPtrScope :: Scope
ghcPtrScope =
  bootScope
    "GHC.Ptr"
    [ "nullPtr",
      "castPtr",
      "plusPtr",
      "alignPtr",
      "minusPtr",
      "Ptr",
      "FunPtr",
      "nullFunPtr",
      "castFunPtr",
      "castFunPtrToPtr",
      "castPtrToFunPtr"
    ]
    ["Ptr", "FunPtr"]

ghcForeignPtrScope :: Scope
ghcForeignPtrScope =
  bootScope
    "GHC.ForeignPtr"
    [ "newForeignPtr",
      "newForeignPtr_",
      "addForeignPtrFinalizer",
      "withForeignPtr",
      "finalizeForeignPtr",
      "touchForeignPtr",
      "castForeignPtr",
      "plusForeignPtr",
      "mallocForeignPtr",
      "mallocForeignPtrBytes",
      "mallocForeignPtrArray",
      "mallocForeignPtrArray0",
      "newConcForeignPtr",
      "addForeignPtrConcFinalizer"
    ]
    ["ForeignPtr", "FinalizerPtr", "FinalizerEnvPtr"]

ghcWeakScope :: Scope
ghcWeakScope =
  bootScope
    "GHC.Weak"
    ["mkWeak", "deRefWeak", "finalize", "mkWeakPtr", "addFinalizer"]
    ["Weak"]

ghcFingerprintScope :: Scope
ghcFingerprintScope =
  bootScope
    "GHC.Fingerprint"
    ["fingerprint0", "fingerprintData", "fingerprintString", "fingerprintFingerprints"]
    ["Fingerprint"]

ghcWordScope :: Scope
ghcWordScope =
  bootScope
    "GHC.Word"
    []
    ["Word8", "Word16", "Word32", "Word64"]

ghcIntScope :: Scope
ghcIntScope =
  bootScope
    "GHC.Int"
    []
    ["Int8", "Int16", "Int32", "Int64"]

ghcUnicodeScope :: Scope
ghcUnicodeScope =
  bootScope
    "GHC.Unicode"
    ["isAlpha", "isAlphaNum", "isDigit", "isLower", "isUpper", "isSpace", "toLower", "toUpper", "toTitle"]
    ["GeneralCategory"]

ghcTypeLitsScope :: Scope
ghcTypeLitsScope =
  bootScope
    "GHC.TypeLits"
    [ "natVal",
      "symbolVal",
      "charVal",
      "sameNat",
      "sameSymbol",
      "sameChar",
      "someNatVal",
      "someSymbolVal",
      "someCharVal",
      "KnownNat",
      "KnownSymbol",
      "KnownChar"
    ]
    [ "Nat",
      "Symbol",
      "Char",
      "SomeNat",
      "SomeSymbol",
      "SomeChar",
      "CmpNat",
      "CmpSymbol",
      "CmpChar",
      "+",
      "-",
      "*",
      "^",
      "Div",
      "Mod",
      "Log2",
      "AppendSymbol",
      "ConsSymbol",
      "UnconsSymbol"
    ]

ghcTypeNatsScope :: Scope
ghcTypeNatsScope =
  bootScope
    "GHC.TypeNats"
    ["natVal", "sameNat", "someNatVal", "KnownNat"]
    ["Nat", "SomeNat", "+", "-", "*", "^", "Div", "Mod", "Log2", "CmpNat"]

ghcOverloadedLabelsScope :: Scope
ghcOverloadedLabelsScope =
  bootScope
    "GHC.OverloadedLabels"
    ["fromLabel"]
    ["IsLabel"]

ghcRecordsScope :: Scope
ghcRecordsScope =
  bootScope
    "GHC.Records"
    ["getField", "setField"]
    ["HasField"]

dataProxyScope :: Scope
dataProxyScope =
  bootScope
    "Data.Proxy"
    ["Proxy", "asProxyTypeOf"]
    ["Proxy", "KProxy"]

dataCoerceScope :: Scope
dataCoerceScope =
  bootScope
    "Data.Coerce"
    ["coerce"]
    ["Coercible"]

dataTypeEqualityScope :: Scope
dataTypeEqualityScope =
  bootScope
    "Data.Type.Equality"
    ["refl", "sym", "trans", "castWith", "gcastWith", "apply", "inner", "outer", "testEquality"]
    [":~:", ":~~:", "TestEquality"]

dataTypeCoercionScope :: Scope
dataTypeCoercionScope =
  bootScope
    "Data.Type.Coercion"
    ["coerceWith", "sym", "trans", "repr", "testCoercion"]
    ["Coercion", "TestCoercion"]

dataVoidScope :: Scope
dataVoidScope =
  bootScope
    "Data.Void"
    ["absurd", "vacuous"]
    ["Void"]

dataFixedScope :: Scope
dataFixedScope =
  bootScope
    "Data.Fixed"
    ["getFixed", "MkFixed"]
    ["Fixed", "E0", "E1", "E2", "E3", "E6", "E9", "E12", "Uni", "Deci", "Centi", "Milli", "Micro", "Nano", "Pico"]

dataComplexScope :: Scope
dataComplexScope =
  bootScope
    "Data.Complex"
    ["realPart", "imagPart", "conjugate", "mkPolar", "cis", "polar", "magnitude", "phase", ":+"]
    ["Complex"]

dataRatioScope :: Scope
dataRatioScope =
  bootScope
    "Data.Ratio"
    ["numerator", "denominator", "%"]
    ["Ratio", "Rational"]

numericScope :: Scope
numericScope =
  bootScope
    "Numeric"
    [ "showHex",
      "showOct",
      "showBin",
      "showIntAtBase",
      "showInt",
      "showFloat",
      "showFFloat",
      "showEFloat",
      "showGFloat",
      "readHex",
      "readOct",
      "readBin",
      "readDec",
      "readInt",
      "readFloat",
      "floatToDigits",
      "fromRat"
    ]
    []

textReadScope :: Scope
textReadScope =
  bootScope
    "Text.Read"
    ["read", "readMaybe", "readEither", "reads", "readListMaybe"]
    ["Read", "ReadS", "ReadPrec", "Lexeme"]

textShowScope :: Scope
textShowScope =
  bootScope
    "Text.Show"
    ["show", "shows", "showsPrec", "showString", "showParen", "showChar", "showListWith"]
    ["Show", "ShowS"]

controlApplicativeScope :: Scope
controlApplicativeScope =
  bootScope
    "Control.Applicative"
    [ "pure",
      "<*>",
      "<*",
      "*>",
      "<$>",
      "<$",
      "liftA",
      "liftA2",
      "liftA3",
      "optional",
      "some",
      "many",
      "<|>",
      "Alternative",
      "empty"
    ]
    ["Applicative", "Alternative"]

controlArrowScope :: Scope
controlArrowScope =
  bootScope
    "Control.Arrow"
    [ "arr",
      "first",
      "second",
      "***",
      "&&&",
      "|||",
      "+++",
      "left",
      "right",
      "returnA",
      ">>>",
      "<<<",
      "loop",
      "app",
      "fanout",
      "fanin"
    ]
    ["Arrow", "ArrowZero", "ArrowPlus", "ArrowChoice", "ArrowApply", "ArrowLoop", "ArrowMonad"]

controlCategoryScope :: Scope
controlCategoryScope =
  bootScope
    "Control.Category"
    ["id", ".", ">>>", "<<<"]
    ["Category"]

controlDeepSeqScope :: Scope
controlDeepSeqScope =
  bootScope
    "Control.DeepSeq"
    ["rnf", "deepseq", "$!!", "force", "rwhnf", "NFData"]
    ["NFData"]

systemIoUnsafeScope :: Scope
systemIoUnsafeScope =
  bootScope
    "System.IO.Unsafe"
    ["unsafePerformIO", "unsafeDupablePerformIO", "unsafeInterleaveIO", "unsafeFixIO"]
    []

foreignStorableScope :: Scope
foreignStorableScope =
  bootScope
    "Foreign.Storable"
    [ "sizeOf",
      "alignment",
      "peekElemOff",
      "pokeElemOff",
      "peekByteOff",
      "pokeByteOff",
      "peek",
      "poke"
    ]
    ["Storable"]

foreignPtrScope :: Scope
foreignPtrScope =
  bootScope
    "Foreign.Ptr"
    [ "nullPtr",
      "castPtr",
      "plusPtr",
      "alignPtr",
      "minusPtr",
      "nullFunPtr",
      "castFunPtr",
      "castFunPtrToPtr",
      "castPtrToFunPtr",
      "freeHaskellFunPtr",
      "Ptr",
      "FunPtr",
      "IntPtr",
      "WordPtr",
      "ptrToIntPtr",
      "intPtrToPtr",
      "ptrToWordPtr",
      "wordPtrToPtr"
    ]
    ["Ptr", "FunPtr", "IntPtr", "WordPtr"]

foreignForeignPtrScope :: Scope
foreignForeignPtrScope =
  bootScope
    "Foreign.ForeignPtr"
    [ "newForeignPtr",
      "newForeignPtr_",
      "addForeignPtrFinalizer",
      "withForeignPtr",
      "finalizeForeignPtr",
      "touchForeignPtr",
      "castForeignPtr",
      "plusForeignPtr",
      "mallocForeignPtr",
      "mallocForeignPtrBytes",
      "mallocForeignPtrArray",
      "mallocForeignPtrArray0",
      "newConcForeignPtr"
    ]
    ["ForeignPtr", "FinalizerPtr", "FinalizerEnvPtr"]

foreignForeignPtrUnsafeScope :: Scope
foreignForeignPtrUnsafeScope =
  bootScope
    "Foreign.ForeignPtr.Unsafe"
    ["unsafeForeignPtrToPtr"]
    []

foreignMarshalAllocScope :: Scope
foreignMarshalAllocScope =
  bootScope
    "Foreign.Marshal.Alloc"
    [ "malloc",
      "mallocBytes",
      "calloc",
      "callocBytes",
      "realloc",
      "reallocBytes",
      "free",
      "finalizerFree",
      "alloca",
      "allocaBytes",
      "allocaBytesAligned"
    ]
    []

foreignMarshalArrayScope :: Scope
foreignMarshalArrayScope =
  bootScope
    "Foreign.Marshal.Array"
    [ "mallocArray",
      "mallocArray0",
      "allocaArray",
      "allocaArray0",
      "reallocArray",
      "reallocArray0",
      "peekArray",
      "peekArray0",
      "pokeArray",
      "pokeArray0",
      "newArray",
      "newArray0",
      "withArray",
      "withArray0",
      "withArrayLen",
      "withArrayLen0",
      "copyArray",
      "moveArray",
      "lengthArray0",
      "advancePtr"
    ]
    []

foreignMarshalUtilsScope :: Scope
foreignMarshalUtilsScope =
  bootScope
    "Foreign.Marshal.Utils"
    [ "with",
      "new",
      "fromBool",
      "toBool",
      "maybeNew",
      "maybeWith",
      "maybePeek",
      "withMany",
      "copyBytes",
      "moveBytes",
      "fillBytes",
      "void"
    ]
    []

foreignMarshalErrorScope :: Scope
foreignMarshalErrorScope =
  bootScope
    "Foreign.Marshal.Error"
    ["throwIfNull", "throwIfNeg", "throwIfNeg_", "throwIf", "throwIf_", "void"]
    []

foreignCTypesScope :: Scope
foreignCTypesScope =
  bootScope
    "Foreign.C.Types"
    []
    [ "CChar",
      "CSChar",
      "CUChar",
      "CShort",
      "CUShort",
      "CInt",
      "CUInt",
      "CLong",
      "CULong",
      "CLLong",
      "CULLong",
      "CBool",
      "CFloat",
      "CDouble",
      "CPtrdiff",
      "CSize",
      "CSsize",
      "CWchar",
      "CSigAtomic",
      "CClock",
      "CTime",
      "CUSeconds",
      "CSUSeconds",
      "CIntPtr",
      "CUIntPtr",
      "CIntMax",
      "CUIntMax",
      "CString",
      "CStringLen",
      "CWString",
      "CWStringLen"
    ]

foreignCStringScope :: Scope
foreignCStringScope =
  bootScope
    "Foreign.C.String"
    [ "peekCString",
      "peekCStringLen",
      "newCString",
      "newCStringLen",
      "withCString",
      "withCStringLen",
      "charIsRepresentable",
      "peekCAString",
      "peekCAStringLen",
      "newCAString",
      "newCAStringLen",
      "withCAString",
      "withCAStringLen"
    ]
    ["CString", "CStringLen", "CWString", "CWStringLen"]

foreignCErrorScope :: Scope
foreignCErrorScope =
  bootScope
    "Foreign.C.Error"
    [ "getErrno",
      "resetErrno",
      "errnoToIOError",
      "throwErrno",
      "throwErrnoIf",
      "throwErrnoIf_",
      "throwErrnoIfNull",
      "throwErrnoIfMinus1",
      "throwErrnoIfMinus1_",
      "throwErrnoIfMinus1Retry",
      "throwErrnoIfMinus1Retry_",
      "throwErrnoIfNullRetry",
      "throwErrnoIfRetry",
      "throwErrnoIfRetry_",
      "throwErrnoPath",
      "throwErrnoPathIf",
      "throwErrnoPathIf_",
      "throwErrnoPathIfNull",
      "throwErrnoPathIfMinus1",
      "throwErrnoPathIfMinus1_",
      "eOK",
      "e2BIG",
      "eACCES",
      "eADDRINUSE",
      "eADDRNOTAVAIL",
      "eINVAL",
      "eNOSYS",
      "ePERM",
      "eEXIST",
      "eNOENT",
      "eNOTDIR",
      "Errno"
    ]
    ["Errno"]

foreignScope :: Scope
foreignScope =
  bootScope
    "Foreign"
    [ "nullPtr",
      "castPtr",
      "plusPtr",
      "peek",
      "poke",
      "free",
      "malloc",
      "alloca",
      "allocaBytes",
      "withForeignPtr",
      "newForeignPtr",
      "newForeignPtr_",
      "Ptr",
      "ForeignPtr",
      "Storable"
    ]
    ["Ptr", "ForeignPtr", "Storable"]

dataStRefScope :: Scope
dataStRefScope =
  bootScope
    "Data.STRef"
    ["newSTRef", "readSTRef", "writeSTRef", "modifySTRef", "modifySTRef'"]
    ["STRef"]

controlStScope :: Scope
controlStScope =
  bootScope
    "Control.ST"
    ["runST", "runSTArray", "ST", "STRef", "STArray"]
    ["ST"]

controlConcurrentScope :: Scope
controlConcurrentScope =
  bootScope
    "Control.Concurrent"
    [ "forkIO",
      "forkIOWithUnmask",
      "forkOn",
      "forkOnWithUnmask",
      "killThread",
      "throwTo",
      "myThreadId",
      "isCurrentThreadBound",
      "threadDelay",
      "threadWaitRead",
      "threadWaitWrite",
      "runInBoundThread",
      "runInUnboundThread",
      "mkWeakThreadId",
      "labelThread",
      "threadStatus",
      "threadCapability",
      "newEmptyMVar",
      "newMVar",
      "takeMVar",
      "putMVar",
      "readMVar",
      "tryTakeMVar",
      "tryPutMVar",
      "isEmptyMVar",
      "withMVar",
      "modifyMVar",
      "modifyMVar_",
      "swapMVar",
      "mkWeakMVar",
      "addMVarFinalizer",
      "getChanContents",
      "writeList2Chan",
      "newChan",
      "readChan",
      "writeChan",
      "dupChan",
      "unGetChan",
      "isEmptyChan",
      "getNumCapabilities",
      "setNumCapabilities",
      "getNumProcessors"
    ]
    ["ThreadId", "MVar", "Chan", "QSem", "QSemN"]

controlMVarScope :: Scope
controlMVarScope =
  bootScope
    "Control.Concurrent.MVar"
    [ "newEmptyMVar",
      "newMVar",
      "takeMVar",
      "putMVar",
      "readMVar",
      "tryTakeMVar",
      "tryPutMVar",
      "isEmptyMVar",
      "withMVar",
      "modifyMVar",
      "modifyMVar_",
      "swapMVar",
      "mkWeakMVar"
    ]
    ["MVar"]

controlChanScope :: Scope
controlChanScope =
  bootScope
    "Control.Concurrent.Chan"
    [ "newChan",
      "readChan",
      "writeChan",
      "dupChan",
      "getChanContents",
      "writeList2Chan",
      "unGetChan",
      "isEmptyChan"
    ]
    ["Chan"]

controlQSemScope :: Scope
controlQSemScope =
  bootScope
    "Control.Concurrent.QSem"
    ["newQSem", "waitQSem", "signalQSem", "tryWaitQSem"]
    ["QSem"]

controlQSemNScope :: Scope
controlQSemNScope =
  bootScope
    "Control.Concurrent.QSemN"
    ["newQSemN", "waitQSemN", "signalQSemN", "tryWaitQSemN"]
    ["QSemN"]

controlStmScope :: Scope
controlStmScope =
  bootScope
    "Control.Concurrent.STM"
    [ "atomically",
      "retry",
      "orElse",
      "check",
      "throwSTM",
      "catchSTM",
      "newTVar",
      "newTVarIO",
      "readTVar",
      "readTVarIO",
      "writeTVar",
      "modifyTVar",
      "modifyTVar'",
      "swapTVar",
      "registerDelay",
      "mkWeakTVar",
      "newEmptyTMVar",
      "newEmptyTMVarIO",
      "newTMVar",
      "newTMVarIO",
      "takeTMVar",
      "putTMVar",
      "readTMVar",
      "tryReadTMVar",
      "swapTMVar",
      "tryTakeTMVar",
      "tryPutTMVar",
      "isEmptyTMVar",
      "mkWeakTMVar",
      "newTChan",
      "newTChanIO",
      "newBroadcastTChan",
      "newBroadcastTChanIO",
      "dupTChan",
      "cloneTChan",
      "readTChan",
      "tryReadTChan",
      "peekTChan",
      "tryPeekTChan",
      "writeTChan",
      "unGetTChan",
      "isEmptyTChan",
      "STM",
      "TVar",
      "TMVar",
      "TChan"
    ]
    ["STM", "TVar", "TMVar", "TChan"]

controlStmTVarScope :: Scope
controlStmTVarScope =
  bootScope
    "Control.Concurrent.STM.TVar"
    ["newTVar", "newTVarIO", "readTVar", "readTVarIO", "writeTVar", "modifyTVar", "modifyTVar'", "swapTVar", "mkWeakTVar"]
    ["TVar"]

controlStmTMVarScope :: Scope
controlStmTMVarScope =
  bootScope
    "Control.Concurrent.STM.TMVar"
    [ "newEmptyTMVar",
      "newEmptyTMVarIO",
      "newTMVar",
      "newTMVarIO",
      "takeTMVar",
      "putTMVar",
      "readTMVar",
      "tryReadTMVar",
      "swapTMVar",
      "tryTakeTMVar",
      "tryPutTMVar",
      "isEmptyTMVar",
      "mkWeakTMVar"
    ]
    ["TMVar"]

controlStmTChanScope :: Scope
controlStmTChanScope =
  bootScope
    "Control.Concurrent.STM.TChan"
    [ "newTChan",
      "newTChanIO",
      "newBroadcastTChan",
      "newBroadcastTChanIO",
      "dupTChan",
      "cloneTChan",
      "readTChan",
      "tryReadTChan",
      "writeTChan",
      "unGetTChan",
      "isEmptyTChan"
    ]
    ["TChan"]

controlStmTQueueScope :: Scope
controlStmTQueueScope =
  bootScope
    "Control.Concurrent.STM.TQueue"
    [ "newTQueue",
      "newTQueueIO",
      "readTQueue",
      "tryReadTQueue",
      "peekTQueue",
      "tryPeekTQueue",
      "writeTQueue",
      "unGetTQueue",
      "isEmptyTQueue",
      "flushTQueue"
    ]
    ["TQueue"]

controlStmTBQueueScope :: Scope
controlStmTBQueueScope =
  bootScope
    "Control.Concurrent.STM.TBQueue"
    [ "newTBQueue",
      "newTBQueueIO",
      "readTBQueue",
      "tryReadTBQueue",
      "peekTBQueue",
      "tryPeekTBQueue",
      "writeTBQueue",
      "unGetTBQueue",
      "isEmptyTBQueue",
      "isFullTBQueue",
      "lengthTBQueue",
      "flushTBQueue"
    ]
    ["TBQueue"]

controlConcurrentAsyncScope :: Scope
controlConcurrentAsyncScope =
  bootScope
    "Control.Concurrent.Async"
    [ "async",
      "asyncBound",
      "asyncOn",
      "asyncWithUnmask",
      "asyncOnWithUnmask",
      "withAsync",
      "withAsyncBound",
      "withAsyncOn",
      "withAsyncWithUnmask",
      "wait",
      "poll",
      "waitCatch",
      "cancel",
      "cancelWith",
      "uninterruptibleCancel",
      "waitAny",
      "waitAnyCatch",
      "waitAnyCancel",
      "waitAnyCatchCancel",
      "waitEither",
      "waitEitherCatch",
      "waitEitherCancel",
      "waitEitherCatchCancel",
      "waitBoth",
      "link",
      "link2",
      "race",
      "race_",
      "concurrently",
      "concurrently_",
      "mapConcurrently",
      "mapConcurrently_",
      "forConcurrently",
      "forConcurrently_",
      "replicateConcurrently",
      "replicateConcurrently_",
      "Async",
      "Concurrently"
    ]
    ["Async", "Concurrently"]

dataFoldableScope :: Scope
dataFoldableScope =
  bootScope
    "Data.Foldable"
    [ "fold",
      "foldMap",
      "foldMap'",
      "foldr",
      "foldr'",
      "foldl",
      "foldl'",
      "foldr1",
      "foldl1",
      "toList",
      "null",
      "length",
      "elem",
      "maximum",
      "minimum",
      "sum",
      "product",
      "concat",
      "concatMap",
      "and",
      "or",
      "any",
      "all",
      "maximumBy",
      "minimumBy",
      "notElem",
      "find",
      "asum",
      "sequenceA_",
      "traverse_",
      "for_",
      "mapM_",
      "forM_",
      "sequence_",
      "msum"
    ]
    ["Foldable"]

dataTraversableScope :: Scope
dataTraversableScope =
  bootScope
    "Data.Traversable"
    ["traverse", "sequenceA", "mapM", "sequence", "for", "forM", "mapAccumL", "mapAccumR"]
    ["Traversable"]

dataBifunctorScope :: Scope
dataBifunctorScope =
  bootScope
    "Data.Bifunctor"
    ["bimap", "first", "second"]
    ["Bifunctor"]

dataBitraversableScope :: Scope
dataBitraversableScope =
  bootScope
    "Data.Bitraversable"
    ["bitraverse", "bisequenceA", "bifor", "bimapM", "bisequence", "biforM"]
    ["Bitraversable"]

dataBifoldableScope :: Scope
dataBifoldableScope =
  bootScope
    "Data.Bifoldable"
    ["bifold", "bifoldMap", "bifoldr", "bifoldl", "bifoldr1", "bifoldl1", "bitraverse_", "bifor_", "bimapM_", "biforM_", "bisequenceA_", "bisequence_", "biList", "binull", "bilength", "bielem", "bimaximum", "biminimum", "bisum", "biproduct", "biconcat", "biconcatMap", "biand", "bior", "biany", "biall"]
    ["Bifoldable"]

dataSemigroupScope :: Scope
dataSemigroupScope =
  bootScope
    "Data.Semigroup"
    ["<>", "sconcat", "stimes", "stimesMonoid", "stimesIdempotent", "stimesIdempotentMonoid", "Arg", "Min", "Max", "First", "Last", "WrappedMonoid", "Dual", "Endo", "All", "Any", "Sum", "Product"]
    ["Semigroup", "Arg", "Min", "Max", "First", "Last", "WrappedMonoid"]

dataMonoidScope :: Scope
dataMonoidScope =
  bootScope
    "Data.Monoid"
    [ "mempty",
      "mappend",
      "mconcat",
      "Dual",
      "getDual",
      "Endo",
      "appEndo",
      "All",
      "getAll",
      "Any",
      "getAny",
      "Sum",
      "getSum",
      "Product",
      "getProduct",
      "First",
      "getFirst",
      "Last",
      "getLast",
      "Alt",
      "Ap"
    ]
    ["Monoid", "Dual", "Endo", "All", "Any", "Sum", "Product", "First", "Last", "Alt", "Ap"]

dataFunctorScope :: Scope
dataFunctorScope =
  bootScope
    "Data.Functor"
    ["fmap", "<$>", "<$", "$>", "<&>", "void"]
    ["Functor"]

dataFunctorIdentityScope :: Scope
dataFunctorIdentityScope =
  bootScope
    "Data.Functor.Identity"
    ["runIdentity", "Identity"]
    ["Identity"]

dataFunctorConstScope :: Scope
dataFunctorConstScope =
  bootScope
    "Data.Functor.Const"
    ["getConst", "Const"]
    ["Const"]

dataFunctorComposeScope :: Scope
dataFunctorComposeScope =
  bootScope
    "Data.Functor.Compose"
    ["getCompose", "Compose"]
    ["Compose"]

dataFunctorClassesScope :: Scope
dataFunctorClassesScope =
  bootScope
    "Data.Functor.Classes"
    [ "liftEq",
      "liftCompare",
      "liftReadsPrec",
      "liftReadList",
      "liftShowsPrec",
      "liftShowList",
      "eq1",
      "compare1",
      "readsPrec1",
      "showsPrec1",
      "liftEq2",
      "liftCompare2",
      "liftReadsPrec2",
      "liftReadList2",
      "liftShowsPrec2",
      "liftShowList2"
    ]
    ["Eq1", "Eq2", "Ord1", "Ord2", "Read1", "Read2", "Show1", "Show2"]

dataFunctorContravariantScope :: Scope
dataFunctorContravariantScope =
  bootScope
    "Data.Functor.Contravariant"
    ["contramap", ">$<", ">$$<", "$<", "Predicate", "Comparison", "Equivalence", "Op"]
    ["Contravariant", "Predicate", "Comparison", "Equivalence", "Op"]

dataListNonEmptyScope :: Scope
dataListNonEmptyScope =
  bootScope
    "Data.List.NonEmpty"
    [ "fromList",
      "toList",
      "head",
      "tail",
      "last",
      "init",
      "map",
      "filter",
      "zip",
      "zipWith",
      "unzip",
      "sort",
      "sortBy",
      "sortOn",
      "nub",
      "nubBy",
      "group",
      "groupBy",
      "partition",
      "isPrefixOf",
      "isSuffixOf",
      "intercalate",
      "intersperse",
      "transpose",
      "subsequences",
      "permutations",
      "tails",
      "inits",
      "unfoldr",
      "scanl",
      "scanr",
      "scanl1",
      "scanr1",
      "iterate",
      "repeat",
      "cycle",
      "take",
      "drop",
      "splitAt",
      "span",
      "break",
      "takeWhile",
      "dropWhile",
      "reverse",
      "length",
      "fromNonEmpty",
      "xor",
      "toNonEmpty",
      ":|",
      "NonEmpty"
    ]
    ["NonEmpty"]

dataSetScope :: Scope
dataSetScope =
  bootScope
    "Data.Set"
    [ "empty",
      "singleton",
      "fromList",
      "fromAscList",
      "fromDescList",
      "toList",
      "toAscList",
      "toDescList",
      "elems",
      "insert",
      "delete",
      "member",
      "notMember",
      "null",
      "size",
      "union",
      "unions",
      "difference",
      "intersection",
      "isSubsetOf",
      "isProperSubsetOf",
      "disjoint",
      "map",
      "filter",
      "partition",
      "foldl",
      "foldl'",
      "foldr",
      "foldr'",
      "findMin",
      "findMax",
      "deleteMin",
      "deleteMax",
      "minView",
      "maxView",
      "lookupMin",
      "lookupMax",
      "splitMember",
      "split",
      "Set"
    ]
    ["Set"]

dataUniqueScope :: Scope
dataUniqueScope =
  bootScope
    "Data.Unique"
    ["newUnique", "hashUnique"]
    ["Unique"]

systemMemWeakScope :: Scope
systemMemWeakScope =
  bootScope
    "System.Mem.Weak"
    ["mkWeak", "mkWeakPtr", "addFinalizer", "deRefWeak", "finalize"]
    ["Weak"]

systemMemScope :: Scope
systemMemScope =
  bootScope
    "System.Mem"
    ["performGC", "performMajorGC", "performMinorGC"]
    []

systemMemStableNameScope :: Scope
systemMemStableNameScope =
  bootScope
    "System.Mem.StableName"
    ["makeStableName", "hashStableName", "eqStableName"]
    ["StableName"]

ghcDesugarScope :: Scope
ghcDesugarScope =
  bootScope
    "GHC.Desugar"
    ["toAnnotationWrapper"]
    ["AnnotationWrapper"]

systemCpuTimeScope :: Scope
systemCpuTimeScope =
  bootScope
    "System.CPUTime"
    ["getCPUTime", "cpuTimePrecision"]
    []

-- ---------------------------------------------------------------------------
-- Boot shims for common non-base packages
-- ---------------------------------------------------------------------------

timeExports :: ModuleExports
timeExports =
  Map.fromList
    [ ("Data.Time", dataTimeScope),
      ("Data.Time.Clock", dataTimeClockScope),
      ("Data.Time.Calendar", dataTimeCalendarScope),
      ("Data.Time.LocalTime", dataTimeLocalTimeScope),
      ("Data.Time.Format", dataTimeFormatScope),
      ("Data.Time.Clock.POSIX", dataTimeClockPosixScope),
      ("Data.Time.Clock.System", dataTimeClockSystemScope)
    ]

dataTimeScope :: Scope
dataTimeScope =
  bootScope
    "Data.Time"
    [ "getCurrentTime",
      "getZonedTime",
      "utcToLocalTime",
      "localTimeToUTC",
      "parseTimeM",
      "formatTime",
      "defaultTimeLocale",
      "UTCTime",
      "ZonedTime",
      "LocalTime",
      "TimeZone",
      "NominalDiffTime",
      "DiffTime",
      "Day",
      "TimeOfDay",
      "CalendarDiffTime"
    ]
    [ "UTCTime",
      "ZonedTime",
      "LocalTime",
      "TimeZone",
      "NominalDiffTime",
      "DiffTime",
      "Day",
      "TimeOfDay",
      "CalendarDiffTime"
    ]

dataTimeClockScope :: Scope
dataTimeClockScope =
  bootScope
    "Data.Time.Clock"
    [ "getCurrentTime",
      "utctDay",
      "utctDayTime",
      "diffUTCTime",
      "addUTCTime",
      "nominalDiffTimeToSeconds",
      "secondsToNominalDiffTime",
      "UTCTime",
      "NominalDiffTime",
      "DiffTime",
      "UniversalTime",
      "getModJulianDate",
      "ModJulianDate"
    ]
    ["UTCTime", "NominalDiffTime", "DiffTime", "UniversalTime", "ModJulianDate"]

dataTimeCalendarScope :: Scope
dataTimeCalendarScope =
  bootScope
    "Data.Time.Calendar"
    [ "fromGregorian",
      "toGregorian",
      "fromGregorianValid",
      "addDays",
      "diffDays",
      "addGregorianMonthsClip",
      "addGregorianYearsClip",
      "gregorianMonthLength",
      "isLeapYear",
      "toModifiedJulianDay",
      "ModifiedJulianDay",
      "Day"
    ]
    ["Day"]

dataTimeLocalTimeScope :: Scope
dataTimeLocalTimeScope =
  bootScope
    "Data.Time.LocalTime"
    [ "getZonedTime",
      "utcToLocalTime",
      "localTimeToUTC",
      "localTimeToUTCTZ",
      "zonedTimeToUTC",
      "utcToZonedTime",
      "getCurrentTimeZone",
      "hoursToTimeZone",
      "minutesToTimeZone",
      "utc",
      "TimeZone",
      "ZonedTime",
      "LocalTime",
      "TimeOfDay",
      "midnight",
      "midday",
      "localDay",
      "localTimeOfDay",
      "zonedTimeToLocalTime",
      "zonedTimeZone",
      "TimeZone",
      "LocalTime",
      "ZonedTime",
      "TimeOfDay"
    ]
    ["TimeZone", "LocalTime", "ZonedTime", "TimeOfDay"]

dataTimeFormatScope :: Scope
dataTimeFormatScope =
  bootScope
    "Data.Time.Format"
    [ "formatTime",
      "parseTimeM",
      "parseTimeOrError",
      "defaultTimeLocale",
      "iso8601DateFormat",
      "rfc822DateFormat",
      "TimeLocale",
      "FormatTime",
      "ParseTime"
    ]
    ["TimeLocale", "FormatTime", "ParseTime"]

dataTimeClockPosixScope :: Scope
dataTimeClockPosixScope =
  bootScope
    "Data.Time.Clock.POSIX"
    [ "getPOSIXTime",
      "utcTimeToPOSIXSeconds",
      "posixSecondsToUTCTime",
      "POSIXTime"
    ]
    ["POSIXTime"]

dataTimeClockSystemScope :: Scope
dataTimeClockSystemScope =
  bootScope
    "Data.Time.Clock.System"
    ["getSystemTime", "systemToUTCTime", "utcToSystemTime", "SystemTime"]
    ["SystemTime"]

bytestringExports :: ModuleExports
bytestringExports =
  Map.fromList
    [ ("Data.ByteString", dataBytestringScope),
      ("Data.ByteString.Char8", dataBytestringChar8Scope),
      ("Data.ByteString.Lazy", dataBytestringLazyScope),
      ("Data.ByteString.Lazy.Char8", dataBytestringLazyChar8Scope),
      ("Data.ByteString.Internal", dataBytestringInternalScope),
      ("Data.ByteString.Builder", dataBytestringBuilderScope),
      ("Data.ByteString.Short", dataBytestringShortScope)
    ]

dataBytestringScope :: Scope
dataBytestringScope =
  bootScope
    "Data.ByteString"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "fromStrict",
      "toStrict",
      "null",
      "length",
      "head",
      "tail",
      "last",
      "init",
      "cons",
      "snoc",
      "append",
      "concat",
      "concatMap",
      "any",
      "all",
      "foldl",
      "foldl'",
      "foldr",
      "map",
      "filter",
      "intercalate",
      "intersperse",
      "transpose",
      "take",
      "drop",
      "splitAt",
      "takeWhile",
      "dropWhile",
      "span",
      "break",
      "split",
      "splitWith",
      "isPrefixOf",
      "isSuffixOf",
      "isInfixOf",
      "elem",
      "notElem",
      "find",
      "findIndex",
      "findIndices",
      "elemIndex",
      "zip",
      "zipWith",
      "unzip",
      "index",
      "unsafeIndex",
      "unsafeTake",
      "unsafeDrop",
      "hPutStr",
      "hPutStrLn",
      "hGetContents",
      "hGet",
      "hGetSome",
      "putStr",
      "putStrLn",
      "getContents",
      "interact",
      "getLine",
      "readFile",
      "writeFile",
      "appendFile",
      "ByteString"
    ]
    ["ByteString"]

dataBytestringChar8Scope :: Scope
dataBytestringChar8Scope =
  bootScope
    "Data.ByteString.Char8"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "null",
      "length",
      "cons",
      "snoc",
      "append",
      "head",
      "tail",
      "last",
      "init",
      "map",
      "filter",
      "foldl",
      "foldl'",
      "foldr",
      "concat",
      "concatMap",
      "take",
      "drop",
      "splitAt",
      "takeWhile",
      "dropWhile",
      "span",
      "break",
      "split",
      "lines",
      "words",
      "unlines",
      "unwords",
      "isPrefixOf",
      "isSuffixOf",
      "isInfixOf",
      "readFile",
      "writeFile",
      "appendFile",
      "putStr",
      "putStrLn",
      "getContents",
      "getLine",
      "hPutStr",
      "hPutStrLn",
      "hGetContents",
      "hGetLine",
      "ByteString"
    ]
    ["ByteString"]

dataBytestringLazyScope :: Scope
dataBytestringLazyScope =
  bootScope
    "Data.ByteString.Lazy"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "fromStrict",
      "toStrict",
      "fromChunks",
      "toChunks",
      "null",
      "length",
      "head",
      "tail",
      "last",
      "init",
      "cons",
      "snoc",
      "append",
      "concat",
      "concatMap",
      "any",
      "all",
      "foldl",
      "foldl'",
      "foldr",
      "map",
      "filter",
      "take",
      "drop",
      "splitAt",
      "takeWhile",
      "dropWhile",
      "span",
      "break",
      "split",
      "intercalate",
      "isPrefixOf",
      "isSuffixOf",
      "readFile",
      "writeFile",
      "appendFile",
      "hGetContents",
      "hPutStr",
      "ByteString"
    ]
    ["ByteString"]

dataBytestringLazyChar8Scope :: Scope
dataBytestringLazyChar8Scope =
  bootScope
    "Data.ByteString.Lazy.Char8"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "null",
      "length",
      "cons",
      "snoc",
      "append",
      "head",
      "tail",
      "lines",
      "words",
      "map",
      "filter",
      "foldl",
      "foldr",
      "concat",
      "take",
      "drop",
      "ByteString"
    ]
    ["ByteString"]

dataBytestringInternalScope :: Scope
dataBytestringInternalScope =
  bootScope
    "Data.ByteString.Internal"
    ["create", "createAndTrim", "unsafeCreate", "fromForeignPtr", "toForeignPtr", "unsafeHead", "inlinePerformIO", "nullForeignPtr", "PS"]
    ["ByteString"]

dataBytestringBuilderScope :: Scope
dataBytestringBuilderScope =
  bootScope
    "Data.ByteString.Builder"
    [ "toLazyByteString",
      "toStrictByteString",
      "hPutBuilder",
      "byteString",
      "lazyByteString",
      "shortByteString",
      "word8",
      "int8",
      "word16BE",
      "word16LE",
      "word32BE",
      "word32LE",
      "word64BE",
      "word64LE",
      "int16BE",
      "int16LE",
      "int32BE",
      "int32LE",
      "int64BE",
      "int64LE",
      "floatBE",
      "floatLE",
      "doubleBE",
      "doubleLE",
      "char7",
      "string7",
      "char8",
      "string8",
      "charUtf8",
      "stringUtf8",
      "Builder"
    ]
    ["Builder"]

dataBytestringShortScope :: Scope
dataBytestringShortScope =
  bootScope
    "Data.ByteString.Short"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "null",
      "length",
      "fromShort",
      "toShort",
      "ShortByteString"
    ]
    ["ShortByteString"]

textExports :: ModuleExports
textExports =
  Map.fromList
    [ ("Data.Text", dataTextScope),
      ("Data.Text.Lazy", dataTextLazyScope),
      ("Data.Text.Encoding", dataTextEncodingScope),
      ("Data.Text.IO", dataTextIoScope),
      ("Data.Text.Read", dataTextReadScope),
      ("Data.Text.Internal", dataTextInternalScope)
    ]

dataTextScope :: Scope
dataTextScope =
  bootScope
    "Data.Text"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "null",
      "length",
      "cons",
      "snoc",
      "append",
      "head",
      "tail",
      "last",
      "init",
      "uncons",
      "unsnoc",
      "map",
      "intercalate",
      "intersperse",
      "transpose",
      "reverse",
      "foldl",
      "foldl'",
      "foldr",
      "foldl1",
      "foldr1",
      "concat",
      "concatMap",
      "any",
      "all",
      "take",
      "drop",
      "takeEnd",
      "dropEnd",
      "splitAt",
      "takeWhile",
      "takeWhileEnd",
      "dropWhile",
      "dropWhileEnd",
      "dropAround",
      "span",
      "break",
      "spanEnd",
      "breakEnd",
      "breakOn",
      "breakOnEnd",
      "breakOnAll",
      "split",
      "splitOn",
      "chunksOf",
      "group",
      "groupBy",
      "isPrefixOf",
      "isSuffixOf",
      "isInfixOf",
      "stripPrefix",
      "stripSuffix",
      "replace",
      "toCaseFold",
      "toLower",
      "toUpper",
      "toTitle",
      "justifyLeft",
      "justifyRight",
      "center",
      "words",
      "unwords",
      "lines",
      "unlines",
      "strip",
      "stripStart",
      "stripEnd",
      "filter",
      "find",
      "findIndex",
      "findIndices",
      "elem",
      "notElem",
      "index",
      "count",
      "compareLength",
      "replicate",
      "copy",
      "unfoldr",
      "unfoldrN",
      "decodeUtf8",
      "encodeUtf8",
      "hPutStr",
      "hPutStrLn",
      "hGetContents",
      "hGetLine",
      "putStr",
      "putStrLn",
      "getContents",
      "getLine",
      "readFile",
      "writeFile",
      "appendFile",
      "Text"
    ]
    ["Text"]

dataTextLazyScope :: Scope
dataTextLazyScope =
  bootScope
    "Data.Text.Lazy"
    [ "empty",
      "singleton",
      "pack",
      "unpack",
      "fromStrict",
      "toStrict",
      "null",
      "length",
      "head",
      "tail",
      "last",
      "init",
      "map",
      "filter",
      "foldl",
      "foldl'",
      "foldr",
      "concat",
      "concatMap",
      "take",
      "drop",
      "splitAt",
      "takeWhile",
      "dropWhile",
      "span",
      "break",
      "isPrefixOf",
      "isSuffixOf",
      "words",
      "unwords",
      "lines",
      "unlines",
      "toLower",
      "toUpper",
      "strip",
      "Text"
    ]
    ["Text"]

dataTextEncodingScope :: Scope
dataTextEncodingScope =
  bootScope
    "Data.Text.Encoding"
    [ "decodeUtf8",
      "decodeUtf8With",
      "decodeUtf8Lenient",
      "encodeUtf8",
      "encodeUtf16LE",
      "encodeUtf16BE",
      "encodeUtf32LE",
      "encodeUtf32BE",
      "decodeASCII",
      "decodeLatin1"
    ]
    []

dataTextIoScope :: Scope
dataTextIoScope =
  bootScope
    "Data.Text.IO"
    ["readFile", "writeFile", "appendFile", "hGetContents", "hGetLine", "hPutStr", "hPutStrLn", "putStr", "putStrLn", "getLine", "getContents", "interact"]
    []

dataTextReadScope :: Scope
dataTextReadScope =
  bootScope
    "Data.Text.Read"
    ["decimal", "hexadecimal", "signed", "rational", "double"]
    ["Reader"]

dataTextInternalScope :: Scope
dataTextInternalScope =
  bootScope
    "Data.Text.Internal"
    ["Text", "empty", "pack", "unpack"]
    ["Text"]

containersExports :: ModuleExports
containersExports =
  Map.fromList
    [ ("Data.Map", dataMapScope),
      ("Data.Map.Strict", dataMapScope),
      ("Data.Map.Lazy", dataMapScope),
      ("Data.Map.Internal", dataMapScope),
      ("Data.Set", dataSetScope),
      ("Data.IntMap", dataIntMapScope),
      ("Data.IntMap.Strict", dataIntMapScope),
      ("Data.IntMap.Lazy", dataIntMapScope),
      ("Data.IntSet", dataIntSetScope),
      ("Data.Sequence", dataSequenceScope),
      ("Data.Map.Strict.Internal", dataMapScope),
      ("Data.Set.Internal", dataSetScope)
    ]

arrayExports :: ModuleExports
arrayExports =
  Map.fromList
    [ ("Data.Array", dataArrayScope),
      ("Data.Array.IO", dataArrayIoScope),
      ("Data.Array.ST", dataArrayStScope),
      ("Data.Array.MArray", dataArrayMArrayScope),
      ("Data.Array.IArray", dataArrayScope),
      ("Data.Array.Unboxed", dataArrayUnboxedScope)
    ]

dataArrayIoScope :: Scope
dataArrayIoScope =
  bootScope
    "Data.Array.IO"
    [ "newArray",
      "newArray_",
      "newListArray",
      "readArray",
      "writeArray",
      "getAssocs",
      "getElems",
      "getBounds",
      "freeze",
      "thaw",
      "IOArray",
      "IOUArray"
    ]
    ["IOArray", "IOUArray", "MArray"]

dataArrayStScope :: Scope
dataArrayStScope =
  bootScope
    "Data.Array.ST"
    [ "newArray",
      "newArray_",
      "newListArray",
      "readArray",
      "writeArray",
      "getAssocs",
      "getElems",
      "getBounds",
      "freeze",
      "thaw",
      "runSTArray",
      "STArray",
      "STUArray"
    ]
    ["STArray", "STUArray", "MArray"]

dataArrayMArrayScope :: Scope
dataArrayMArrayScope =
  bootScope
    "Data.Array.MArray"
    [ "newArray",
      "newArray_",
      "newListArray",
      "readArray",
      "writeArray",
      "getAssocs",
      "getElems",
      "getBounds",
      "getIndices",
      "freeze",
      "thaw",
      "mapArray",
      "mapIndices"
    ]
    ["MArray"]

dataArrayUnboxedScope :: Scope
dataArrayUnboxedScope =
  bootScope
    "Data.Array.Unboxed"
    [ "array",
      "listArray",
      "bounds",
      "indices",
      "elems",
      "assocs",
      "//",
      "UArray"
    ]
    ["UArray", "Ix"]

deepseqExports :: ModuleExports
deepseqExports =
  Map.fromList
    [ ("Control.DeepSeq", controlDeepSeqScope)
    ]

binaryExports :: ModuleExports
binaryExports =
  Map.fromList
    [ ("Data.Binary", dataBinaryScope),
      ("Data.Binary.Get", dataBinaryGetScope),
      ("Data.Binary.Put", dataBinaryPutScope)
    ]

dataBinaryScope :: Scope
dataBinaryScope =
  bootScope
    "Data.Binary"
    ["encode", "decode", "decodeOrFail", "encodeFile", "decodeFile", "decodeFileOrFail"]
    ["Binary", "Get", "Put", "Word8"]

dataBinaryGetScope :: Scope
dataBinaryGetScope =
  bootScope
    "Data.Binary.Get"
    [ "runGet",
      "runGetOrFail",
      "runGetIncremental",
      "getWord8",
      "getWord16be",
      "getWord16le",
      "getWord32be",
      "getWord32le",
      "getWord64be",
      "getWord64le",
      "getInt8",
      "getInt16be",
      "getInt16le",
      "getInt32be",
      "getInt32le",
      "getInt64be",
      "getInt64le",
      "getByteString",
      "getLazyByteString",
      "getDoublebe",
      "getDoublele",
      "getFloatbe",
      "getFloatle",
      "skip",
      "bytesRead",
      "isEmpty",
      "lookAhead",
      "Get",
      "Decoder"
    ]
    ["Get", "Decoder"]

dataBinaryPutScope :: Scope
dataBinaryPutScope =
  bootScope
    "Data.Binary.Put"
    [ "runPut",
      "runPutM",
      "execPut",
      "putWord8",
      "putWord16be",
      "putWord16le",
      "putWord32be",
      "putWord32le",
      "putWord64be",
      "putWord64le",
      "putInt8",
      "putInt16be",
      "putInt16le",
      "putInt32be",
      "putInt32le",
      "putInt64be",
      "putInt64le",
      "putByteString",
      "putLazyByteString",
      "putDoublebe",
      "putDoublele",
      "putFloatbe",
      "putFloatle",
      "Put",
      "PutM"
    ]
    ["Put", "PutM"]

transformersExports :: ModuleExports
transformersExports =
  Map.fromList
    [ ("Control.Monad.Trans.Class", controlMonadTransClassScope),
      ("Control.Monad.Trans.Maybe", controlMonadTransMaybeScope),
      ("Control.Monad.Trans.Either", controlMonadTransEitherScope),
      ("Control.Monad.Trans.Except", controlMonadTransExceptScope),
      ("Control.Monad.Trans.Reader", controlMonadTransReaderScope),
      ("Control.Monad.Trans.Writer", controlMonadTransWriterScope),
      ("Control.Monad.Trans.Writer.Strict", controlMonadTransWriterScope),
      ("Control.Monad.Trans.Writer.Lazy", controlMonadTransWriterScope),
      ("Control.Monad.Trans.Writer.CPS", controlMonadTransWriterScope),
      ("Control.Monad.Trans.State", controlMonadTransStateScope),
      ("Control.Monad.Trans.State.Strict", controlMonadTransStateScope),
      ("Control.Monad.Trans.State.Lazy", controlMonadTransStateScope),
      ("Control.Monad.Trans.RWS", controlMonadTransRWSScope),
      ("Control.Monad.Trans.RWS.Strict", controlMonadTransRWSScope),
      ("Control.Monad.Trans.RWS.Lazy", controlMonadTransRWSScope),
      ("Control.Monad.Trans.RWS.CPS", controlMonadTransRWSScope),
      ("Control.Monad.Trans.Cont", controlMonadTransContScope),
      ("Control.Monad.Trans.Identity", controlMonadTransIdentityScope),
      ("Data.Functor.Identity", dataFunctorIdentityScope)
    ]

controlMonadTransClassScope :: Scope
controlMonadTransClassScope =
  bootScope
    "Control.Monad.Trans.Class"
    ["lift"]
    ["MonadTrans"]

controlMonadTransMaybeScope :: Scope
controlMonadTransMaybeScope =
  bootScope
    "Control.Monad.Trans.Maybe"
    ["runMaybeT", "mapMaybeT", "MaybeT"]
    ["MaybeT"]

controlMonadTransEitherScope :: Scope
controlMonadTransEitherScope =
  bootScope
    "Control.Monad.Trans.Either"
    ["runEitherT", "EitherT", "left", "right", "bimapEitherT", "mapLeft", "mapRight"]
    ["EitherT"]

controlMonadTransExceptScope :: Scope
controlMonadTransExceptScope =
  bootScope
    "Control.Monad.Trans.Except"
    [ "runExceptT",
      "mapExceptT",
      "withExceptT",
      "ExceptT",
      "Except",
      "throwE",
      "catchE",
      "except",
      "runExcept",
      "mapExcept",
      "withExcept"
    ]
    ["ExceptT", "Except"]

controlMonadTransReaderScope :: Scope
controlMonadTransReaderScope =
  bootScope
    "Control.Monad.Trans.Reader"
    [ "runReaderT",
      "mapReaderT",
      "withReaderT",
      "ReaderT",
      "Reader",
      "runReader",
      "mapReader",
      "withReader",
      "ask",
      "asks",
      "local",
      "reader"
    ]
    ["ReaderT", "Reader"]

controlMonadTransWriterScope :: Scope
controlMonadTransWriterScope =
  bootScope
    "Control.Monad.Trans.Writer"
    [ "runWriterT",
      "execWriterT",
      "mapWriterT",
      "WriterT",
      "Writer",
      "runWriter",
      "execWriter",
      "mapWriter",
      "tell",
      "listen",
      "listens",
      "pass",
      "censor",
      "writer"
    ]
    ["WriterT", "Writer"]

controlMonadTransStateScope :: Scope
controlMonadTransStateScope =
  bootScope
    "Control.Monad.Trans.State"
    [ "runStateT",
      "evalStateT",
      "execStateT",
      "mapStateT",
      "withStateT",
      "StateT",
      "State",
      "runState",
      "evalState",
      "execState",
      "mapState",
      "withState",
      "get",
      "put",
      "modify",
      "modify'",
      "gets",
      "state"
    ]
    ["StateT", "State"]

controlMonadTransRWSScope :: Scope
controlMonadTransRWSScope =
  bootScope
    "Control.Monad.Trans.RWS"
    [ "runRWST",
      "evalRWST",
      "execRWST",
      "mapRWST",
      "withRWST",
      "RWST",
      "RWS",
      "runRWS",
      "evalRWS",
      "execRWS",
      "mapRWS",
      "withRWS",
      "ask",
      "asks",
      "local",
      "reader",
      "tell",
      "listen",
      "listens",
      "pass",
      "censor",
      "get",
      "put",
      "modify",
      "modify'",
      "gets",
      "state"
    ]
    ["RWST", "RWS"]

controlMonadTransContScope :: Scope
controlMonadTransContScope =
  bootScope
    "Control.Monad.Trans.Cont"
    ["runContT", "mapContT", "withContT", "ContT", "Cont", "runCont", "mapCont", "withCont", "callCC", "cont", "shiftT", "resetT"]
    ["ContT", "Cont"]

controlMonadTransIdentityScope :: Scope
controlMonadTransIdentityScope =
  bootScope
    "Control.Monad.Trans.Identity"
    ["runIdentityT", "mapIdentityT", "IdentityT"]
    ["IdentityT"]

mtlExports :: ModuleExports
mtlExports =
  Map.fromList
    [ ("Control.Monad.Reader", controlMonadTransReaderScope),
      ("Control.Monad.Writer", controlMonadTransWriterScope),
      ("Control.Monad.Writer.Strict", controlMonadTransWriterScope),
      ("Control.Monad.Writer.Lazy", controlMonadTransWriterScope),
      ("Control.Monad.State", controlMonadTransStateScope),
      ("Control.Monad.State.Strict", controlMonadTransStateScope),
      ("Control.Monad.State.Lazy", controlMonadTransStateScope),
      ("Control.Monad.RWS", controlMonadTransRWSScope),
      ("Control.Monad.RWS.Strict", controlMonadTransRWSScope),
      ("Control.Monad.Except", controlMonadTransExceptScope),
      ("Control.Monad.Trans", controlMonadTransClassScope),
      ("Control.Monad.Trans.Class", controlMonadTransClassScope),
      ("Control.Monad.Identity", controlMonadTransIdentityScope),
      ("Control.Monad.Cont", controlMonadTransContScope),
      ("Control.Monad.Reader.Class", mtlReaderClassScope),
      ("Control.Monad.Writer.Class", mtlWriterClassScope),
      ("Control.Monad.State.Class", mtlStateClassScope),
      ("Control.Monad.Error", mtlErrorScope),
      ("Control.Monad.Error.Class", mtlErrorClassScope)
    ]

mtlReaderClassScope :: Scope
mtlReaderClassScope =
  bootScope
    "Control.Monad.Reader.Class"
    ["ask", "asks", "local", "reader"]
    ["MonadReader"]

mtlWriterClassScope :: Scope
mtlWriterClassScope =
  bootScope
    "Control.Monad.Writer.Class"
    ["tell", "listen", "listens", "pass", "censor", "writer"]
    ["MonadWriter"]

mtlStateClassScope :: Scope
mtlStateClassScope =
  bootScope
    "Control.Monad.State.Class"
    ["get", "put", "modify", "modify'", "gets", "state"]
    ["MonadState"]

mtlErrorScope :: Scope
mtlErrorScope =
  bootScope
    "Control.Monad.Error"
    ["throwError", "catchError", "runErrorT", "mapErrorT", "ErrorT"]
    ["MonadError", "ErrorT", "Error"]

mtlErrorClassScope :: Scope
mtlErrorClassScope =
  bootScope
    "Control.Monad.Error.Class"
    ["throwError", "catchError"]
    ["MonadError"]

stmExports :: ModuleExports
stmExports =
  Map.fromList
    [ ("Control.Concurrent.STM", controlStmScope),
      ("Control.Concurrent.STM.TVar", controlStmTVarScope),
      ("Control.Concurrent.STM.TMVar", controlStmTMVarScope),
      ("Control.Concurrent.STM.TChan", controlStmTChanScope),
      ("Control.Concurrent.STM.TQueue", controlStmTQueueScope),
      ("Control.Concurrent.STM.TBQueue", controlStmTBQueueScope)
    ]

unixExports :: ModuleExports
unixExports =
  Map.fromList
    [ ("System.Posix", systemPosixScope),
      ("System.Posix.Files", systemPosixFilesScope),
      ("System.Posix.Directory", systemPosixDirectoryScope),
      ("System.Posix.Process", systemPosixProcessScope),
      ("System.Posix.IO", systemPosixIoScope),
      ("System.Posix.Signals", systemPosixSignalsScope),
      ("System.Posix.Types", systemPosixTypesScope),
      ("System.Posix.Env", systemPosixEnvScope),
      ("System.Posix.User", systemPosixUserScope),
      ("System.Posix.Temp", systemPosixTempScope),
      ("System.Posix.Unistd", systemPosixUnistdScope),
      ("System.Posix.DynamicLinker", systemPosixDynamicLinkerScope)
    ]

systemPosixScope :: Scope
systemPosixScope =
  bootScope
    "System.Posix"
    ["getEnvironment"]
    []

systemPosixFilesScope :: Scope
systemPosixFilesScope =
  bootScope
    "System.Posix.Files"
    [ "getFileStatus",
      "getSymbolicLinkStatus",
      "fileExist",
      "accessTime",
      "modificationTime",
      "fileSize",
      "fileMode",
      "fileOwner",
      "fileGroup",
      "isDirectory",
      "isRegularFile",
      "isSymbolicLink",
      "isNamedPipe",
      "isSocket",
      "isCharacterDevice",
      "isBlockDevice",
      "setFileMode",
      "setOwnerAndGroup",
      "createLink",
      "createSymbolicLink",
      "removeLink",
      "rename",
      "createDirectory",
      "removeDirectory",
      "unionFileModes",
      "intersectFileModes",
      "ownerReadMode",
      "ownerWriteMode",
      "ownerExecuteMode",
      "groupReadMode",
      "groupWriteMode",
      "groupExecuteMode",
      "otherReadMode",
      "otherWriteMode",
      "otherExecuteMode",
      "FileStatus"
    ]
    ["FileStatus", "FileMode"]

systemPosixDirectoryScope :: Scope
systemPosixDirectoryScope =
  bootScope
    "System.Posix.Directory"
    [ "openDirStream",
      "readDirStream",
      "rewindDirStream",
      "closeDirStream",
      "getWorkingDirectory",
      "changeWorkingDirectory",
      "DirStream"
    ]
    ["DirStream"]

systemPosixProcessScope :: Scope
systemPosixProcessScope =
  bootScope
    "System.Posix.Process"
    [ "forkProcess",
      "executeFile",
      "getProcessID",
      "getParentProcessID",
      "getProcessGroupID",
      "createSession",
      "setProcessGroupID",
      "getProcessTimes",
      "nice",
      "scheduleAlarm",
      "sleep",
      "getpid",
      "ProcessID",
      "ProcessGroupID"
    ]
    ["ProcessID", "ProcessGroupID", "ProcessTimes"]

systemPosixIoScope :: Scope
systemPosixIoScope =
  bootScope
    "System.Posix.IO"
    [ "openFd",
      "closeFd",
      "dupFd",
      "dup",
      "dup2",
      "createPipe",
      "fdRead",
      "fdWrite",
      "fdSeek",
      "fdGetMode",
      "handleToFd",
      "fdToHandle",
      "setFdOption",
      "getFdStatus",
      "queryFdOption",
      "Fd",
      "OpenMode",
      "OpenFileFlags",
      "FdOption",
      "SeekMode"
    ]
    ["Fd", "OpenMode", "OpenFileFlags"]

systemPosixSignalsScope :: Scope
systemPosixSignalsScope =
  bootScope
    "System.Posix.Signals"
    [ "installHandler",
      "raiseSignal",
      "signalProcess",
      "signalProcessGroup",
      "getSignalMask",
      "setSignalMask",
      "blockSignals",
      "unblockSignals",
      "sigHUP",
      "sigINT",
      "sigKILL",
      "sigPIPE",
      "sigQUIT",
      "sigTERM",
      "sigUSR1",
      "sigUSR2",
      "sigCHLD",
      "sigCONT",
      "sigSTOP",
      "sigTSTP",
      "sigALRM",
      "sigSEGV",
      "sigFPE",
      "sigILL",
      "sigBUS",
      "sigABRT",
      "Signal",
      "Handler",
      "SignalSet"
    ]
    ["Signal", "Handler", "SignalSet"]

systemPosixEnvScope :: Scope
systemPosixEnvScope =
  bootScope
    "System.Posix.Env"
    ["getEnvironment", "getEnv", "putEnv", "setEnv", "unsetEnv", "getEnvDefault"]
    []

systemPosixUserScope :: Scope
systemPosixUserScope =
  bootScope
    "System.Posix.User"
    [ "getRealUserID",
      "getEffectiveUserID",
      "getRealGroupID",
      "getEffectiveGroupID",
      "setUserID",
      "setGroupID",
      "getUserEntryForName",
      "getUserEntryForID",
      "getGroupEntryForName",
      "getGroupEntryForID",
      "getAllUserEntries",
      "getAllGroupEntries",
      "userName",
      "userPassword",
      "userID",
      "userGroupID",
      "userGecos",
      "userHomeDirectory",
      "userShell",
      "groupName",
      "groupPassword",
      "groupID",
      "groupMembers",
      "UserEntry",
      "GroupEntry"
    ]
    ["UserEntry", "GroupEntry"]

systemPosixTempScope :: Scope
systemPosixTempScope =
  bootScope
    "System.Posix.Temp"
    ["mkstemp", "mkstemps", "mkdtemp"]
    []

systemPosixUnistdScope :: Scope
systemPosixUnistdScope =
  bootScope
    "System.Posix.Unistd"
    [ "sleep",
      "usleep",
      "nanosleep",
      "getSystemID",
      "getClockTime",
      "setEnvironment",
      "putEnv",
      "setEnv",
      "unsetEnv",
      "SystemID",
      "ClockTick",
      "EpochTime"
    ]
    ["SystemID"]

systemPosixDynamicLinkerScope :: Scope
systemPosixDynamicLinkerScope =
  bootScope
    "System.Posix.DynamicLinker"
    ["dlopen", "dlsym", "dlclose", "dlerror", "DL", "RTLDFlags"]
    ["DL", "RTLDFlags"]

win32Exports :: ModuleExports
win32Exports =
  Map.fromList
    [ ("System.Win32.Types", systemWin32TypesScope),
      ("System.Win32.File", systemWin32FileScope),
      ("System.Win32.Process", systemWin32ProcessScope),
      ("System.Win32.Console", systemWin32ConsoleScope)
    ]

systemWin32TypesScope :: Scope
systemWin32TypesScope =
  bootScope
    "System.Win32.Types"
    [ "nullHANDLE",
      "iNVALID_HANDLE_VALUE",
      "HANDLE",
      "BOOL",
      "BYTE",
      "WORD",
      "DWORD",
      "LONG",
      "LPVOID",
      "LPCVOID",
      "LPSTR",
      "LPCSTR",
      "LPWSTR",
      "LPCWSTR",
      "LPTSTR",
      "LPCTSTR",
      "LPDWORD",
      "LPBOOL",
      "LPBYTE",
      "LPWORD",
      "LPLONG",
      "INT",
      "UINT",
      "HMODULE",
      "HINSTANCE",
      "HRESULT",
      "HKEY",
      "HGLOBAL",
      "HLOCAL",
      "HWND",
      "HDC",
      "HFONT",
      "HMENU",
      "WPARAM",
      "LPARAM",
      "LRESULT",
      "COMPUTER_NAME_FORMAT"
    ]
    [ "HANDLE",
      "BOOL",
      "BYTE",
      "WORD",
      "DWORD",
      "LONG",
      "LPVOID",
      "LPCVOID",
      "LPSTR",
      "LPCSTR",
      "LPWSTR",
      "LPCWSTR",
      "LPTSTR",
      "LPCTSTR",
      "LPDWORD",
      "LPBOOL",
      "LPBYTE",
      "LPWORD",
      "LPLONG",
      "INT",
      "UINT",
      "HMODULE",
      "HINSTANCE",
      "HRESULT",
      "HKEY",
      "HGLOBAL",
      "HLOCAL",
      "HWND",
      "HDC",
      "HFONT",
      "HMENU",
      "WPARAM",
      "LPARAM",
      "LRESULT",
      "COMPUTER_NAME_FORMAT"
    ]

systemWin32FileScope :: Scope
systemWin32FileScope =
  bootScope
    "System.Win32.File"
    [ "createFile",
      "closeHandle",
      "readFile",
      "writeFile",
      "gENERIC_READ",
      "gENERIC_WRITE",
      "oPEN_EXISTING",
      "cREATE_ALWAYS",
      "fILE_SHARE_READ",
      "fILE_SHARE_WRITE"
    ]
    []

systemWin32ProcessScope :: Scope
systemWin32ProcessScope =
  bootScope
    "System.Win32.Process"
    ["getCurrentProcess", "getProcessId", "terminateProcess", "exitProcess", "getExitCodeProcess"]
    []

systemWin32ConsoleScope :: Scope
systemWin32ConsoleScope =
  bootScope
    "System.Win32.Console"
    ["getStdHandle", "setConsoleTitle", "setConsoleTextAttribute", "getConsoleScreenBufferInfo", "setConsoleCursorPosition"]
    []

directoryExports :: ModuleExports
directoryExports =
  Map.fromList
    [ ("System.Directory", systemDirectoryScope),
      ("System.Directory.Internal", systemDirectoryScope)
    ]

systemDirectoryScope :: Scope
systemDirectoryScope =
  bootScope
    "System.Directory"
    [ "createDirectory",
      "createDirectoryIfMissing",
      "removeDirectory",
      "removeDirectoryRecursive",
      "renameDirectory",
      "getDirectoryContents",
      "listDirectory",
      "getCurrentDirectory",
      "setCurrentDirectory",
      "getHomeDirectory",
      "getTemporaryDirectory",
      "doesDirectoryExist",
      "doesFileExist",
      "doesPathExist",
      "removeFile",
      "renameFile",
      "copyFile",
      "copyFileWithMetadata",
      "getFileSize",
      "getModificationTime",
      "getAccessTime",
      "findFile",
      "findFiles",
      "findFileWith",
      "findFilesWith",
      "findExecutable",
      "findExecutables",
      "findExecutableInDirectories",
      "makeAbsolute",
      "makeRelativeToCurrentDirectory",
      "canonicalizePath",
      "getXdgDirectory",
      "getXdgDirectoryList",
      "exeExtension",
      "Permissions",
      "getPermissions",
      "setPermissions",
      "copyPermissions",
      "XdgDirectory",
      "XdgDirectoryList"
    ]
    ["Permissions", "XdgDirectory", "XdgDirectoryList"]

filepathExports :: ModuleExports
filepathExports =
  Map.fromList
    [ ("System.FilePath", systemFilepathScope),
      ("System.FilePath.Posix", systemFilepathScope),
      ("System.FilePath.Windows", systemFilepathScope)
    ]

systemFilepathScope :: Scope
systemFilepathScope =
  bootScope
    "System.FilePath"
    [ "(</>)",
      "(<.>)",
      "(-<.>)",
      "takeDirectory",
      "takeFileName",
      "takeBaseName",
      "takeExtension",
      "takeExtensions",
      "dropExtension",
      "dropExtensions",
      "addExtension",
      "replaceExtension",
      "replaceBaseName",
      "replaceDirectory",
      "replaceFileName",
      "normalise",
      "makeValid",
      "makeRelative",
      "makeAbsolute",
      "splitFileName",
      "splitPath",
      "splitDirectories",
      "splitSearchPath",
      "joinPath",
      "isAbsolute",
      "isRelative",
      "isDrive",
      "isExtensionOf",
      "hasDrive",
      "hasExtension",
      "hasTrailingPathSeparator",
      "addTrailingPathSeparator",
      "dropTrailingPathSeparator",
      "isPathSeparator",
      "pathSeparator",
      "pathSeparators",
      "extSeparator",
      "isExtSeparator",
      "searchPathSeparator",
      "isSearchPathSeparator",
      "FilePath"
    ]
    []

processExports :: ModuleExports
processExports =
  Map.fromList
    [ ("System.Process", systemProcessScope),
      ("System.Process.Typed", systemProcessTypedScope)
    ]

systemProcessScope :: Scope
systemProcessScope =
  bootScope
    "System.Process"
    [ "createProcess",
      "createProcess_",
      "withCreateProcess",
      "runProcess",
      "runCommand",
      "runInteractiveProcess",
      "runInteractiveCommand",
      "system",
      "rawSystem",
      "readProcess",
      "readProcessWithExitCode",
      "callProcess",
      "callCommand",
      "spawnProcess",
      "spawnCommand",
      "terminateProcess",
      "interruptProcessGroupOf",
      "waitForProcess",
      "getProcessExitCode",
      "proc",
      "shell",
      "ProcessHandle",
      "CreateProcess",
      "CmdSpec",
      "StdStream",
      "StdStream",
      "Inherit",
      "UseHandle",
      "CreatePipe",
      "std_in",
      "std_out",
      "std_err",
      "cwd",
      "env",
      "close_fds",
      "create_group",
      "delegate_ctlc"
    ]
    ["ProcessHandle", "CreateProcess", "CmdSpec", "StdStream", "ExitCode"]

systemProcessTypedScope :: Scope
systemProcessTypedScope =
  bootScope
    "System.Process.Typed"
    [ "proc",
      "shell",
      "runProcess",
      "readProcess",
      "readProcessStdout",
      "readProcessStderr",
      "startProcess",
      "stopProcess",
      "withProcess",
      "setStdin",
      "setStdout",
      "setStderr",
      "setCwd",
      "setEnv",
      "ProcessConfig",
      "Process"
    ]
    ["ProcessConfig", "Process"]

networkExports :: ModuleExports
networkExports =
  Map.fromList
    [ ("Network.Socket", networkSocketScope)
    ]

networkSocketScope :: Scope
networkSocketScope =
  bootScope
    "Network.Socket"
    [ "socket",
      "bind",
      "connect",
      "listen",
      "accept",
      "close",
      "shutdown",
      "send",
      "recv",
      "sendTo",
      "recvFrom",
      "getAddrInfo",
      "getNameInfo",
      "setSocketOption",
      "getSocketOption",
      "getSockName",
      "getPeerName",
      "inet_addr",
      "inet_ntoa",
      "htonl",
      "ntohl",
      "htons",
      "ntohs",
      "withSocketsDo",
      "socketPair",
      "Socket",
      "SockAddr",
      "AddrInfo",
      "SocketType",
      "Family",
      "ProtocolNumber",
      "PortNumber",
      "HostName",
      "ServiceName",
      "AF_INET",
      "AF_INET6",
      "AF_UNIX",
      "SOCK_STREAM",
      "SOCK_DGRAM",
      "SOCK_RAW",
      "defaultProtocol"
    ]
    ["Socket", "SockAddr", "AddrInfo", "SocketType", "Family", "PortNumber", "HostName", "ServiceName"]

randomExports :: ModuleExports
randomExports =
  Map.fromList
    [ ("System.Random", systemRandomScope)
    ]

systemRandomScope :: Scope
systemRandomScope =
  bootScope
    "System.Random"
    [ "random",
      "randomR",
      "randoms",
      "randomRs",
      "randomIO",
      "randomRIO",
      "mkStdGen",
      "newStdGen",
      "getStdGen",
      "setStdGen",
      "getStdRandom",
      "split",
      "next",
      "genRange",
      "StdGen",
      "Random",
      "RandomGen",
      "Uniform",
      "UniformRange"
    ]
    ["StdGen", "Random", "RandomGen"]

parsecExports :: ModuleExports
parsecExports =
  Map.fromList
    [ ("Text.Parsec", textParsecScope),
      ("Text.ParserCombinators.Parsec", textParsecScope)
    ]

textParsecScope :: Scope
textParsecScope =
  bootScope
    "Text.Parsec"
    [ "parse",
      "parseTest",
      "runParser",
      "runParserT",
      "try",
      "many",
      "many1",
      "option",
      "optional",
      "between",
      "choice",
      "count",
      "skipMany",
      "skipMany1",
      "sepBy",
      "sepBy1",
      "manyTill",
      "manyTill_",
      "anyTill",
      "endBy",
      "endBy1",
      "sepEndBy",
      "sepEndBy1",
      "chainl",
      "chainl1",
      "chainr",
      "chainr1",
      "eof",
      "notFollowedBy",
      "lookAhead",
      "anyToken",
      "char",
      "string",
      "digit",
      "letter",
      "alphaNum",
      "space",
      "spaces",
      "newline",
      "tab",
      "upper",
      "lower",
      "oneOf",
      "noneOf",
      "satisfy",
      "hexDigit",
      "octDigit",
      "anyChar",
      "label",
      "labels",
      "(<?>)",
      "getPosition",
      "setPosition",
      "getInput",
      "setInput",
      "getState",
      "putState",
      "modifyState",
      "unexpected",
      "ParseError",
      "SourcePos",
      "sourceLine",
      "sourceColumn",
      "sourceName",
      "Parsec",
      "ParsecT",
      "Stream"
    ]
    ["ParseError", "SourcePos", "Parsec", "ParsecT", "Stream", "ParseError"]

prettyExports :: ModuleExports
prettyExports =
  Map.fromList
    [ ("Text.PrettyPrint", textPrettyPrintScope),
      ("Text.PrettyPrint.HughesPJ", textPrettyPrintScope),
      ("Text.PrettyPrint.HughesPJClass", textPrettyPrintScope)
    ]

textPrettyPrintScope :: Scope
textPrettyPrintScope =
  bootScope
    "Text.PrettyPrint"
    [ "render",
      "renderStyle",
      "fullRender",
      "prettyShow",
      "text",
      "char",
      "ptext",
      "sizedText",
      "zeroWidthText",
      "int",
      "integer",
      "float",
      "double",
      "rational",
      "parens",
      "brackets",
      "braces",
      "quotes",
      "doubleQuotes",
      "empty",
      "nest",
      "hang",
      "punctuate",
      "(<>)",
      "(<+>)",
      "($$)",
      "($+$)",
      "hcat",
      "hsep",
      "vcat",
      "fsep",
      "fcat",
      "cat",
      "sep",
      "comma",
      "colon",
      "semi",
      "space",
      "equals",
      "lbrace",
      "rbrace",
      "lparen",
      "rparen",
      "lbrack",
      "rbrack",
      "pp",
      "Doc",
      "Style",
      "Mode",
      "TextDetails"
    ]
    ["Doc", "Style", "Mode", "TextDetails"]

dataSequenceScope :: Scope
dataSequenceScope =
  bootScope
    "Data.Sequence"
    [ "empty",
      "singleton",
      "fromList",
      "toList",
      "length",
      "null",
      "<|",
      "|>",
      "><",
      "index",
      "adjust",
      "adjust'",
      "take",
      "drop",
      "splitAt",
      "takeWhileL",
      "takeWhileR",
      "dropWhileL",
      "dropWhileR",
      "spanl",
      "spanr",
      "breakl",
      "breakr",
      "viewl",
      "viewr",
      "EmptyL",
      "EmptyR",
      ":<",
      ":>",
      "sort",
      "sortBy",
      "sortOn",
      "unstableSort",
      "unstableSortBy",
      "foldlWithIndex",
      "foldrWithIndex",
      "mapWithIndex",
      "traverseWithIndex",
      "findIndicesL",
      "findIndicesR",
      "findIndexL",
      "findIndexR",
      "elemIndicesL",
      "elemIndicesR",
      "elemIndexL",
      "elemIndexR",
      "replicate",
      "replicateA",
      "replicateM",
      "cycleTaking",
      "iterateN",
      "unfoldr",
      "scanl",
      "scanl1",
      "scanr",
      "scanr1",
      "reverse",
      "intersperse",
      "Seq"
    ]
    ["Seq", "ViewL", "ViewR"]

dataHashMapScope :: Scope
dataHashMapScope =
  bootScope
    "Data.HashMap.Strict"
    [ "empty",
      "singleton",
      "fromList",
      "fromListWith",
      "toList",
      "keys",
      "elems",
      "insert",
      "insertWith",
      "delete",
      "adjust",
      "update",
      "alter",
      "lookup",
      "findWithDefault",
      "member",
      "notMember",
      "null",
      "size",
      "union",
      "unionWith",
      "unions",
      "unionsWith",
      "difference",
      "intersection",
      "intersectionWith",
      "map",
      "mapWithKey",
      "filter",
      "filterWithKey",
      "foldl'",
      "foldr",
      "foldlWithKey'",
      "foldrWithKey",
      "traverseWithKey",
      "HashMap"
    ]
    ["HashMap"]

dataHashSetScope :: Scope
dataHashSetScope =
  bootScope
    "Data.HashSet"
    [ "empty",
      "singleton",
      "fromList",
      "toList",
      "insert",
      "delete",
      "member",
      "notMember",
      "null",
      "size",
      "union",
      "unions",
      "difference",
      "intersection",
      "isSubsetOf",
      "map",
      "filter",
      "foldl'",
      "foldr",
      "HashSet"
    ]
    ["HashSet"]

dataIntMapScope :: Scope
dataIntMapScope =
  bootScope
    "Data.IntMap.Strict"
    [ "empty",
      "singleton",
      "fromList",
      "fromListWith",
      "toList",
      "toAscList",
      "fromAscList",
      "fromAscListWith",
      "fromDescList",
      "fromSet",
      "keys",
      "elems",
      "insert",
      "insertWith",
      "delete",
      "adjust",
      "update",
      "alter",
      "lookup",
      "findWithDefault",
      "member",
      "notMember",
      "null",
      "size",
      "union",
      "unionWith",
      "unions",
      "difference",
      "intersection",
      "intersectionWith",
      "map",
      "mapWithKey",
      "filter",
      "filterWithKey",
      "foldl",
      "foldl'",
      "foldr",
      "foldr'",
      "foldlWithKey",
      "foldlWithKey'",
      "foldrWithKey",
      "traverseWithKey",
      "mapAccum",
      "mapAccumWithKey",
      "keysSet",
      "restrictKeys",
      "withoutKeys",
      "findMin",
      "findMax",
      "deleteMin",
      "deleteMax",
      "minView",
      "maxView",
      "minViewWithKey",
      "maxViewWithKey",
      "IntMap",
      "(!)"
    ]
    ["IntMap"]

dataIntSetScope :: Scope
dataIntSetScope =
  bootScope
    "Data.IntSet"
    [ "empty",
      "singleton",
      "fromList",
      "toList",
      "toAscList",
      "insert",
      "delete",
      "member",
      "notMember",
      "null",
      "size",
      "union",
      "unions",
      "difference",
      "intersection",
      "isSubsetOf",
      "map",
      "filter",
      "foldl",
      "foldl'",
      "foldr",
      "foldr'",
      "IntSet"
    ]
    ["IntSet"]

dataArrayScope :: Scope
dataArrayScope =
  bootScope
    "Data.Array"
    [ "array",
      "listArray",
      "accumArray",
      "bounds",
      "indices",
      "elems",
      "assocs",
      "(//)",
      "accum",
      "ixRange",
      "rangeSize",
      "inRange",
      "index",
      "Array"
    ]
    ["Array", "Ix"]

dataIxScope :: Scope
dataIxScope =
  bootScope
    "Data.Ix"
    ["range", "index", "inRange", "rangeSize"]
    ["Ix"]

dataTupleSoloScope :: Scope
dataTupleSoloScope =
  bootScope
    "Data.Tuple.Solo"
    ["getSolo", "MkSolo", "Solo"]
    ["Solo"]

ghcTupleScope :: Scope
ghcTupleScope =
  bootScope
    "GHC.Tuple"
    ["fst", "snd", "swap"]
    ["Solo"]

textPrintfScope :: Scope
textPrintfScope =
  bootScope
    "Text.Printf"
    ["printf", "hPrintf", "vprintf", "hVprintf", "sprintf", "hSprintf"]
    ["PrintfType", "PrintfArg", "HPrintfType", "IsChar"]

dataVersionScope :: Scope
dataVersionScope =
  bootScope
    "Data.Version"
    ["makeVersion", "parseVersion", "showVersion", "versionBranch", "versionTags"]
    ["Version"]

foreignConcurrentScope :: Scope
foreignConcurrentScope =
  bootScope
    "Foreign.Concurrent"
    ["newForeignPtr", "addForeignPtrFinalizer"]
    ["ForeignPtr"]

dataListSplitScope :: Scope
dataListSplitScope =
  bootScope
    "Data.List.Split"
    ["splitOn", "splitWhen", "wordsBy", "linesBy", "chunksOf", "splitOneOf", "splitEvery"]
    []

systemPosixTypesScope :: Scope
systemPosixTypesScope =
  bootScope
    "System.Posix.Types"
    []
    [ "CDev",
      "CIno",
      "CMode",
      "COff",
      "CPid",
      "CSsize",
      "CGid",
      "CNlink",
      "CUid",
      "CCc",
      "CSpeed",
      "CTcflag",
      "CRLim",
      "Fd",
      "DeviceID",
      "FileID",
      "FileMode",
      "ByteCount",
      "ProcessID",
      "UserID",
      "GroupID",
      "FileOffset",
      "LinkCount",
      "ClockTick",
      "EpochTime",
      "Limit"
    ]

dataMapScope :: Scope
dataMapScope =
  bootScope
    "Data.Map.Strict"
    [ "empty",
      "singleton",
      "fromList",
      "fromListWith",
      "fromListWithKey",
      "fromAscList",
      "fromAscListWith",
      "fromAscListWithKey",
      "fromDescList",
      "fromDescListWith",
      "fromDescListWithKey",
      "fromSet",
      "toList",
      "toAscList",
      "toDescList",
      "keys",
      "elems",
      "assocs",
      "insert",
      "insertWith",
      "insertWithKey",
      "insertLookupWithKey",
      "delete",
      "adjust",
      "adjustWithKey",
      "update",
      "updateWithKey",
      "alter",
      "lookup",
      "findWithDefault",
      "member",
      "notMember",
      "null",
      "size",
      "union",
      "unionWith",
      "unionWithKey",
      "unions",
      "unionsWith",
      "difference",
      "differenceWith",
      "differenceWithKey",
      "intersection",
      "intersectionWith",
      "intersectionWithKey",
      "map",
      "mapWithKey",
      "mapKeys",
      "mapKeysWith",
      "filter",
      "filterWithKey",
      "partition",
      "partitionWithKey",
      "foldl",
      "foldl'",
      "foldr",
      "foldr'",
      "foldlWithKey",
      "foldlWithKey'",
      "foldrWithKey",
      "foldrWithKey'",
      "foldMapWithKey",
      "traverseWithKey",
      "mapAccum",
      "mapAccumWithKey",
      "mapAccumRWithKey",
      "keysSet",
      "restrictKeys",
      "withoutKeys",
      "findMin",
      "findMax",
      "deleteMin",
      "deleteMax",
      "minView",
      "maxView",
      "minViewWithKey",
      "maxViewWithKey",
      "updateMin",
      "updateMax",
      "updateMinWithKey",
      "updateMaxWithKey",
      "splitLookup",
      "split",
      "isSubmapOf",
      "isSubmapOfBy",
      "mergeWithKey",
      "merge",
      "Map",
      "(!)",
      "(!?)"
    ]
    ["Map"]

ghcPrimExports :: ModuleExports
ghcPrimExports =
  Map.fromList
    [ ("GHC.Prim", ghcPrimScope),
      ("GHC.Prim.Ext", ghcPrimExtScope)
    ]

ghcPrimScope :: Scope
ghcPrimScope =
  bootScope
    "GHC.Prim"
    [ "seq#",
      "void#",
      "realWorld#",
      "coerce#",
      "negateInt#",
      "negateWord#",
      "not#",
      "narrow8Int#",
      "narrow16Int#",
      "narrow32Int#",
      "narrow8Word#",
      "narrow16Word#",
      "narrow32Word#"
    ]
    [ "Int#",
      "Word#",
      "Char#",
      "Float#",
      "Double#",
      "Addr#",
      "State#",
      "RealWorld",
      "MutVar#",
      "MutableByteArray#",
      "ByteArray#",
      "Array#",
      "MutableArray#",
      "SmallArray#",
      "SmallMutableArray#",
      "StablePtr#",
      "StableName#",
      "MVar#",
      "TVar#",
      "BCO#",
      "ThreadId#",
      "Weak#",
      "Proxy#"
    ]

ghcPrimExtScope :: Scope
ghcPrimExtScope =
  bootScope
    "GHC.Prim.Ext"
    []
    []

integerExports :: ModuleExports
integerExports =
  Map.fromList
    [ ("GHC.Integer", integerScope),
      ("GHC.Integer.Type", integerScope),
      ("GHC.Num.Integer", integerScope),
      ("GHC.Num.Natural", naturalScope),
      ("GHC.Natural", naturalScope),
      ("Numeric.Natural", naturalScope)
    ]

integerScope :: Scope
integerScope =
  bootScope
    "GHC.Integer"
    [ "smallInteger",
      "wordToInteger",
      "integerToWord",
      "integerToInt",
      "integerToWord64",
      "integerToInt64",
      "word64ToInteger",
      "int64ToInteger",
      "plusInteger",
      "minusInteger",
      "timesInteger",
      "negateInteger",
      "absInteger",
      "signumInteger",
      "divModInteger",
      "quotRemInteger",
      "quotInteger",
      "remInteger",
      "divInteger",
      "modInteger",
      "gcdInteger",
      "lcmInteger",
      "andInteger",
      "orInteger",
      "xorInteger",
      "complementInteger",
      "shiftLInteger",
      "shiftRInteger",
      "testBitInteger",
      "popCountInteger",
      "bitInteger"
    ]
    ["Integer"]

naturalScope :: Scope
naturalScope =
  bootScope
    "Numeric.Natural"
    []
    ["Natural"]

-- ---------------------------------------------------------------------------
-- main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  opts0 <- parseOptions args
  jobs <- if optJobs opts0 == 0 then getNumProcessors else pure (optJobs opts0)
  let opts = opts0 {optJobs = jobs}

  snapshotResult <- loadStackageSnapshot Nothing (optSnapshot opts) (optOffline opts)
  packages <- case snapshotResult of
    Left err -> hPutStrLn stderr ("Failed to load snapshot: " ++ err) >> exitFailure
    Right pkgs -> pure pkgs

  let total = length packages
      snapshotNames = Set.fromList (map (T.pack . pkgName) packages)
      -- GHC boot packages use CPP and internal syntax our parser can't handle.
      -- Pre-stub them as successful with hand-written export shims.
      (bootPkgs, regularPkgs) = partition (\p -> pkgVersion p == "installed") packages
      bootResults =
        Map.fromList
          [ (T.pack (pkgName p), PkgSuccess (bootPackageExports (T.pack (pkgName p))))
          | p <- bootPkgs
          ]

  isStdoutTerminal <- hIsTerminalDevice stdout
  putStrLn $ "Resolving " ++ optSnapshot opts ++ " (" ++ show total ++ " packages, " ++ show jobs ++ " jobs)..."
  putStrLn $ "  " ++ show (length bootPkgs) ++ " GHC boot packages stubbed, " ++ show (length regularPkgs) ++ " to download"
  putStrLn "Phase 1: downloading packages and collecting dependency info..."

  infos <- phase1Parallel opts regularPkgs snapshotNames isStdoutTerminal (length regularPkgs)

  putStrLn "\nPhase 2: resolving packages in dependency order..."

  let depGraph =
        Map.fromList
          [(name, piSnapshotDeps info) | (name, info) <- Map.toList infos]
      layers = kahnLayers depGraph

  -- Warn about cycles (packages not covered by Kahn's layers)
  let coveredPkgs = Set.fromList (concat layers)
      cyclePkgs = Set.difference (Set.fromList (map (T.pack . pkgName) regularPkgs)) coveredPkgs
  if Set.null cyclePkgs
    then pure ()
    else hPutStrLn stderr ("Warning: " ++ show (Set.size cyclePkgs) ++ " packages in dep cycles, resolving without dep interfaces")

  results <- processLayers opts layers infos depGraph bootResults

  reportResults (optTopFailures opts) results

phase1Parallel :: Options -> [PackageSpec] -> Set Text -> Bool -> Int -> IO (Map Text PackageInfo)
phase1Parallel opts packages snapshotNames showProgress total = do
  queue <- newChan
  mapM_ (writeChan queue . Just) packages
  replicateM_ workerCount (writeChan queue Nothing)
  resultVar <- newMVar Map.empty
  progressVar <- newMVar (0 :: Int)
  let worker = do
        next <- readChan queue
        case next of
          Nothing -> pure ()
          Just spec -> do
            outcome <- try (collectPackageInfo (optOffline opts) spec snapshotNames)
            let (name, info) = case outcome of
                  Left (_ :: SomeException) ->
                    ( T.pack (pkgName spec),
                      PackageInfo "" [] []
                    )
                  Right r -> r
            modifyMVar_ resultVar (pure . Map.insert name info)
            done <- modifyMVar progressVar (\d -> let d' = d + 1 in pure (d', d'))
            Control.Monad.when showProgress $ putProgressLine done total
            worker
  replicateConcurrently_ workerCount worker
  readMVar resultVar
  where
    workerCount = max 1 (optJobs opts)

replicateM_ :: Int -> IO () -> IO ()
replicateM_ 0 _ = pure ()
replicateM_ n action = action >> replicateM_ (n - 1) action
