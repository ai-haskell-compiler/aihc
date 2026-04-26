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
    SourceSpan (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    parseExtensionSettingName,
    parseLanguageEdition,
  )
import Aihc.Resolve (ModuleExports, ResolveError (..), ResolveResult (..), extractInterface, resolveWithDeps)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (mplus)
import Control.Monad qualified
import Data.ByteString qualified as BS
import Data.Char (toLower)
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
      | BS.index bytes (i - 1) == 10 = i  -- '\n'
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
       in pad ++ " |\n"
            ++ lineNumStr ++ " | " ++ lineText ++ "\n"
            ++ pad ++ " | " ++ replicate caretStart ' ' ++ carets ++ "\n"

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
      mapM_ (\l -> putStrLn ("    " ++ l)) (take 3 (lines msg))

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
      -- Pre-stub them as successful with empty exports so their dependents aren't skipped.
      (bootPkgs, regularPkgs) = partition (\p -> pkgVersion p == "installed") packages
      bootResults = Map.fromList [(T.pack (pkgName p), PkgSuccess Map.empty) | p <- bootPkgs]

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
