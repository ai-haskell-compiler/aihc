{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TcStackageProgress
  ( Options (..),
    PackageCounts (..),
    optionsParser,
    processLayers,
    summarizePackageStatuses,
    typecheckOnePackage,
    run,
  )
where

import Aihc.Hackage.Stackage (loadStackageSnapshot)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Resolve (ModuleExports, ResolveResult (..), extractInterface, resolveWithDeps)
import Aihc.Tc (TcModuleResult (..), typecheck)
import BootInterface (bootPackageNames, loadBootInterfaces)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad qualified
import Data.List (partition, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Conc (getNumProcessors)
import Options.Applicative qualified as OA
import ResolveStackageProgress qualified as RSP
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr, stdout)

data Options = Options
  { optSnapshot :: String,
    optJobs :: Int,
    optOffline :: Bool,
    optTopFailures :: Int
  }

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> OA.strOption
      ( OA.long "snapshot"
          <> OA.metavar "SNAPSHOT"
          <> OA.value "lts-24.33"
          <> OA.showDefault
          <> OA.help "Stackage snapshot to type-check"
      )
    <*> OA.option
      OA.auto
      ( OA.long "jobs"
          <> OA.metavar "N"
          <> OA.value 0
          <> OA.help "Number of parallel jobs (default: CPU cores)"
      )
    <*> OA.switch
      ( OA.long "offline"
          <> OA.help "Use only cached packages, don't download"
      )
    <*> OA.option
      OA.auto
      ( OA.long "top"
          <> OA.metavar "N"
          <> OA.value 10
          <> OA.showDefault
          <> OA.help "Number of top failing packages to display"
      )

data PackageCounts = PackageCounts
  { countTypechecked :: !Int,
    countFailed :: !Int,
    countSkipped :: !Int,
    countTotal :: !Int
  }
  deriving (Eq, Show)

summarizePackageStatuses :: Map Text RSP.PackageStatus -> PackageCounts
summarizePackageStatuses results =
  PackageCounts
    { countTypechecked = length [() | RSP.PkgSuccess _ <- statuses],
      countFailed = length [() | RSP.PkgFailed _ <- statuses],
      countSkipped = length [() | RSP.PkgSkipped <- statuses],
      countTotal = length statuses
    }
  where
    statuses = Map.elems results

hasFailedDep :: Map Text RSP.PackageStatus -> Map Text [Text] -> Text -> Bool
hasFailedDep completed depGraph pkg =
  any isFailure [Map.findWithDefault RSP.PkgSkipped dep completed | dep <- deps]
  where
    deps = Map.findWithDefault [] pkg depGraph
    isFailure (RSP.PkgFailed _) = True
    isFailure RSP.PkgSkipped = True
    isFailure _ = False

gatherDepExports :: ModuleExports -> Text -> Map Text RSP.PackageStatus -> Map Text [Text] -> ModuleExports
gatherDepExports ambientExports pkg completed depGraph =
  foldl'
    Map.union
    ambientExports
    [ iface
    | dep <- Map.findWithDefault [] pkg depGraph,
      Just (RSP.PkgSuccess iface) <- [Map.lookup dep completed]
    ]

processLayer ::
  Options ->
  ModuleExports ->
  [Text] ->
  Map Text RSP.PackageInfo ->
  Map Text RSP.PackageStatus ->
  Map Text [Text] ->
  IO (Map Text RSP.PackageStatus)
processLayer opts ambientExports layer infos completed depGraph = do
  let (toSkip, toProcess) = partition (hasFailedDep completed depGraph) layer
      withSkips = foldl' (\m p -> Map.insert p RSP.PkgSkipped m) completed toSkip
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
                let depExports = gatherDepExports ambientExports pkg current depGraph
                    info = Map.findWithDefault (RSP.PackageInfo "" [] []) pkg infos
                status <- typecheckOnePackage (optOffline opts) pkg info depExports
                modifyMVar_ resultVar (pure . Map.insert pkg status)
                worker
      replicateConcurrently_ workerCount worker
      readMVar resultVar

processLayers ::
  Options ->
  ModuleExports ->
  [[Text]] ->
  Map Text RSP.PackageInfo ->
  Map Text [Text] ->
  Map Text RSP.PackageStatus ->
  IO (Map Text RSP.PackageStatus)
processLayers opts ambientExports layers infos depGraph = go layers
  where
    go [] acc = pure acc
    go (layer : rest) acc = do
      acc' <- processLayer opts ambientExports layer infos acc depGraph
      go rest acc'

typecheckOnePackage :: Bool -> Text -> RSP.PackageInfo -> ModuleExports -> IO RSP.PackageStatus
typecheckOnePackage offline pkg info depExports = do
  result <- try (typecheckOnePackageOrThrow offline pkg info depExports)
  pure $ case result of
    Left (e :: SomeException) -> RSP.PkgFailed (displayException e)
    Right status -> status

typecheckOnePackageOrThrow :: Bool -> Text -> RSP.PackageInfo -> ModuleExports -> IO RSP.PackageStatus
typecheckOnePackageOrThrow _offline _pkg info depExports = do
  let rawFiles = RSP.piFiles info
  if null rawFiles
    then pure (RSP.PkgSuccess depExports)
    else do
      parseResults <- mapM (RSP.parseFileInfo (RSP.piSrcDir info)) rawFiles
      let (errs, pairs) = partitionEithers parseResults
      case errs of
        (e : _) -> pure (RSP.PkgFailed e)
        [] -> do
          let modules = map fst pairs
              srcTexts = Map.fromList [(path, src) | (_, (path, src)) <- pairs]
              resolveResult = resolveWithDeps depExports modules
              !annCount = sum (map (length . snd) (resolvedAnnotations resolveResult))
          _ <- evaluate annCount
          case resolveErrors resolveResult of
            [] -> typecheckResolvedPackage resolveResult
            resolveErrs -> pure (RSP.PkgFailed (unlines (map (RSP.renderResolveError srcTexts) resolveErrs)))

typecheckResolvedPackage :: ResolveResult -> IO RSP.PackageStatus
typecheckResolvedPackage resolveResult = do
  let results = typecheck (resolvedModules resolveResult)
      !diagCount = sum (map (length . tcmDiagnostics) results)
  _ <- evaluate diagCount
  if all tcmSuccess results
    then pure (RSP.PkgSuccess (extractInterface resolveResult))
    else pure (RSP.PkgFailed (renderTcDiagnostics results))

renderTcDiagnostics :: [TcModuleResult] -> String
renderTcDiagnostics results =
  unlines [show diag | result <- results, diag <- tcmDiagnostics result]

reportResults :: Int -> Map Text RSP.PackageStatus -> IO ()
reportResults topN results = do
  let counts = summarizePackageStatuses results
      allList = Map.toList results
      failed = [(pkg, msg) | (pkg, RSP.PkgFailed msg) <- allList]
  putStrLn ""
  putStrLn "Type checker results:"
  putStrLn $ "  Typechecked: " ++ show (countTypechecked counts) ++ " / " ++ show (countTotal counts) ++ " (" ++ show (pct (countTypechecked counts) (countTotal counts)) ++ "%)"
  putStrLn $ "  Failed:      " ++ show (countFailed counts) ++ " (parse, resolver, or type-checker errors)"
  putStrLn $ "  Skipped:     " ++ show (countSkipped counts) ++ " (dep had failure)"
  let n = min topN (countFailed counts)
  Control.Monad.when (n > 0) $ do
    putStrLn ""
    putStrLn $ "Top " ++ show n ++ " failing packages:"
    let sorted = take n (sortOn (length . snd) failed)
    mapM_ printFailure sorted
  if countTypechecked counts == countTotal counts then exitSuccess else exitFailure
  where
    printFailure (pkg, msg) = do
      putStrLn $ "  " ++ T.unpack pkg ++ ":"
      mapM_ (\l -> putStrLn ("    " ++ l)) (take 5 (lines msg))

pct :: Int -> Int -> Int
pct _ 0 = 100
pct n total = (n * 100) `div` total

run :: Options -> IO ()
run opts0 = do
  jobs <- if optJobs opts0 == 0 then getNumProcessors else pure (optJobs opts0)
  let opts = opts0 {optJobs = jobs}

  snapshotResult <- loadStackageSnapshot Nothing (optSnapshot opts) (optOffline opts)
  packages <- case snapshotResult of
    Left err -> hPutStrLn stderr ("Failed to load snapshot: " ++ err) >> exitFailure
    Right pkgs -> pure pkgs

  let total = length packages
      snapshotNames = Set.fromList (map (T.pack . pkgName) packages)
      (bootPkgs, regularPkgs) = partition isBootPkg packages
      isBootPkg p = T.pack (pkgName p) `Set.member` bootPackageNames
      progressOptions =
        RSP.Options
          { RSP.optSnapshot = optSnapshot opts,
            RSP.optJobs = optJobs opts,
            RSP.optOffline = optOffline opts,
            RSP.optTopFailures = optTopFailures opts
          }

  putStrLn "Loading boot interfaces..."
  bootIfaceMap <- loadBootInterfaces
  let bootExports = foldl' Map.union Map.empty (Map.elems bootIfaceMap)
      bootResults =
        Map.fromList
          [ (T.pack (pkgName p), RSP.PkgSuccess (Map.findWithDefault Map.empty (T.pack (pkgName p)) bootIfaceMap))
          | p <- bootPkgs
          ]

  isStdoutTerminal <- hIsTerminalDevice stdout
  putStrLn $ "Type-checking " ++ optSnapshot opts ++ " (" ++ show total ++ " packages, " ++ show jobs ++ " jobs)..."
  putStrLn $ "  " ++ show (length bootPkgs) ++ " boot packages (pre-generated), " ++ show (length regularPkgs) ++ " to type-check from source"
  putStrLn "Phase 1: downloading packages and collecting dependency info..."

  infos <- RSP.phase1Parallel progressOptions regularPkgs snapshotNames isStdoutTerminal (length regularPkgs)

  putStrLn "\nPhase 2: parsing, resolving, and type-checking packages in dependency order..."

  let depGraph =
        Map.fromList
          [(name, RSP.piSnapshotDeps info) | (name, info) <- Map.toList infos]
      layers = RSP.kahnLayers depGraph
      coveredPkgs = Set.fromList (concat layers)
      cyclePkgs = Set.difference (Set.fromList (map (T.pack . pkgName) regularPkgs)) coveredPkgs
  if Set.null cyclePkgs
    then pure ()
    else hPutStrLn stderr ("Warning: " ++ show (Set.size cyclePkgs) ++ " packages in dep cycles, type-checking without dep interfaces")

  results <- processLayers opts bootExports layers infos depGraph bootResults

  reportResults (optTopFailures opts) results

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers [] = ([], [])
partitionEithers (Left a : rest) = let (as, bs) = partitionEithers rest in (a : as, bs)
partitionEithers (Right b : rest) = let (as, bs) = partitionEithers rest in (as, b : bs)

replicateM_ :: Int -> IO () -> IO ()
replicateM_ 0 _ = pure ()
replicateM_ n action = action >> replicateM_ (n - 1) action
