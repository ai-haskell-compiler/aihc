{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TcStackageProgress
  ( Options (..),
    PackageCounts (..),
    optionsParser,
    checkOnePackage,
    smallestFailingPackages,
    summarizePackageStatuses,
    run,
  )
where

import Aihc.Cli.Install
  ( DependencyResolver (..),
    PackageCheckCache,
    PackagePlan (..),
    PackagePlanCache,
    PackageVariantKey (..),
    buildPackagePlanWithResolverCached,
    checkPackagePlanWithCache,
    defaultStoreRoot,
    installFailureIsForPackage,
    lookupPackagePlanSourceLineCount,
    newPackageCheckCache,
    newPackagePlanCache,
    packagePlanFailureShouldBeReportedForPackage,
    renderInstallFailure,
  )
import Aihc.Hackage.Cabal qualified as HC
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Stackage (loadStackageSnapshot)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util (readTextFileLenient)
import Aihc.Hackage.VersionResolver (getLatestVersion)
import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (modifyMVar_, newMVar, readMVar)
import Control.Exception (SomeException, displayException, try)
import Control.Monad qualified
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Conc (getNumProcessors)
import Options.Applicative qualified as OA
import ResolveStackageProgress qualified as RSP
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

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
          <> OA.help "Stackage snapshot to check"
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

smallestFailingPackages ::
  Int ->
  Map Text RSP.PackageInfo ->
  Map Text RSP.PackageStatus ->
  IO [(Text, Int, String)]
smallestFailingPackages topN infos results = do
  sizes <- traverse packageLineCount infos
  let failed =
        [ (pkg, Map.findWithDefault 0 pkg sizes, msg)
        | (pkg, RSP.PkgFailed msg) <- Map.toList results
        ]
  pure (take topN (sortOn (\(pkg, lineCount, msg) -> (lineCount, pkg, msg)) failed))

packageLineCount :: RSP.PackageInfo -> IO Int
packageLineCount info =
  sum <$> mapM fileLineCount (RSP.piFiles info)

fileLineCount :: HC.FileInfo -> IO Int
fileLineCount fileInfo = do
  let path = HC.fileInfoPath fileInfo
  exists <- doesFileExist path
  if exists
    then length . T.lines <$> readTextFileLenient path
    else pure 0

run :: Options -> IO ()
run opts0 = do
  requestedJobs <- if optJobs opts0 == 0 then getNumProcessors else pure (optJobs opts0)
  let jobs = max 1 requestedJobs
      opts = opts0 {optJobs = jobs}
  snapshotResult <- loadStackageSnapshot Nothing (optSnapshot opts) (optOffline opts)
  packages <- case snapshotResult of
    Left err -> hPutStrLn stderr ("Failed to load snapshot: " ++ err) >> exitFailure
    Right specs -> pure specs

  storeRoot <- defaultStoreRoot
  planCache <- newPackagePlanCache
  checkCache <- newPackageCheckCache
  let snapshotVersions = Map.fromList [(pkgName spec, pkgVersion spec) | spec <- packages]
      resolver = snapshotDependencyResolver opts snapshotVersions

  putStrLn $
    "Checking "
      ++ optSnapshot opts
      ++ " ("
      ++ show (length packages)
      ++ " packages, "
      ++ show jobs
      ++ " jobs) through the install pipeline..."

  queue <- newChan
  mapM_ (writeChan queue . Just) packages
  replicateM_ jobs (writeChan queue Nothing)
  resultVar <- newMVar Map.empty
  let worker = do
        next <- readChan queue
        case next of
          Nothing -> pure ()
          Just spec -> do
            (status, lineCount) <- checkOnePackage planCache checkCache resolver storeRoot spec
            modifyMVar_ resultVar (pure . Map.insert (T.pack (pkgName spec)) (status, lineCount))
            worker
  replicateConcurrently_ jobs worker
  resultsWithSizes <- readMVar resultVar
  reportResults (optTopFailures opts) resultsWithSizes

snapshotDependencyResolver :: Options -> Map String String -> DependencyResolver
snapshotDependencyResolver opts versions =
  DependencyResolver
    { resolverResolveVersion = \name ->
        case Map.lookup name versions of
          Just "installed" -> resolveLatestVersion name
          Just version -> pure version
          Nothing -> ioError (userError ("dependency is not in " ++ optSnapshot opts ++ ": " ++ name)),
      resolverSourcePath =
        HackageDownload.downloadPackageWithOptions
          HackageDownload.defaultDownloadOptions
            { HackageDownload.downloadVerbose = False,
              HackageDownload.downloadAllowNetwork = not (optOffline opts)
            }
    }
  where
    resolveLatestVersion name = do
      result <- getLatestVersion Nothing name
      either (ioError . userError) pure result

checkOnePackage :: PackagePlanCache -> PackageCheckCache -> DependencyResolver -> FilePath -> PackageSpec -> IO (RSP.PackageStatus, Int)
checkOnePackage planCache checkCache resolver storeRoot spec = do
  result <- try $ do
    plan <- buildPackagePlanWithResolverCached planCache resolver storeRoot spec
    checkResult <- checkPackagePlanWithCache checkCache plan
    pure (plan, checkResult)
  lineCount <- fromMaybe 0 <$> lookupPackagePlanSourceLineCount planCache spec
  case result of
    Left (err :: SomeException) -> do
      let status =
            if packagePlanFailureShouldBeReportedForPackage spec err
              then RSP.PkgFailed (displayException err)
              else RSP.PkgSkipped
      pure (status, lineCount)
    Right (plan, Left failure) ->
      let rootSpec = packageKeySpec (planPackageKey plan)
          status =
            if installFailureIsForPackage rootSpec failure
              then RSP.PkgFailed (renderInstallFailure failure)
              else RSP.PkgSkipped
       in pure (status, lineCount)
    Right (_, Right _) -> pure (RSP.PkgSuccess Map.empty, lineCount)

reportResults :: Int -> Map Text (RSP.PackageStatus, Int) -> IO ()
reportResults topN resultsWithSizes = do
  let results = fmap fst resultsWithSizes
      counts = summarizePackageStatuses results
  putStrLn ""
  putStrLn "Type checker results:"
  putStrLn $ "  Typechecked: " ++ show (countTypechecked counts) ++ " / " ++ show (countTotal counts) ++ " (" ++ show (pct (countTypechecked counts) (countTotal counts)) ++ "%)"
  putStrLn $ "  Failed:      " ++ show (countFailed counts) ++ " (parse, scope, type-check, or desugar errors)"
  putStrLn $ "  Skipped:     " ++ show (countSkipped counts) ++ " (dependency failed)"
  let failures =
        take topN . sortOn (\(pkg, lineCount, msg) -> (lineCount, pkg, msg)) $
          [ (pkg, lineCount, msg)
          | (pkg, (RSP.PkgFailed msg, lineCount)) <- Map.toList resultsWithSizes
          ]
  Control.Monad.unless (null failures) $ do
    putStrLn ""
    putStrLn $ "Smallest " ++ show (length failures) ++ " failing packages:"
    mapM_ printFailure failures
  if countTypechecked counts == countTotal counts then exitSuccess else exitFailure
  where
    printFailure (pkg, lineCount, msg) = do
      putStrLn $ "  " ++ T.unpack pkg ++ " (" ++ show lineCount ++ " lines):"
      mapM_ (\line -> putStrLn ("    " ++ line)) (take 5 (lines msg))

pct :: Int -> Int -> Int
pct _ 0 = 100
pct n total = (n * 100) `div` total

replicateM_ :: Int -> IO () -> IO ()
replicateM_ 0 _ = pure ()
replicateM_ n action = action >> replicateM_ (n - 1) action
