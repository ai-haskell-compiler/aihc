{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Aihc.PackagePlan
-- Description : Dependency resolution shared by the aihc tools
--
-- Turns a set of root packages into a plan: one version and one flag
-- assignment for every package the build needs, each resolved to a source
-- directory. The versions come from the solver in "Aihc.PackagePlan.Solver"
-- and are kept in the @aihc.lock@ file of "Aihc.PackagePlan.Lock", so that
-- a later build reuses the plan instead of solving. Core libraries (@base@,
-- @ghc-prim@, @ghc-internal@, @template-haskell@) are redirected to the
-- standins under @core-libs@; local packages and the packages of a
-- workspace shadow Hackage; everything else is a Hackage release.
--
-- The compiler and the documentation tool share this module so that both
-- see the same dependency graph for a package.
module Aihc.PackagePlan
  ( -- * Planning
    PlanRequest (..),
    PlanRoot (..),
    LockMode (..),
    PlannedPackages (..),
    planPackages,
    PackagePlan (..),
    PlanOrigin (..),
    planBuildContext,
    canonicalPackageName,
    parseConstraint,

    -- * Core libraries
    DependencyVersions,
    dependencyVersionsFromManifests,
    coreProviders,
    coreProviderSourcePath,
    aihcRtsProvider,
    CoreProvider (..),
    lookupCoreProvider,

    -- * Cabal files
    packageSpecFromSource,
    parseSourcePackageDescription,
    parseSourcePackageDescriptionAt,
  )
where

import Aihc.Hackage.Cabal (BuildContext (..))
import Aihc.Hackage.Cpp (DependencyVersions)
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Index (IndexEntry (..))
import Aihc.Hackage.IndexCache (HackageIndex, IndexVersion (..), indexPackageVersions, indexReadCabalFile, indexState)
import Aihc.Hackage.Release (BootLibrary (..), GhcRelease (..), emulatedGhc, lookupBootLibraryByStandin, releaseVersionText, showVersionBranch)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.PackagePlan.Lock
import Aihc.PackagePlan.Solver
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Distribution.Package (PackageName, mkPackageName, unPackageName)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch, OS)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Types.Flag (FlagAssignment, mkFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.Version (Version, mkVersion)
import Distribution.Types.VersionRange (VersionRange, anyVersion, thisVersion)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath (normalise, takeDirectory, (</>))

-- | One package of a plan, with the plans of its dependencies.
data PackagePlan = PackagePlan
  { planName :: !PackageName,
    planSourcePath :: !FilePath,
    -- | The @.cabal@ file under 'planSourcePath'.
    planCabalFile :: !FilePath,
    -- | What the plan solved with. For a Hackage release this is the
    -- cabal file of the recorded revision, which may be newer than the one
    -- in the source tree.
    planDescription :: GenericPackageDescription,
    planOrigin :: !PlanOrigin,
    -- | The cabal file revision of a Hackage release.
    planRevision :: !(Maybe Int),
    -- | The flags the solver decided. Every other flag takes its default.
    planFlags :: !FlagAssignment,
    planDependencyPlans :: ![PackagePlan]
  }
  deriving (Eq, Show)

-- | Where the source of a planned package comes from. The origin decides
-- whether the package is immutable: a Hackage release and a core library
-- never change for a given compiler, while a local directory is edited.
data PlanOrigin
  = -- | A directory the user works in.
    PlanLocal
  | -- | A Hackage release.
    PlanHackage
  | -- | A standin under @core-libs@.
    PlanCore
  deriving (Eq, Ord, Show)

-- | The conditions a planned package is built under on one platform.
planBuildContext :: (OS, Arch) -> PackagePlan -> BuildContext
planBuildContext (os, arch) plan = BuildContext os arch (planFlags plan)

-- | A package the plan is made for.
data PlanRoot
  = -- | A directory holding a cabal file.
    RootLocal FilePath
  | -- | A Hackage package, at one version or at whichever the solver
    -- picks.
    RootHackage String (Maybe Version)
  deriving (Eq, Show)

-- | What to do with the lock file.
data LockMode
  = -- | Take a valid lock, solve around a stale or absent one and rewrite
    -- it.
    LockNormal
  | -- | Fail instead of rewriting a stale or absent lock.
    LockLocked
  | -- | Ignore the lock for every package and rewrite it.
    LockUpdateAll
  | -- | Ignore the lock for these packages and their dependents, and
    -- rewrite it.
    LockUpdate [PackageName]
  deriving (Eq, Show)

data PlanRequest = PlanRequest
  { requestRoots :: ![PlanRoot],
    -- | Packages to plan besides the roots, each with the range it must
    -- satisfy: the @-p@ constraints of a main module.
    requestGoals :: ![(PackageName, VersionRange)],
    -- | Directories whose subdirectory @NAME@ is the source of the package
    -- @NAME@, before Hackage.
    requestWorkspaces :: ![FilePath],
    requestPlatform :: !(OS, Arch),
    requestConstraints :: ![Constraint],
    -- | The local lock file. Hackage targets have no lock file.
    requestLockFile :: !(Maybe FilePath),
    requestLockMode :: !LockMode,
    requestIndex :: !HackageIndex,
    requestVerbose :: String -> IO ()
  }

data PlannedPackages = PlannedPackages
  { plannedSolution :: !Solution,
    -- | Every package of the plan under its canonical name.
    plannedPlans :: !(Map PackageName PackagePlan),
    -- | The roots, in request order.
    plannedRoots :: ![PackagePlan]
  }

-- | The name a dependency resolves to: the standin of a boot library, or
-- the name itself.
canonicalPackageName :: PackageName -> PackageName
canonicalPackageName name = fromMaybe name (Map.lookup name packageAliases)

packageAliases :: Map PackageName PackageName
packageAliases =
  Map.fromList
    [ (mkPackageName (bootLibraryName library), mkPackageName (bootLibraryStandin library))
    | library <- releaseBootLibraries emulatedGhc,
      bootLibraryName library /= bootLibraryStandin library
    ]

-- | Parse a @--constraint@ argument: @NAME RANGE@, or @NAME@ followed by
-- @+flag@ and @-flag@ words.
parseConstraint :: String -> Either String [Constraint]
parseConstraint input =
  case words input of
    [] -> Left "empty constraint"
    name : flags@(_ : _)
      | all isFlagWord flags,
        Just packageName <- simpleParsec name ->
          Right [ConstraintFlag packageName (mkFlagName (drop 1 flag)) (take 1 flag == "+") | flag <- flags]
    _ ->
      case simpleParsec input of
        Just (Dependency name range _) -> Right [ConstraintVersion name range]
        Nothing -> Left ("Invalid constraint: " <> input <> " (expected NAME RANGE, NAME +flag, or NAME -flag)")
  where
    isFlagWord flag = case flag of
      '+' : _ : _ -> True
      '-' : _ : _ -> True
      _ -> False

-- | Plan the roots and goals of a request.
planPackages :: PlanRequest -> IO PlannedPackages
planPackages request = do
  roots <- forM (requestRoots request) $ \case
    RootLocal path -> do
      (cabalFile, gpd) <- parseSourcePackageDescriptionAt path
      pure (packageNameOf gpd, Left (path, cabalFile, gpd))
    RootHackage name version -> pure (mkPackageName name, Right version)
  descriptions <- newIORef Map.empty
  let localRoots = Map.fromList [(name, path) | (name, Left (path, _, _)) <- roots]
      -- The package itself and its siblings resolve before the workspace,
      -- and the workspace before Hackage.
      localDirectories = [takeDirectory (normalise path) | (_, Left (path, _, _)) <- roots] <> requestWorkspaces request
      config =
        SolverConfig
          { configPlatform = requestPlatform request,
            configAliases = packageAliases,
            configConstraints =
              requestConstraints request
                <> [ConstraintVersion name (thisVersion version) | (name, Right (Just version)) <- roots],
            configPreferences = Map.empty,
            configRoots = Map.fromList [(name, noStanzas) | (name, _) <- roots],
            -- Every package depends on aihc-prim, so the plan needs it even
            -- when no cabal file names it; see 'withImplicitPrimDependency'.
            configGoals =
              requestGoals request
                <> [(prim, anyVersion) | all ((`notElem` [prim, mkPackageName "aihc-rts"]) . fst) roots],
            configMaxBacktracks = 2000
          }
      inputs = solverInputs request descriptions localRoots localDirectories
  forM_ (Map.toList localRoots) $ \(name, path) ->
    when (Map.member name packageAliases) $
      ioError (userError ("The package " <> unPackageName name <> " at " <> path <> " has the name of a boot library"))
  (solution, solved) <- solveWithLock request inputs config
  checkBuildTools request inputs config solution
  plans <- buildPlans inputs solution
  when (solved && any usesHackage (Map.elems solution) && requestLockMode request /= LockLocked) $
    writeLock request solution
  pure
    PlannedPackages
      { plannedSolution = solution,
        plannedPlans = plans,
        plannedRoots = [plans Map.! canonicalPackageName name | (name, _) <- roots]
      }
  where
    usesHackage assignment = assignmentSource assignment == CandidateHackage
    prim = mkPackageName "aihc-prim"

-- | Take the plan from a valid lock, or solve and say so.
solveWithLock :: PlanRequest -> SolverInputs IO -> SolverConfig -> IO (Solution, Bool)
solveWithLock request inputs config
  | Nothing <- requestLockFile request = runSolve config
  | Just lockPath <- requestLockFile request = do
      lock <- readLockFile lockPath
      entries <-
        case lock of
          Nothing -> pure Nothing
          Just (Left problem) -> ioError (userError ("Invalid lock file " <> lockPath <> ": " <> problem))
          Just (Right file)
            | lockCompiler file /= compilerName -> do
                requestVerbose request ("Ignoring " <> lockPath <> ": it was written for " <> lockCompiler file <> ", this compiler is " <> compilerName)
                pure Nothing
            | otherwise -> pure (Map.lookup platform (lockPlatforms file))
      verified <-
        case entries of
          Just locked | requestLockMode request /= LockUpdateAll -> Just <$> verifySolution inputs config (lockRecorded locked)
          _ -> pure Nothing
      case (requestLockMode request, entries, verified) of
        (LockLocked, Nothing, _) ->
          ioError (userError ("No plan for " <> platform <> " in " <> lockPath <> ", and --locked forbids solving"))
        (LockLocked, Just _, Just (Left problem)) ->
          ioError (userError ("The lock file " <> lockPath <> " is stale (" <> problem <> "), and --locked forbids solving"))
        (mode, Just _, Just (Right solution))
          | mode == LockNormal || mode == LockLocked -> do
              requestVerbose request ("Plan taken from " <> lockPath)
              pure (solution, False)
        (mode, Just locked, _) -> do
          case verified of
            Just (Left problem) -> requestVerbose request ("The lock file " <> lockPath <> " is stale: " <> problem)
            _ -> pure ()
          let dropped = case mode of
                LockUpdate names -> Set.fromList (map canonicalPackageName names)
                _ -> Set.empty
              dependents = case verified of
                Just (Right solution) -> transitiveDependents solution dropped
                _ -> dropped
              preferences = Map.withoutKeys (lockPreferences locked) dependents
          runSolve config {configPreferences = if mode == LockUpdateAll then Map.empty else preferences}
        (_, Nothing, _) -> runSolve config
  where
    platform = uncurry platformKey (requestPlatform request)
    runSolve solverConfig = do
      result <- solve inputs solverConfig
      case result of
        Left failure -> ioError (userError ("Could not resolve dependencies:\n" <> renderSolveFailure failure))
        Right solution -> pure (solution, True)

-- | The packages that depend on any of the given ones, transitively,
-- together with the given ones.
transitiveDependents :: Solution -> Set.Set PackageName -> Set.Set PackageName
transitiveDependents solution = go
  where
    go known =
      let next =
            Set.fromList
              [ name
              | (name, assignment) <- Map.toList solution,
                any (`Set.member` known) (Map.keys (assignmentDependencies assignment))
              ]
       in if next `Set.isSubsetOf` known then known else go (Set.union known next)

compilerName :: String
compilerName = "ghc-" <> releaseVersionText emulatedGhc

writeLock :: PlanRequest -> Solution -> IO ()
writeLock request solution = forM_ (requestLockFile request) $ \lockPath -> do
  existing <- readLockFile lockPath
  state <- indexState (requestIndex request)
  let otherPlatforms =
        case existing of
          Just (Right file) | lockCompiler file == compilerName -> lockPlatforms file
          _ -> Map.empty
      lock =
        LockFile
          { lockCompiler = compilerName,
            lockIndexState = Just (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (posixSecondsToUTCTime (fromIntegral state))),
            lockPlatforms = Map.insert (uncurry platformKey (requestPlatform request)) (lockEntriesFromSolution solution) otherPlatforms
          }
  requestVerbose request ("Writing " <> lockPath)
  writeLockFile lockPath lock

-- | Every build tool the plan needs must be one the host can run.
checkBuildTools :: PlanRequest -> SolverInputs IO -> SolverConfig -> Solution -> IO ()
checkBuildTools request inputs config solution = do
  problems <- forM (Map.toList solution) $ \(name, assignment) -> do
    gpd <- inputsDescription inputs (assignmentCandidate name assignment)
    let unknown = unknownBuildTools (requestPlatform request) (assignmentFlags assignment) (Map.lookup name (configRoots config)) gpd
    pure [unPackageName name <> "-" <> prettyShow (assignmentVersion assignment) <> " needs the build tool " <> tool | tool <- unknown]
  unless (all null problems) $
    ioError (userError ("The plan needs build tools this compiler cannot run:\n" <> intercalate "\n" (map ("  " <>) (concat problems))))

assignmentCandidate :: PackageName -> Assignment -> Candidate
assignmentCandidate name assignment =
  Candidate
    { candidateName = name,
      candidateVersion = assignmentVersion assignment,
      candidateRevision = assignmentRevision assignment,
      candidateDeprecated = False,
      candidateSource = assignmentSource assignment
    }

-- | The candidates and cabal files the solver reads: core standins, the
-- roots, their siblings and the workspace, then Hackage.
solverInputs :: PlanRequest -> IORef (Map FilePath (FilePath, GenericPackageDescription)) -> Map PackageName FilePath -> [FilePath] -> SolverInputs IO
solverInputs request descriptions localRoots localDirectories =
  SolverInputs
    { inputsCandidates = candidates,
      inputsDescription = description
    }
  where
    candidates name preference =
      case Map.lookup name localRoots of
        Just path -> pure <$> localCandidate name path
        Nothing ->
          case lookupCoreProvider (unPackageName name) of
            Just provider -> do
              path <- coreProviderSourcePath provider
              pure [Candidate name (mkVersion (bootVersion provider)) 0 False (CandidateCore path)]
            Nothing -> do
              local <- findLocal name localDirectories
              case local of
                Just path -> pure <$> localCandidate name path
                Nothing -> hackageCandidates name preference

    bootVersion provider =
      maybe [] bootLibraryVersion (lookupBootLibraryByStandin (coreProviderName provider) emulatedGhc)

    findLocal _ [] = pure Nothing
    findLocal name (directory : rest) = do
      let candidate = directory </> unPackageName name
      exists <- doesDirectoryExist candidate
      cabalFiles <- if exists then HackageUtil.findCabalFiles candidate else pure []
      if null cabalFiles then findLocal name rest else pure (Just candidate)

    localCandidate name path = do
      (_, gpd) <- describeLocal path
      let actual = packageNameOf gpd
      when (actual /= name) $
        ioError (userError ("The package at " <> path <> " is " <> unPackageName actual <> ", not " <> unPackageName name))
      pure (Candidate name (CabalPackage.packageVersion (package (packageDescription gpd))) 0 False (CandidateLocal path))

    hackageCandidates name preference = do
      versions <- indexPackageVersions (requestIndex request) (unPackageName name)
      pure
        [ Candidate name (indexVersionVersion version) revision (indexVersionDeprecated version) CandidateHackage
        | version <- fromMaybe [] versions,
          let latest = maximum (map indexEntryRevision (indexVersionRevisions version))
              revision =
                case preference of
                  Just chosen
                    | preferredVersion chosen == indexVersionVersion version,
                      Just wanted <- preferredRevision chosen,
                      wanted `elem` map indexEntryRevision (indexVersionRevisions version) ->
                        wanted
                  _ -> latest
        ]

    description candidate =
      case candidateSource candidate of
        CandidateLocal path -> snd <$> describeLocal path
        CandidateCore path -> snd <$> describeLocal path
        CandidateHackage -> do
          result <- indexReadCabalFile (requestIndex request) (unPackageName (candidateName candidate)) (candidateVersion candidate) (Just (candidateRevision candidate))
          case result of
            Left problem -> ioError (userError problem)
            Right (_, bytes) -> parseDescriptionBytes (unPackageName (candidateName candidate) <> "-" <> prettyShow (candidateVersion candidate) <> " from the Hackage index") bytes

    describeLocal path = do
      known <- Map.lookup path <$> readIORef descriptions
      case known of
        Just parsed -> pure parsed
        Nothing -> do
          parsed <- parseSourcePackageDescriptionAt path
          modifyIORef' descriptions (Map.insert path parsed)
          pure parsed

-- | Turn the solution into plan trees, one node per package, fetching the
-- Hackage releases it chose.
buildPlans :: SolverInputs IO -> Solution -> IO (Map PackageName PackagePlan)
buildPlans inputs solution = do
  built <- newIORef Map.empty
  forM_ (Map.keys solution) (build built [])
  readIORef built
  where
    build built stack name
      | name `elem` stack =
          ioError (userError ("Cyclic dependency: " <> intercalate " -> " (map unPackageName (reverse (name : stack)))))
      | otherwise = do
          known <- Map.lookup name <$> readIORef built
          case known of
            Just plan -> pure plan
            Nothing -> do
              let assignment = solution Map.! name
              let dependencyNames = withImplicitPrimDependency name (Map.keys (assignmentDependencies assignment))
              dependencies <- mapM (build built (name : stack)) (sort dependencyNames)
              (sourcePath, origin) <-
                case assignmentSource assignment of
                  CandidateLocal path -> pure (path, PlanLocal)
                  CandidateCore path -> pure (path, PlanCore)
                  CandidateHackage -> do
                    path <-
                      HackageDownload.downloadPackageWithOptions
                        HackageDownload.defaultDownloadOptions
                        PackageSpec {pkgName = unPackageName name, pkgVersion = prettyShow (assignmentVersion assignment)}
                    pure (path, PlanHackage)
              cabalFiles <- HackageUtil.findCabalFiles sourcePath
              cabalFile <-
                case cabalFiles of
                  [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
                  files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
              gpd <- inputsDescription inputs (assignmentCandidate name assignment)
              let plan =
                    PackagePlan
                      { planName = name,
                        planSourcePath = sourcePath,
                        planCabalFile = cabalFile,
                        planDescription = gpd,
                        planOrigin = origin,
                        planRevision = case assignmentSource assignment of
                          CandidateHackage -> Just (assignmentRevision assignment)
                          _ -> Nothing,
                        planFlags = assignmentFlags assignment,
                        planDependencyPlans = dependencies
                      }
              modifyIORef' built (Map.insert name plan)
              pure plan

-- | Every package depends on @aihc-prim@, whether its Cabal file says so or
-- not. The two packages below it are the exception: @aihc-prim@ itself, and
-- the runtime it depends on, which has no Haskell modules.
withImplicitPrimDependency :: PackageName -> [PackageName] -> [PackageName]
withImplicitPrimDependency name dependencies
  | unPackageName name `elem` ["aihc-prim", "aihc-rts"] = dependencies
  | prim `elem` dependencies = dependencies
  | otherwise = prim : dependencies
  where
    prim = mkPackageName "aihc-prim"

packageNameOf :: GenericPackageDescription -> PackageName
packageNameOf = CabalPackage.packageName . package . packageDescription

data CoreProvider = CoreProvider
  { coreProviderName :: !String,
    coreProviderVersion :: !String,
    coreProviderSourceRel :: !FilePath
  }

-- | Read the package name and version from the Cabal file of a source tree.
packageSpecFromSource :: FilePath -> IO PackageSpec
packageSpecFromSource sourcePath =
  packageSpecFromDescription <$> parseSourcePackageDescription sourcePath

packageSpecFromDescription :: GenericPackageDescription -> PackageSpec
packageSpecFromDescription gpd =
  let packageId = package (packageDescription gpd)
   in PackageSpec
        { pkgName = CabalPackage.unPackageName (CabalPackage.packageName packageId),
          pkgVersion = prettyShow (CabalPackage.packageVersion packageId)
        }

parseSourcePackageDescription :: FilePath -> IO GenericPackageDescription
parseSourcePackageDescription sourcePath = snd <$> parseSourcePackageDescriptionAt sourcePath

-- | Parse the @.cabal@ file of a source tree and say which file it was.
parseSourcePackageDescriptionAt :: FilePath -> IO (FilePath, GenericPackageDescription)
parseSourcePackageDescriptionAt sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  parsed <- parseDescriptionBytes cabalFile cabalBytes
  pure (cabalFile, parsed)

parseDescriptionBytes :: String -> BS.ByteString -> IO GenericPackageDescription
parseDescriptionBytes label cabalBytes =
  case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right parsed) -> pure parsed
    (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> label <> ": " <> show errs))

-- | The standin that provides a package name, under either the name of the
-- boot library or the name of the standin itself.
lookupCoreProvider :: String -> Maybe CoreProvider
lookupCoreProvider name =
  case name of
    "base" -> Just aihcBaseProvider
    "aihc-base" -> Just aihcBaseProvider
    "ghc-prim" -> Just aihcPrimProvider
    "aihc-prim" -> Just aihcPrimProvider
    "rts" -> Just aihcRtsProvider
    "aihc-rts" -> Just aihcRtsProvider
    "ghc-internal" -> Just aihcInternalProvider
    "aihc-internal" -> Just aihcInternalProvider
    "template-haskell" -> Just aihcTemplateHaskellProvider
    "aihc-template-haskell" -> Just aihcTemplateHaskellProvider
    "system-cxx-std-lib" -> Just systemCxxStdLibProvider
    _ -> Nothing

-- | Every standin under @core-libs@, with the version of the boot library it
-- replaces. The versions come from the emulated GHC release so that a
-- package sees the same @base@ version in its @MIN_VERSION_base@ macro, in
-- its resolved dependencies and in the standin's own @.cabal@ file.
coreProviders :: [CoreProvider]
coreProviders = map (uncurry coreProvider) coreProviderSources
  where
    coreProviderSources =
      [ ("aihc-base", "core-libs" </> "aihc-base"),
        ("aihc-prim", "core-libs" </> "aihc-prim"),
        ("aihc-rts", "core-libs" </> "aihc-rts"),
        ("aihc-internal", "core-libs" </> "aihc-internal"),
        ("aihc-template-haskell", "core-libs" </> "aihc-template-haskell"),
        ("system-cxx-std-lib", "core-libs" </> "system-cxx-std-lib")
      ]
    coreProvider name sourceRel =
      CoreProvider
        { coreProviderName = name,
          coreProviderVersion =
            maybe
              (error ("core-libs package " <> name <> " is not a boot library of the emulated GHC release"))
              (showVersionBranch . bootLibraryVersion)
              (lookupBootLibraryByStandin name emulatedGhc),
          coreProviderSourceRel = sourceRel
        }

namedCoreProvider :: String -> CoreProvider
namedCoreProvider name =
  case [provider | provider <- coreProviders, coreProviderName provider == name] of
    provider : _ -> provider
    [] -> error ("unknown core provider " <> name)

aihcBaseProvider :: CoreProvider
aihcBaseProvider = namedCoreProvider "aihc-base"

aihcPrimProvider :: CoreProvider
aihcPrimProvider = namedCoreProvider "aihc-prim"

-- | The runtime system: the C and Lir units every program links.
aihcRtsProvider :: CoreProvider
aihcRtsProvider = namedCoreProvider "aihc-rts"

aihcInternalProvider :: CoreProvider
aihcInternalProvider = namedCoreProvider "aihc-internal"

aihcTemplateHaskellProvider :: CoreProvider
aihcTemplateHaskellProvider = namedCoreProvider "aihc-template-haskell"

systemCxxStdLibProvider :: CoreProvider
systemCxxStdLibProvider = namedCoreProvider "system-cxx-std-lib"

-- | The versions a file's @MIN_VERSION_*@ macros report, from the manifests
-- of the packages it is compiled against. A standin is reachable under both
-- its own name and the name of the boot library it replaces, because a
-- Hackage package writes @MIN_VERSION_base@ while the installed package is
-- called @aihc-base@.
dependencyVersionsFromManifests :: [(Text, Text)] -> DependencyVersions
dependencyVersionsFromManifests manifests =
  Map.fromList (concatMap entries manifests)
  where
    entries (name, versionText) =
      case mapM readComponent (T.splitOn "." versionText) of
        Just version ->
          (name, version)
            : [ (T.pack (bootLibraryName library), version)
              | Just library <- [lookupBootLibraryByStandin (T.unpack name) emulatedGhc]
              ]
        Nothing -> []
    readComponent component =
      case reads (T.unpack component) of
        [(value, "")] -> Just value
        _ -> Nothing

coreProviderSourcePath :: CoreProvider -> IO FilePath
coreProviderSourcePath provider = do
  override <- lookupEnv "AIHC_CORE_LIBS_ROOT"
  case override of
    Just root -> pure (root </> coreProviderSourceRel provider)
    Nothing -> do
      cwd <- getCurrentDirectory
      findAncestorContaining providerMarker cwd
  where
    providerRel = coreProviderSourceRel provider
    providerMarker = providerRel </> coreProviderName provider <> ".cabal"

    findAncestorContaining marker dir = do
      exists <- doesFileExist (dir </> marker)
      if exists
        then pure (dir </> providerRel)
        else do
          let parent = takeDirectory dir
          if parent == dir
            then ioError (userError ("Could not find local core library " <> providerRel <> " from current directory"))
            else findAncestorContaining marker parent
