-- |
-- Module      : Aihc.PackagePlan
-- Description : Dependency resolution shared by the aihc tools
--
-- Resolves a package and its transitive library dependencies to source
-- directories. Core libraries (@base@, @ghc-prim@, @ghc-internal@,
-- @template-haskell@) are redirected to the standins under @core-libs@; every
-- other package is resolved through a caller-supplied 'DependencyResolver'.
--
-- The compiler and the documentation tool share this module so that both see
-- the same dependency graph for a package.
module Aihc.PackagePlan
  ( DependencyResolver (..),
    PackagePlan (..),
    PlanOrigin (..),
    ResolvedSource (..),
    buildPackagePlanWithResolver,
    DependencyVersions,
    dependencyVersionsFromManifests,
    coreProviders,
    coreProviderSourcePath,
    CoreProvider (..),
    localDependencyResolverWithFallback,
    lookupCoreProvider,
    workspaceDependencyResolver,
    packageSpecFromSource,
    parseSourcePackageDescription,
    parseSourcePackageDescriptionAt,
  )
where

import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cpp (DependencyVersions)
import Aihc.Hackage.Release (BootLibrary (..), emulatedGhc, lookupBootLibraryByStandin, showVersionBranch)
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HackageUtil
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (buildable, condLibrary, condSubLibraries, libBuildInfo, package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Version (Version)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath (normalise, takeDirectory, (</>))

data PackagePlan = PackagePlan
  { planSourcePath :: !FilePath,
    -- | The @.cabal@ file the plan read under 'planSourcePath'.
    planCabalFile :: !FilePath,
    -- | What that file says. The installer reuses it instead of parsing the
    -- file a second time.
    planDescription :: GenericPackageDescription,
    planOrigin :: !PlanOrigin,
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

data ResolvedSource = ResolvedSource
  { resolvedSourcePath :: !FilePath,
    resolvedSourceOrigin :: !PlanOrigin
  }
  deriving (Eq, Show)

data DependencyResolver = DependencyResolver
  { resolverResolveVersion :: String -> IO String,
    resolverSourcePath :: PackageSpec -> IO ResolvedSource
  }

data CoreProvider = CoreProvider
  { coreProviderName :: !String,
    coreProviderVersion :: !String,
    coreProviderSourceRel :: !FilePath
  }

-- | Prefer the package being installed and its siblings in the directory
-- above it over the fallback resolver. The caller passes the spec of the
-- root package so the resolver does not parse that Cabal file again.
localDependencyResolverWithFallback :: DependencyResolver -> FilePath -> PackageSpec -> DependencyResolver
localDependencyResolverWithFallback fallback rootSource rootSpec =
  localPackagesResolver fallback (Just (rootSpec, rootSource)) (takeDirectory (normalise rootSource))

-- | Prefer the packages that are directories of the workspace over the
-- fallback resolver.
workspaceDependencyResolver :: DependencyResolver -> FilePath -> DependencyResolver
workspaceDependencyResolver fallback = localPackagesResolver fallback Nothing

localPackagesResolver :: DependencyResolver -> Maybe (PackageSpec, FilePath) -> FilePath -> DependencyResolver
localPackagesResolver fallback rootPackage workspace =
  DependencyResolver
    { resolverResolveVersion = \name -> do
        local <- localPackage name
        maybe (resolverResolveVersion fallback name) (pure . pkgVersion . fst) local,
      resolverSourcePath = \spec -> do
        local <- localPackage (pkgName spec)
        case local of
          Just (localSpec, path)
            | pkgVersion localSpec == pkgVersion spec -> pure (ResolvedSource path PlanLocal)
          _ -> resolverSourcePath fallback spec
    }
  where
    localPackage name =
      case rootPackage of
        Just (rootSpec, source)
          | pkgName rootSpec == name ->
              pure (Just (rootSpec, source))
        _ -> do
          let candidate = workspace </> name
          exists <- doesDirectoryExist candidate
          if exists
            then do
              spec <- packageSpecFromSource candidate
              pure (Just (spec, candidate))
            else pure Nothing

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
  case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right parsed) -> pure (cabalFile, parsed)
    (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))

buildPackagePlanWithResolver :: DependencyResolver -> PackageSpec -> IO PackagePlan
buildPackagePlanWithResolver resolver spec = do
  -- The dependency graph is a DAG that the recursion walks as a tree: without
  -- the cache a package shared by several dependents is resolved and parsed
  -- once per path that reaches it.
  cache <- newIORef Map.empty
  versions <- newIORef Map.empty
  buildPackagePlanRecursive PlanCaches {planCache = cache, planVersionCache = versions} resolver [] spec

-- | What the recursion remembers across the packages of one plan.
data PlanCaches = PlanCaches
  { planCache :: IORef (Map.Map (String, String) PackagePlan),
    -- | The version a dependency name resolves to, so that flag resolution
    -- asks the resolver about a package name only once per plan.
    planVersionCache :: IORef (Map.Map String (Maybe Version))
  }

buildPackagePlanRecursive :: PlanCaches -> DependencyResolver -> [PackageSpec] -> PackageSpec -> IO PackagePlan
buildPackagePlanRecursive caches resolver stack rawSpec
  | packageSpecIdentity spec `elem` map packageSpecIdentity stack =
      ioError (userError ("Cyclic dependency while installing " <> formatPackage spec))
  | otherwise = do
      cached <- Map.lookup (packageSpecIdentity spec) <$> readIORef (planCache caches)
      case cached of
        -- A cached plan is complete, so it took part in no cycle.
        Just plan -> pure plan
        Nothing -> do
          plan <- buildPlan
          modifyIORef' (planCache caches) (Map.insert (packageSpecIdentity spec) plan)
          pure plan
  where
    buildPlan = do
      ResolvedSource sourcePath origin <- sourcePathForSpec resolver spec
      (cabalFile, parsedGpd) <- parseSourcePackageDescriptionAt sourcePath
      -- Automatic flags are settled before anything reads the description, so
      -- that the plan and the installer see the same conditional branches.
      flags <- HackageCabal.resolveFlagAssignment resolvedDependencyVersion parsedGpd
      let gpd = HackageCabal.applyFlagAssignment flags parsedGpd
      let dependencyNames = packageDependencyNames gpd
      dependencySpecs <- mapM resolveDependencySpec (withImplicitPrimDependency spec dependencyNames)
      dependencyPlans <- mapM (buildPackagePlanRecursive caches resolver (spec : stack)) dependencySpecs
      pure
        PackagePlan
          { planSourcePath = sourcePath,
            planCabalFile = cabalFile,
            planDescription = gpd,
            planOrigin = origin,
            planDependencyPlans = dependencyPlans
          }

    spec = canonicalPackageSpec rawSpec
    resolveDependencySpec dependencyName = do
      version <- resolveVersionForDependency dependencyName
      pure (canonicalPackageSpec (PackageSpec dependencyName version))

    -- The version a dependency name would resolve to, for judging whether a
    -- flag's branch contradicts the plan. A name the resolver cannot resolve
    -- at all is unknown rather than a contradiction: a flag branch that names
    -- a package outside the index must not look preferable for that reason.
    resolvedDependencyVersion dependencyName = do
      known <- Map.lookup dependencyName <$> readIORef (planVersionCache caches)
      case known of
        Just version -> pure version
        Nothing -> do
          resolved <- try (resolveVersionForDependency dependencyName)
          let version =
                case resolved :: Either IOException String of
                  Right text -> simpleParsec text
                  Left _ -> Nothing
          modifyIORef' (planVersionCache caches) (Map.insert dependencyName version)
          pure version

    resolveVersionForDependency dependencyName =
      case lookupCoreProvider dependencyName of
        Just provider -> pure (coreProviderVersion provider)
        Nothing -> resolverResolveVersion resolver dependencyName

withImplicitPrimDependency :: PackageSpec -> [String] -> [String]
withImplicitPrimDependency spec dependencies
  | pkgName spec == "aihc-prim" = dependencies
  | any isPrimDependency dependencies = dependencies
  | otherwise = "aihc-prim" : dependencies
  where
    isPrimDependency name = name == "aihc-prim" || name == "ghc-prim"

sourcePathForSpec :: DependencyResolver -> PackageSpec -> IO ResolvedSource
sourcePathForSpec resolver spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> (`ResolvedSource` PlanCore) <$> coreProviderSourcePath provider
    Nothing -> resolverSourcePath resolver spec

-- | The standin that provides a package name, under either the name of the
-- boot library or the name of the standin itself.
lookupCoreProvider :: String -> Maybe CoreProvider
lookupCoreProvider name =
  case name of
    "base" -> Just aihcBaseProvider
    "aihc-base" -> Just aihcBaseProvider
    "ghc-prim" -> Just aihcPrimProvider
    "aihc-prim" -> Just aihcPrimProvider
    "ghc-internal" -> Just aihcInternalProvider
    "aihc-internal" -> Just aihcInternalProvider
    "template-haskell" -> Just aihcTemplateHaskellProvider
    "aihc-template-haskell" -> Just aihcTemplateHaskellProvider
    "system-cxx-std-lib" -> Just systemCxxStdLibProvider
    _ -> Nothing

canonicalPackageSpec :: PackageSpec -> PackageSpec
canonicalPackageSpec spec =
  case lookupCoreProvider (pkgName spec) of
    Just provider -> PackageSpec (coreProviderName provider) (coreProviderVersion provider)
    Nothing -> spec

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

packageSpecIdentity :: PackageSpec -> (String, String)
packageSpecIdentity spec =
  (pkgName spec, pkgVersion spec)

packageDependencyNames :: GenericPackageDescription -> [String]
packageDependencyNames gpd =
  (sort . nub . map T.unpack)
    ( concatMap
        (filter (/= currentPackageName) . libraryDependencies)
        libraryTrees
    )
  where
    evalCond = HackageCabal.conditionEvaluator gpd
    currentPackageName = T.pack . CabalPackage.unPackageName . CabalPackage.packageName . package $ packageDescription gpd
    libraryTrees =
      maybe [] pure (condLibrary gpd)
        <> map snd (condSubLibraries gpd)

    libraryDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond libBuildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []
