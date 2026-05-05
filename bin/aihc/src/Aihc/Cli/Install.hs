module Aihc.Cli.Install
  ( ArtifactManifest (..),
    DependencyResolver (..),
    InstallResult (..),
    PackageHash (..),
    PackagePlan (..),
    PackageVariantKey (..),
    PhaseManifest (..),
    PhaseStatus (..),
    ResolvedDependency (..),
    buildDryRunPackagePlanWithResolver,
    buildPackagePlanFromSource,
    buildPackagePlanWithResolver,
    defaultStoreRoot,
    dryRunInstallScaffold,
    runInstall,
    writeInstallScaffold,
  )
where

import Aihc.Cli.Options (InstallOptions (..))
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Cache (sanitizeName)
import Aihc.Hackage.Download qualified as HackageDownload
import Aihc.Hackage.Types (PackageSpec (..), formatPackage)
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Hackage.VersionResolver (getLatestVersion)
import Control.Monad (when)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.List (nub, sort, sortOn)
import Data.Text qualified as T
import Data.Word (Word64)
import Distribution.PackageDescription (buildInfo, buildable, condExecutables, condLibrary, condSubLibraries, genPackageFlags, libBuildInfo)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Types.Flag (flagDefault, flagName, unFlagName)
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Numeric (showHex)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
  )
import System.FilePath (takeDirectory, (</>))

data PackagePlan = PackagePlan
  { planPackageKey :: !PackageVariantKey,
    planSourcePath :: !FilePath,
    planCabalFile :: !FilePath,
    planSetupFile :: !(Maybe FilePath),
    planStoreRoot :: !FilePath,
    planStorePath :: !FilePath,
    planSourceFileCount :: !Int
  }
  deriving (Eq, Show)

data InstallResult = InstallResult
  { resultStorePath :: !FilePath,
    resultManifestPath :: !FilePath,
    resultInterfacePath :: !FilePath,
    resultFcPath :: !FilePath
  }
  deriving (Eq, Show)

data ArtifactManifest = ArtifactManifest
  { manifestPackageKey :: !PackageVariantKey,
    manifestSourcePath :: !FilePath,
    manifestCabalFile :: !FilePath,
    manifestSetupFile :: !(Maybe FilePath),
    manifestStorePath :: !FilePath,
    manifestInterfacePath :: !FilePath,
    manifestFcPath :: !FilePath,
    manifestSourceFileCount :: !Int,
    manifestPhases :: ![PhaseManifest]
  }
  deriving (Eq, Show)

data PhaseManifest = PhaseManifest
  { phaseName :: !String,
    phaseStatus :: !PhaseStatus,
    phaseDescription :: !String
  }
  deriving (Eq, Show)

data PhaseStatus
  = Planned
  | Unimplemented
  | Complete
  deriving (Eq, Show)

data DependencyResolver = DependencyResolver
  { resolverResolveVersion :: String -> IO String,
    resolverSourcePath :: PackageSpec -> IO FilePath
  }

newtype PackageHash = PackageHash
  { unPackageHash :: String
  }
  deriving (Eq, Ord, Show)

data ResolvedDependency = ResolvedDependency
  { resolvedDependencySpec :: !PackageSpec,
    resolvedDependencyHash :: !PackageHash
  }
  deriving (Eq, Show)

data PackageVariantKey = PackageVariantKey
  { packageKeySpec :: !PackageSpec,
    packageKeyHash :: !PackageHash,
    packageKeyFlags :: ![(String, Bool)],
    packageKeyDependencies :: ![ResolvedDependency]
  }
  deriving (Eq, Show)

data SourceAnalysis = SourceAnalysis
  { sourceCabalFile :: !FilePath,
    sourceCabalBytes :: !BS.ByteString,
    sourceSetupFile :: !(Maybe FilePath),
    sourceSetupBytes :: !BS.ByteString,
    sourceFileCount :: !Int,
    sourceFlagAssignments :: ![(String, Bool)],
    sourceDependencyNames :: ![String]
  }

data SourceAnalysisMode
  = AllowSourceWrites
  | AvoidSourceWrites

instance ToJSON ArtifactManifest where
  toJSON manifest =
    object
      [ "schemaVersion" .= (1 :: Int),
        "package" .= packageSpecValue (packageKeySpec (manifestPackageKey manifest)),
        "packageKey" .= packageVariantKeyValue (manifestPackageKey manifest),
        "sourcePath" .= manifestSourcePath manifest,
        "cabalFile" .= manifestCabalFile manifest,
        "setupFile" .= manifestSetupFile manifest,
        "storePath" .= manifestStorePath manifest,
        "interfacePath" .= manifestInterfacePath manifest,
        "fcPath" .= manifestFcPath manifest,
        "sourceFileCount" .= manifestSourceFileCount manifest,
        "phases" .= manifestPhases manifest
      ]

instance ToJSON PhaseManifest where
  toJSON phase =
    object
      [ "name" .= phaseName phase,
        "status" .= phaseStatus phase,
        "description" .= phaseDescription phase
      ]

instance ToJSON PhaseStatus where
  toJSON status =
    Aeson.String $
      case status of
        Planned -> "planned"
        Unimplemented -> "unimplemented"
        Complete -> "complete"

runInstall :: InstallOptions -> IO ()
runInstall opts = do
  storeRoot <- maybe defaultStoreRoot pure (installStoreRoot opts)
  version <- resolveVersion opts
  let spec = PackageSpec (installPackageName opts) version
      resolver = hackageDependencyResolver opts
  plan <-
    if installDryRun opts
      then buildDryRunPackagePlanWithResolver resolver storeRoot spec
      else buildPackagePlanWithResolver resolver storeRoot spec
  result <-
    if installDryRun opts
      then dryRunInstallScaffold plan
      else writeInstallScaffold plan
  putStrLn ("store: " <> resultStorePath result)
  putStrLn ("manifest: " <> resultManifestPath result)
  putStrLn ("interfaces: " <> resultInterfacePath result <> " (unimplemented)")
  putStrLn ("system-fc: " <> resultFcPath result <> " (unimplemented)")
  when (installDryRun opts) $
    putStrLn "dry-run: no files written"

hackageDependencyResolver :: InstallOptions -> DependencyResolver
hackageDependencyResolver opts =
  DependencyResolver
    { resolverResolveVersion = resolveDependencyVersion opts,
      resolverSourcePath =
        HackageDownload.downloadPackageWithOptions
          HackageDownload.defaultDownloadOptions
            { HackageDownload.downloadAllowNetwork = not (installOffline opts || installDryRun opts)
            }
    }

resolveDependencyVersion :: InstallOptions -> String -> IO String
resolveDependencyVersion opts packageName
  | installOffline opts =
      ioError (userError ("aihc install --offline cannot resolve dependency " <> packageName <> " without cached dependency-plan metadata"))
  | otherwise = do
      result <- getLatestVersion Nothing packageName
      case result of
        Right version -> pure version
        Left err -> ioError (userError err)

resolveVersion :: InstallOptions -> IO String
resolveVersion opts =
  case installPackageVersion opts of
    Just version -> pure version
    Nothing
      | installOffline opts ->
          ioError (userError "aihc install --offline requires --version because latest-version resolution needs Hackage metadata")
      | otherwise -> do
          result <- getLatestVersion Nothing (installPackageName opts)
          case result of
            Right version -> pure version
            Left err -> ioError (userError err)

defaultStoreRoot :: IO FilePath
defaultStoreRoot = do
  cacheDir <- getXdgDirectory XdgCache "aihc"
  pure (cacheDir </> "store")

buildPackagePlanWithResolver :: DependencyResolver -> FilePath -> PackageSpec -> IO PackagePlan
buildPackagePlanWithResolver resolver storeRoot spec =
  snd <$> buildPackagePlanRecursive AllowSourceWrites resolver storeRoot [] spec

buildDryRunPackagePlanWithResolver :: DependencyResolver -> FilePath -> PackageSpec -> IO PackagePlan
buildDryRunPackagePlanWithResolver resolver storeRoot spec =
  snd <$> buildPackagePlanRecursive AvoidSourceWrites resolver storeRoot [] spec

buildPackagePlanRecursive :: SourceAnalysisMode -> DependencyResolver -> FilePath -> [PackageSpec] -> PackageSpec -> IO (ResolvedDependency, PackagePlan)
buildPackagePlanRecursive mode resolver storeRoot stack spec
  | packageSpecIdentity spec `elem` map packageSpecIdentity stack =
      ioError (userError ("Cyclic dependency while installing " <> formatPackage spec))
  | otherwise = do
      sourcePath <- resolverSourcePath resolver spec
      analysis <- analyzeSourceWith mode sourcePath
      dependencySpecs <- mapM resolveDependencySpec (sourceDependencyNames analysis)
      dependencyPlans <- mapM (buildPackagePlanRecursive mode resolver storeRoot (spec : stack)) dependencySpecs
      let plan =
            buildPackagePlanFromAnalysis
              storeRoot
              spec
              sourcePath
              (map fst dependencyPlans)
              analysis
      pure (resolvedDependencyFromPlan plan, plan)
  where
    resolveDependencySpec dependencyName = do
      version <- resolverResolveVersion resolver dependencyName
      pure (PackageSpec dependencyName version)

packageSpecIdentity :: PackageSpec -> (String, String)
packageSpecIdentity spec =
  (pkgName spec, pkgVersion spec)

resolvedDependencyFromPlan :: PackagePlan -> ResolvedDependency
resolvedDependencyFromPlan plan =
  ResolvedDependency
    { resolvedDependencySpec = packageKeySpec key,
      resolvedDependencyHash = packageKeyHash key
    }
  where
    key = planPackageKey plan

buildPackagePlanFromSource :: FilePath -> PackageSpec -> FilePath -> IO PackagePlan
buildPackagePlanFromSource storeRoot spec sourcePath = do
  analysis <- analyzeSourceWith AllowSourceWrites sourcePath
  pure (buildPackagePlanFromAnalysis storeRoot spec sourcePath [] analysis)

buildPackagePlanFromAnalysis :: FilePath -> PackageSpec -> FilePath -> [ResolvedDependency] -> SourceAnalysis -> PackagePlan
buildPackagePlanFromAnalysis storeRoot spec sourcePath dependencies analysis =
  let sortedDependencies = sortDependencies dependencies
      packageHash =
        computePackageHash
          spec
          (sourceFlagAssignments analysis)
          sortedDependencies
          (sourceCabalBytes analysis)
          (sourceSetupBytes analysis)
      storePath = storeRoot </> (unPackageHash packageHash <> "-" <> sanitizeName (formatPackage spec))
   in PackagePlan
        { planPackageKey =
            PackageVariantKey
              { packageKeySpec = spec,
                packageKeyHash = packageHash,
                packageKeyFlags = sourceFlagAssignments analysis,
                packageKeyDependencies = sortedDependencies
              },
          planSourcePath = sourcePath,
          planCabalFile = sourceCabalFile analysis,
          planSetupFile = sourceSetupFile analysis,
          planStoreRoot = storeRoot,
          planStorePath = storePath,
          planSourceFileCount = sourceFileCount analysis
        }

analyzeSourceWith :: SourceAnalysisMode -> FilePath -> IO SourceAnalysis
analyzeSourceWith mode sourcePath = do
  cabalFiles <- HackageUtil.findCabalFiles sourcePath
  cabalFile <-
    case cabalFiles of
      [] -> ioError (userError ("No .cabal file found under " <> sourcePath))
      files -> pure (HackageUtil.chooseBestCabalFile sourcePath files)
  cabalBytes <- BS.readFile cabalFile
  gpd <-
    case runParseResult (parseGenericPackageDescription cabalBytes) of
      (_, Right parsed) -> pure parsed
      (_, Left (_, errs)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errs))
  sourceFileCount <- analyzeSourceFileCount mode gpd sourcePath
  setupFile <- findSetupFile sourcePath
  setupBytes <- maybe (pure BS.empty) BS.readFile setupFile
  pure
    SourceAnalysis
      { sourceCabalFile = cabalFile,
        sourceCabalBytes = cabalBytes,
        sourceSetupFile = setupFile,
        sourceSetupBytes = setupBytes,
        sourceFileCount = sourceFileCount,
        sourceFlagAssignments = packageFlagAssignments gpd,
        sourceDependencyNames = packageDependencyNames gpd
      }

analyzeSourceFileCount :: SourceAnalysisMode -> GenericPackageDescription -> FilePath -> IO Int
analyzeSourceFileCount mode gpd sourcePath =
  case mode of
    AllowSourceWrites -> length <$> HackageCabal.collectComponentFiles gpd sourcePath
    AvoidSourceWrites -> pure 0

writeInstallScaffold :: PackagePlan -> IO InstallResult
writeInstallScaffold plan = do
  let result = installResultForPlan plan
      manifest = artifactManifestForPlan result plan
      manifestPath = resultManifestPath result
      interfacePath = resultInterfacePath result
      fcPath = resultFcPath result
  createDirectoryIfMissing True (takeDirectory manifestPath)
  createDirectoryIfMissing True (takeDirectory interfacePath)
  createDirectoryIfMissing True (takeDirectory fcPath)
  BL.writeFile manifestPath (Aeson.encode manifest)
  BL.writeFile interfacePath (Aeson.encode (interfacePlaceholder plan))
  BL.writeFile fcPath (Aeson.encode (fcPlaceholder plan))
  pure result

dryRunInstallScaffold :: PackagePlan -> IO InstallResult
dryRunInstallScaffold =
  pure . installResultForPlan

installResultForPlan :: PackagePlan -> InstallResult
installResultForPlan plan =
  InstallResult
    { resultStorePath = storePath,
      resultManifestPath = manifestPath,
      resultInterfacePath = interfacePath,
      resultFcPath = fcPath
    }
  where
    storePath = planStorePath plan
    manifestPath = storePath </> "manifest.json"
    interfacePath = storePath </> "interfaces" </> "package-interface.json"
    fcPath = storePath </> "fc" </> "package-fc.json"

artifactManifestForPlan :: InstallResult -> PackagePlan -> ArtifactManifest
artifactManifestForPlan result plan =
  ArtifactManifest
    { manifestPackageKey = planPackageKey plan,
      manifestSourcePath = planSourcePath plan,
      manifestCabalFile = planCabalFile plan,
      manifestSetupFile = planSetupFile plan,
      manifestStorePath = resultStorePath result,
      manifestInterfacePath = resultInterfacePath result,
      manifestFcPath = resultFcPath result,
      manifestSourceFileCount = planSourceFileCount plan,
      manifestPhases = plannedPhases
    }

plannedPhases :: [PhaseManifest]
plannedPhases =
  [ PhaseManifest "resolve-dependency-closure" Planned "Resolve dependency versions recursively and key each package variant by direct dependency hashes",
    PhaseManifest "compile-setup" Unimplemented "Compile Setup.hs or Setup.lhs with ghc in an isolated work directory",
    PhaseManifest "configure-package" Unimplemented "Use the Cabal library to configure package components without invoking cabal-install",
    PhaseManifest "run-external-processors" Planned "Reserve processors such as happy, alex, and c2hs for reproducible generated sources",
    PhaseManifest "compile-interfaces" Unimplemented "Generate name-resolution, type, and fixity interface data",
    PhaseManifest "desugar-system-fc" Unimplemented "Generate desugared System-FC data files"
  ]

interfacePlaceholder :: PackagePlan -> Aeson.Value
interfacePlaceholder plan =
  object
    [ "schemaVersion" .= (1 :: Int),
      "packageKey" .= packageVariantKeyValue (planPackageKey plan),
      "status" .= ("unimplemented" :: String),
      "contains" .= (["name-resolution", "types", "fixities"] :: [String])
    ]

fcPlaceholder :: PackagePlan -> Aeson.Value
fcPlaceholder plan =
  object
    [ "schemaVersion" .= (1 :: Int),
      "packageKey" .= packageVariantKeyValue (planPackageKey plan),
      "status" .= ("unimplemented" :: String),
      "contains" .= (["system-fc"] :: [String])
    ]

findSetupFile :: FilePath -> IO (Maybe FilePath)
findSetupFile sourcePath = do
  let setupHs = sourcePath </> "Setup.hs"
      setupLhs = sourcePath </> "Setup.lhs"
  setupHsExists <- doesFileExist setupHs
  setupLhsExists <- doesFileExist setupLhs
  pure $
    if setupHsExists
      then Just setupHs
      else
        if setupLhsExists
          then Just setupLhs
          else Nothing

stableHash :: [BS.ByteString] -> String
stableHash chunks =
  padLeft 16 '0' (showHex digest "")
  where
    digest = foldl' hashChunk fnvOffset chunks
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * fnvPrime)

fnvOffset :: Word64
fnvOffset = 14695981039346656037

fnvPrime :: Word64
fnvPrime = 1099511628211

padLeft :: Int -> Char -> String -> String
padLeft width char value =
  replicate (max 0 (width - length value)) char <> value

packageSpecValue :: PackageSpec -> Aeson.Value
packageSpecValue spec =
  object
    [ "name" .= pkgName spec,
      "version" .= pkgVersion spec
    ]

packageVariantKeyValue :: PackageVariantKey -> Aeson.Value
packageVariantKeyValue key =
  object
    [ "hash" .= unPackageHash (packageKeyHash key),
      "package" .= packageSpecValue (packageKeySpec key),
      "flags" .= map flagAssignmentValue (packageKeyFlags key),
      "dependencies" .= map resolvedDependencyValue (packageKeyDependencies key)
    ]

resolvedDependencyValue :: ResolvedDependency -> Aeson.Value
resolvedDependencyValue dependency =
  object
    [ "package" .= packageSpecValue (resolvedDependencySpec dependency),
      "hash" .= unPackageHash (resolvedDependencyHash dependency)
    ]

flagAssignmentValue :: (String, Bool) -> Aeson.Value
flagAssignmentValue (flag, enabled) =
  object
    [ "name" .= flag,
      "enabled" .= enabled
    ]

packageFlagAssignments :: GenericPackageDescription -> [(String, Bool)]
packageFlagAssignments gpd =
  sort [(unFlagName (flagName flag), flagDefault flag) | flag <- genPackageFlags gpd]

packageDependencyNames :: GenericPackageDescription -> [String]
packageDependencyNames gpd =
  sort . nub . map T.unpack $
    concatMap libraryDependencies libraryTrees
      <> concatMap (executableDependencies . snd) (condExecutables gpd)
  where
    evalCond = HackageCabal.conditionEvaluator gpd
    libraryTrees =
      maybe [] pure (condLibrary gpd)
        <> map snd (condSubLibraries gpd)

    libraryDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond libBuildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []

    executableDependencies tree =
      let build = HackageCabal.collectMergedBuildInfo evalCond buildInfo tree
       in if buildable build
            then HackageCabal.extractDependencies build
            else []

sortDependencies :: [ResolvedDependency] -> [ResolvedDependency]
sortDependencies =
  sortOn (\dependency -> (pkgName (resolvedDependencySpec dependency), pkgVersion (resolvedDependencySpec dependency), unPackageHash (resolvedDependencyHash dependency)))

computePackageHash :: PackageSpec -> [(String, Bool)] -> [ResolvedDependency] -> BS.ByteString -> BS.ByteString -> PackageHash
computePackageHash spec flags dependencies cabalBytes setupBytes =
  PackageHash $
    stableHash
      [ BSC.pack "aihc-package-hash-v1",
        BSC.pack (pkgName spec),
        BSC.pack (pkgVersion spec),
        BSC.pack (show flags),
        BSC.pack (show [(pkgName depSpec, pkgVersion depSpec, unPackageHash (resolvedDependencyHash dep)) | dep <- dependencies, let depSpec = resolvedDependencySpec dep]),
        cabalBytes,
        setupBytes,
        BSC.pack "tools:happy,alex,c2hs:planned",
        BSC.pack "phases:setup,configure,build,interfaces,system-fc"
      ]
