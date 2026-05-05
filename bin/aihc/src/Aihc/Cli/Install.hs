module Aihc.Cli.Install
  ( ArtifactManifest (..),
    InstallResult (..),
    PackagePlan (..),
    PhaseManifest (..),
    PhaseStatus (..),
    buildPackagePlanFromSource,
    defaultStoreRoot,
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
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Word (Word64)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Numeric (showHex)
import System.Directory
  ( XdgDirectory (XdgCache),
    createDirectoryIfMissing,
    doesFileExist,
    getXdgDirectory,
  )
import System.FilePath (takeDirectory, (</>))

data PackagePlan = PackagePlan
  { planPackageSpec :: !PackageSpec,
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
  { manifestPackageSpec :: !PackageSpec,
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

instance ToJSON ArtifactManifest where
  toJSON manifest =
    object
      [ "schemaVersion" .= (1 :: Int),
        "package" .= packageSpecValue (manifestPackageSpec manifest),
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
  sourcePath <-
    HackageDownload.downloadPackageWithOptions
      HackageDownload.defaultDownloadOptions
        { HackageDownload.downloadAllowNetwork = not (installOffline opts)
        }
      spec
  plan <- buildPackagePlanFromSource storeRoot spec sourcePath
  result <- writeInstallScaffold plan
  putStrLn ("store: " <> resultStorePath result)
  putStrLn ("manifest: " <> resultManifestPath result)
  putStrLn ("interfaces: " <> resultInterfacePath result <> " (unimplemented)")
  putStrLn ("system-fc: " <> resultFcPath result <> " (unimplemented)")

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

buildPackagePlanFromSource :: FilePath -> PackageSpec -> FilePath -> IO PackagePlan
buildPackagePlanFromSource storeRoot spec sourcePath = do
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
  sourceFiles <- HackageCabal.collectComponentFiles gpd sourcePath
  setupFile <- findSetupFile sourcePath
  setupBytes <- maybe (pure BS.empty) BS.readFile setupFile
  let fingerprint =
        stableHash
          [ BSC.pack (pkgName spec),
            BSC.pack (pkgVersion spec),
            cabalBytes,
            setupBytes,
            BSC.pack "tools:happy,alex,c2hs:planned",
            BSC.pack "phases:setup,configure,build,interfaces,system-fc"
          ]
      storePath = storeRoot </> (fingerprint <> "-" <> sanitizeName (formatPackage spec))
  pure
    PackagePlan
      { planPackageSpec = spec,
        planSourcePath = sourcePath,
        planCabalFile = cabalFile,
        planSetupFile = setupFile,
        planStoreRoot = storeRoot,
        planStorePath = storePath,
        planSourceFileCount = length sourceFiles
      }

writeInstallScaffold :: PackagePlan -> IO InstallResult
writeInstallScaffold plan = do
  let storePath = planStorePath plan
      manifestPath = storePath </> "manifest.json"
      interfacePath = storePath </> "interfaces" </> "package-interface.json"
      fcPath = storePath </> "fc" </> "package-fc.json"
      manifest =
        ArtifactManifest
          { manifestPackageSpec = planPackageSpec plan,
            manifestSourcePath = planSourcePath plan,
            manifestCabalFile = planCabalFile plan,
            manifestSetupFile = planSetupFile plan,
            manifestStorePath = storePath,
            manifestInterfacePath = interfacePath,
            manifestFcPath = fcPath,
            manifestSourceFileCount = planSourceFileCount plan,
            manifestPhases = plannedPhases
          }
  createDirectoryIfMissing True (takeDirectory manifestPath)
  createDirectoryIfMissing True (takeDirectory interfacePath)
  createDirectoryIfMissing True (takeDirectory fcPath)
  BL.writeFile manifestPath (Aeson.encode manifest)
  BL.writeFile interfacePath (Aeson.encode (interfacePlaceholder plan))
  BL.writeFile fcPath (Aeson.encode (fcPlaceholder plan))
  pure
    InstallResult
      { resultStorePath = storePath,
        resultManifestPath = manifestPath,
        resultInterfacePath = interfacePath,
        resultFcPath = fcPath
      }

plannedPhases :: [PhaseManifest]
plannedPhases =
  [ PhaseManifest "compile-setup" Unimplemented "Compile Setup.hs or Setup.lhs with ghc in an isolated work directory",
    PhaseManifest "configure-package" Unimplemented "Use the Cabal library to configure package components without invoking cabal-install",
    PhaseManifest "run-external-processors" Planned "Reserve processors such as happy, alex, and c2hs for reproducible generated sources",
    PhaseManifest "compile-interfaces" Unimplemented "Generate name-resolution, type, and fixity interface data",
    PhaseManifest "desugar-system-fc" Unimplemented "Generate desugared System-FC data files"
  ]

interfacePlaceholder :: PackagePlan -> Aeson.Value
interfacePlaceholder plan =
  object
    [ "schemaVersion" .= (1 :: Int),
      "package" .= packageSpecValue (planPackageSpec plan),
      "status" .= ("unimplemented" :: String),
      "contains" .= (["name-resolution", "types", "fixities"] :: [String])
    ]

fcPlaceholder :: PackagePlan -> Aeson.Value
fcPlaceholder plan =
  object
    [ "schemaVersion" .= (1 :: Int),
      "package" .= packageSpecValue (planPackageSpec plan),
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
