{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ResolvePackage
  ( InterfaceFormat (..),
    Options (..),
    ResolveModuleIface (..),
    ResolvePackageIface (..),
    dependencyClosure,
    formatDependencyFailure,
    interfaceFromExports,
    optionsParser,
    renderInterfaceJSON,
    renderInterfaceYAML,
    run,
    targetLayers,
  )
where

import Aihc.Hackage.Stackage (loadStackageSnapshot)
import Aihc.Hackage.Types (PackageSpec (..))
import Aihc.Resolve (ModuleExports, Scope (..))
import BootInterface (bootPackageNames, loadBootInterfaces)
import Control.Exception (SomeException, try)
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BL
import Data.List (find, sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Yaml
import Options.Applicative qualified as OA
import ResolveStackageProgress
  ( PackageInfo (..),
    PackageStatus (..),
    collectPackageInfo,
    kahnLayers,
    processLayers,
  )
import ResolveStackageProgress qualified as RSP
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)

data InterfaceFormat = InterfaceYAML | InterfaceJSON
  deriving (Eq, Show)

data Options = Options
  { optPackage :: String,
    optSnapshot :: String,
    optOffline :: Bool,
    optFormat :: InterfaceFormat
  }

optionsParser :: OA.Parser Options
optionsParser =
  Options
    <$> OA.strArgument
      ( OA.metavar "PACKAGE"
          <> OA.help "Package name from the selected Stackage snapshot"
      )
    <*> OA.strOption
      ( OA.long "snapshot"
          <> OA.metavar "SNAPSHOT"
          <> OA.value "lts-24.33"
          <> OA.showDefault
          <> OA.help "Stackage snapshot to resolve"
      )
    <*> OA.switch
      ( OA.long "offline"
          <> OA.help "Use only cached packages, don't download"
      )
    <*> OA.option
      parseFormat
      ( OA.long "format"
          <> OA.metavar "yaml|json"
          <> OA.value InterfaceYAML
          <> OA.showDefaultWith renderFormat
          <> OA.help "Interface output format"
      )

parseFormat :: OA.ReadM InterfaceFormat
parseFormat =
  OA.eitherReader $ \case
    "yaml" -> Right InterfaceYAML
    "json" -> Right InterfaceJSON
    _ -> Left "expected yaml or json"

renderFormat :: InterfaceFormat -> String
renderFormat InterfaceYAML = "yaml"
renderFormat InterfaceJSON = "json"

data ResolvePackageIface = ResolvePackageIface
  { rpiPackage :: Text,
    rpiModules :: [ResolveModuleIface]
  }
  deriving (Eq, Show)

instance ToJSON ResolvePackageIface where
  toJSON rpi =
    object
      [ "package" .= rpiPackage rpi,
        "modules" .= rpiModules rpi
      ]

data ResolveModuleIface = ResolveModuleIface
  { rmiModule :: Text,
    rmiTerms :: [Text],
    rmiTypes :: [Text],
    rmiConstructors :: Map Text [Text],
    rmiMethods :: Map Text [Text]
  }
  deriving (Eq, Show)

instance ToJSON ResolveModuleIface where
  toJSON rmi =
    object
      [ "module" .= rmiModule rmi,
        "terms" .= rmiTerms rmi,
        "types" .= rmiTypes rmi,
        "constructors" .= rmiConstructors rmi,
        "methods" .= rmiMethods rmi
      ]

interfaceFromExports :: Text -> ModuleExports -> ResolvePackageIface
interfaceFromExports pkg iface =
  ResolvePackageIface
    { rpiPackage = pkg,
      rpiModules = map (uncurry moduleIfaceFromScope) (Map.toAscList iface)
    }

moduleIfaceFromScope :: Text -> Scope -> ResolveModuleIface
moduleIfaceFromScope modName scope =
  ResolveModuleIface
    { rmiModule = modName,
      rmiTerms = sort (Map.keys (scopeTerms scope)),
      rmiTypes = sort (Map.keys (scopeTypes scope)),
      rmiConstructors = fmap sort (scopeConstructors scope),
      rmiMethods = fmap sort (scopeMethods scope)
    }

renderInterfaceJSON :: ResolvePackageIface -> BL.ByteString
renderInterfaceJSON = encodePretty

renderInterfaceYAML :: ResolvePackageIface -> BL.ByteString
renderInterfaceYAML = BL.fromStrict . Yaml.encode

dependencyClosure :: Text -> Map Text [Text] -> Set Text
dependencyClosure target depGraph = go Set.empty [target]
  where
    go seen [] = seen
    go seen (pkg : rest)
      | pkg `Set.member` seen = go seen rest
      | otherwise =
          let deps = Map.findWithDefault [] pkg depGraph
           in go (Set.insert pkg seen) (deps ++ rest)

targetLayers :: Text -> Map Text [Text] -> [[Text]]
targetLayers target depGraph =
  let closure = dependencyClosure target depGraph
      subGraph =
        Map.fromSet
          (\pkg -> filter (`Set.member` closure) (Map.findWithDefault [] pkg depGraph))
          closure
   in kahnLayers subGraph

formatDependencyFailure :: Text -> Map Text [Text] -> Map Text PackageStatus -> String
formatDependencyFailure target depGraph results =
  let closure = Set.delete target (dependencyClosure target depGraph)
      failed =
        [ (pkg, msg)
        | pkg <- Set.toAscList closure,
          Just (PkgFailed msg) <- [Map.lookup pkg results]
        ]
      skipped =
        [ pkg
        | pkg <- Set.toAscList closure,
          Just PkgSkipped <- [Map.lookup pkg results]
        ]
      renderFailed (pkg, msg) =
        "  " ++ T.unpack pkg ++ ":\n" ++ unlines ["    " ++ line | line <- take 5 (lines msg)]
      failedBlock = concatMap renderFailed failed
      skippedBlock =
        if null skipped
          then ""
          else "Skipped because dependencies failed: " ++ T.unpack (T.intercalate ", " skipped) ++ "\n"
   in "Could not resolve " ++ T.unpack target ++ " because dependencies failed.\n" ++ failedBlock ++ skippedBlock

run :: Options -> IO ()
run opts = do
  snapshotResult <- loadStackageSnapshot Nothing (optSnapshot opts) (optOffline opts)
  packages <- case snapshotResult of
    Left err -> hPutStrLn stderr ("Failed to load snapshot: " ++ err) >> exitFailure
    Right pkgs -> pure pkgs

  let target = T.pack (optPackage opts)
      snapshotNames = Set.fromList (map (T.pack . pkgName) packages)
  case find ((== optPackage opts) . pkgName) packages of
    Nothing -> do
      hPutStrLn stderr ("Package not found in " ++ optSnapshot opts ++ ": " ++ optPackage opts)
      exitFailure
    Just targetSpec
      | target `Set.member` bootPackageNames -> resolveBootTarget opts target
      | otherwise -> resolveSourceTarget opts target targetSpec packages snapshotNames

resolveBootTarget :: Options -> Text -> IO ()
resolveBootTarget opts target = do
  bootIfaceMap <- loadBootInterfaces
  case Map.lookup target bootIfaceMap of
    Nothing -> hPutStrLn stderr ("Boot interface not found for " ++ T.unpack target) >> exitFailure
    Just iface -> writeInterface opts (interfaceFromExports target iface)

resolveSourceTarget :: Options -> Text -> PackageSpec -> [PackageSpec] -> Set Text -> IO ()
resolveSourceTarget opts target targetSpec packages snapshotNames = do
  targetInfoResult <- try (collectPackageInfo (optOffline opts) targetSpec snapshotNames)
  (targetName, targetInfo) <- case targetInfoResult of
    Left (e :: SomeException) -> hPrint stderr e >> exitFailure
    Right r -> pure r
  let firstDepGraph = Map.singleton targetName (piSnapshotDeps targetInfo)
      initialClosure = dependencyClosure target firstDepGraph
      packageMap = Map.fromList [(T.pack (pkgName spec), spec) | spec <- packages]
  infos <- collectClosureInfos opts packageMap snapshotNames (Map.singleton targetName targetInfo) initialClosure
  let depGraph = Map.fromList [(name, piSnapshotDeps info) | (name, info) <- Map.toList infos]
      closure = dependencyClosure target depGraph
      missing = Set.toList (Set.difference closure (Map.keysSet infos `Set.union` bootPackageNames))
  if null missing
    then pure ()
    else do
      hPutStrLn stderr ("Missing package info for: " ++ T.unpack (T.intercalate ", " missing))
      exitFailure

  bootIfaceMap <- loadBootInterfaces
  let bootExports = foldl' Map.union Map.empty (Map.elems bootIfaceMap)
      sourceClosure = Set.difference closure bootPackageNames
      sourceInfos = Map.restrictKeys infos sourceClosure
      sourceDepGraph = Map.map (filter (`Set.member` sourceClosure)) (Map.restrictKeys depGraph sourceClosure)
      layers = targetLayers target sourceDepGraph
      bootResults =
        Map.fromList
          [(pkg, PkgSuccess iface) | (pkg, iface) <- Map.toList bootIfaceMap, pkg `Set.member` closure]
  results <- processLayers progressOptions bootExports layers sourceInfos sourceDepGraph bootResults
  case Map.lookup target results of
    Just (PkgSuccess iface) -> writeInterface opts (interfaceFromExports target iface)
    Just (PkgFailed msg) -> hPutStrLn stderr msg >> exitFailure
    Just PkgSkipped -> hPutStrLn stderr (formatDependencyFailure target depGraph results) >> exitFailure
    Nothing -> hPutStrLn stderr ("No result produced for " ++ T.unpack target) >> exitFailure
  where
    progressOptions =
      RSP.Options
        { RSP.optSnapshot = optSnapshot opts,
          RSP.optJobs = 1,
          RSP.optOffline = optOffline opts,
          RSP.optTopFailures = 10
        }

collectClosureInfos ::
  Options ->
  Map Text PackageSpec ->
  Set Text ->
  Map Text PackageInfo ->
  Set Text ->
  IO (Map Text PackageInfo)
collectClosureInfos opts packageMap snapshotNames = go
  where
    go infos wanted =
      let sourceWanted = Set.difference wanted bootPackageNames
          missing = Set.difference sourceWanted (Map.keysSet infos)
       in if Set.null missing
            then pure infos
            else do
              newInfos <- mapM collectOne (Set.toList missing)
              let infos' = foldl' (\acc (name, info) -> Map.insert name info acc) infos newInfos
                  depGraph = Map.fromList [(name, piSnapshotDeps info) | (name, info) <- Map.toList infos']
                  wanted' = foldl' Set.union wanted [dependencyClosure name depGraph | name <- Map.keys infos']
              go infos' wanted'
    collectOne name =
      case Map.lookup name packageMap of
        Nothing -> pure (name, PackageInfo "" [] [])
        Just spec -> collectPackageInfo (optOffline opts) spec snapshotNames

writeInterface :: Options -> ResolvePackageIface -> IO ()
writeInterface opts iface =
  BL.putStr $
    case optFormat opts of
      InterfaceYAML -> renderInterfaceYAML iface
      InterfaceJSON -> renderInterfaceJSON iface
