module Aihc.Cli.InstallV2
  ( InstallV2Result (..),
    installV2,
    runInstallV2,
  )
where

import Aihc.Cli.Install (ParsedInterfaceFile (..), parseInterfaceFile)
import Aihc.Cli.Options (InstallV2Options (..))
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact, encodeResolveArtifact, encodeResolveScope)
import Aihc.Cli.Store (defaultStoreRoot)
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Parser.Syntax (ImportDecl (..), Module, moduleName)
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
    Package (..),
    PackageId (..),
    ResolveResult (..),
    extractInterfaceWithDeps,
    modulesInPackage,
    resolveWithDeps,
  )
import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, when)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub, sort, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath (makeRelative, takeDirectory, (</>))

data InstallV2Result = InstallV2Result
  { installV2StorePath :: !FilePath,
    installV2WrittenModules :: ![Text],
    installV2ReusedModules :: ![Text]
  }
  deriving (Eq, Show)

data SourceModule = SourceModule
  { sourceModulePath :: !FilePath,
    sourceModuleHash :: !Text,
    sourceModuleAst :: !Module
  }

runInstallV2 :: InstallV2Options -> IO ()
runInstallV2 options = do
  result <- installV2 options
  putStrLn ("store: " <> installV2StorePath result)

installV2 :: InstallV2Options -> IO InstallV2Result
installV2 options = do
  storeRoot <- maybe defaultStoreRoot pure (installV2StoreRoot options)
  let root = installV2PackageDirectory options
      verbose message = when (installV2Verbose options) (putStrLn message)
  verbose ("Read Cabal package: " <> root)
  cabalFiles <- HackageUtil.findCabalFiles root
  cabalFile <- case cabalFiles of
    [] -> ioError (userError ("No .cabal file found under " <> root))
    files -> pure (HackageUtil.chooseBestCabalFile root files)
  cabalBytes <- BS.readFile cabalFile
  gpd <- case runParseResult (parseGenericPackageDescription cabalBytes) of
    (_, Right value) -> pure value
    (_, Left (_, errors)) -> ioError (userError ("Failed to parse " <> cabalFile <> ": " <> show errors))
  files <- HackageCabal.collectLibraryFiles gpd root
  let packageId = package (packageDescription gpd)
      packageNameText = T.pack (CabalPackage.unPackageName (CabalPackage.packageName packageId))
      packageVersionText = T.pack (prettyShow (CabalPackage.packageVersion packageId))
      dependencyNames = sort . nub $ concatMap (filter (/= packageNameText) . HackageCabal.fileInfoDependencies) files
  dependencyIds <- resolveDependencyIds storeRoot dependencyNames (installV2Dependencies options)
  verbose ("Use library dependencies: " <> show dependencyIds)
  verbose ("Parse " <> show (length files) <> " library modules")
  parsed <- mapM (parseSource root) files
  let packageHash = stableHash (TE.encodeUtf8 "aihc-dependencies-v1" : map TE.encodeUtf8 dependencyIds)
      packageDirectory = T.unpack packageNameText <> "-" <> T.unpack packageVersionText <> "-" <> packageHash
      storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId (T.pack packageDirectory))
      units = sourceModuleSccs parsed
  verbose ("Compute " <> show (length units) <> " SCC units")
  (_, _, written, reused) <- foldM (installUnit verbose storePath resolvePackage root) (Map.empty, Map.empty, [], []) units
  pure (InstallV2Result storePath (reverse written) (reverse reused))

resolveDependencyIds :: FilePath -> [Text] -> [String] -> IO [Text]
resolveDependencyIds storeRoot expected assignments = do
  parsed <- mapM parseAssignment assignments
  let selected = Map.fromList parsed
      selectedNames = sort (Map.keys selected)
  unless (selectedNames == expected) $ ioError (userError ("Direct library dependencies require exact --dependency inputs: " <> show expected))
  mapM validateIdentity (Map.toAscList selected)
  where
    parseAssignment assignment =
      case break (== '=') assignment of
        (name, '=' : identity) | not (null name) && not (null identity) -> pure (T.pack name, T.pack identity)
        _ -> ioError (userError ("Invalid dependency identity: " <> assignment))
    validateIdentity (name, identity) = do
      unless ((name <> "-") `T.isPrefixOf` identity) $ ioError (userError ("Dependency identity does not match " <> T.unpack name <> ": " <> T.unpack identity))
      exists <- doesDirectoryExist (storeRoot </> T.unpack identity)
      unless exists $ ioError (userError ("Dependency artifact is not installed: " <> T.unpack identity))
      pure identity

parseSource :: FilePath -> HackageCabal.FileInfo -> IO SourceModule
parseSource root fileInfo = do
  bytes <- BS.readFile (HackageCabal.fileInfoPath fileInfo)
  parsed <- parseInterfaceFile root fileInfo
  case parsed of
    ParsedFileOk path modu _ _ -> pure (SourceModule path (T.pack (stableHash [bytes])) modu)
    ParsedFileFailed path _ _ _ -> ioError (userError ("Failed to parse " <> path))

sourceModuleSccs :: [SourceModule] -> [[SourceModule]]
sourceModuleSccs = map flatten . stronglyConnComp . map node
  where
    node source = (source, sourceName source, map importDeclModule (moduleImportsOf source))
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst
    moduleImportsOf = Syntax.moduleImports . sourceModuleAst
    flatten (AcyclicSCC value) = [value]
    flatten (CyclicSCC values) = values

installUnit :: (String -> IO ()) -> FilePath -> Package -> FilePath -> (ModuleExports, Map.Map Text Text, [Text], [Text]) -> [SourceModule] -> IO (ModuleExports, Map.Map Text Text, [Text], [Text])
installUnit verbose storePath resolvePackage root (dependencyExports, scopeHashes, written, reused) unit = do
  let packageModules = modulesInPackage resolvePackage (map sourceModuleAst unit)
      unitNames = map sourceName unit
      importedNames = nub (concatMap (map importDeclModule . Syntax.moduleImports . sourceModuleAst) unit)
      dependencyHashes = Map.fromList [("scope:" <> name, digest) | name <- importedNames, name `notElem` unitNames, Just digest <- [Map.lookup name scopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- unit]
      hashes = sortOn fst (sourceHashes <> Map.toList dependencyHashes)
      artifactPath source = storePath </> moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
  cachedExports <- tryReadUnitArtifacts hashes resolvePackage artifactPath unit
  case cachedExports of
    Just diskExports -> do
      mapM_ (verbose . ("Reuse resolve context: " <>) . T.unpack) unitNames
      updatedScopeHashes <- updateScopeHashes artifactPath scopeHashes unit
      pure (diskExports `Map.union` dependencyExports, updatedScopeHashes, written, reverse unitNames <> reused)
    Nothing -> do
      let result = resolveWithDeps dependencyExports packageModules
      unless (null (resolveErrors result)) (ioError (userError ("Name resolution failed: " <> show (resolveErrors result))))
      let exports = extractInterfaceWithDeps dependencyExports result
      mapM_ (\source -> writeArtifact verbose hashes exports resolvePackage (artifactPath source) source) unit
      diskExports <- readUnitArtifacts hashes resolvePackage artifactPath unit
      updatedScopeHashes <- updateScopeHashes artifactPath scopeHashes unit
      pure (diskExports `Map.union` dependencyExports, updatedScopeHashes, reverse unitNames <> written, reused)
  where
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

updateScopeHashes :: (SourceModule -> FilePath) -> Map.Map Text Text -> [SourceModule] -> IO (Map.Map Text Text)
updateScopeHashes artifactPath previous unit = do
  scopeHashes <- mapM artifactScopeHash unit
  pure (foldl' (\hashes (name, digest) -> Map.insert name digest hashes) previous scopeHashes)
  where
    artifactScopeHash source = do
      bytes <- BS.readFile (artifactPath source)
      artifact <- either (\message -> ioError (userError ("Invalid resolve artifact while hashing scope: " <> message))) pure (decodeResolveArtifact bytes)
      let scopeBytes = BL.toStrict (encodeResolveScope (resolveArtifactScope artifact))
      pure (resolveArtifactModuleName artifact, T.pack (stableHash [scopeBytes]))

moduleDirectory :: Module -> FilePath
moduleDirectory = foldl' (</>) "" . map T.unpack . T.splitOn "." . fromMaybe "Main" . moduleName

writeArtifact :: (String -> IO ()) -> [(Text, Text)] -> ModuleExports -> Package -> FilePath -> SourceModule -> IO ()
writeArtifact verbose hashes exports package path source = do
  createDirectoryIfMissing True (takeDirectory path)
  let name = fromMaybe "Main" (moduleName (sourceModuleAst source))
      scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
  BL.writeFile path (encodeResolveArtifact (ResolveArtifact name hashes scope))
  verbose ("Write resolve context: " <> T.unpack name)

tryReadUnitArtifacts :: [(Text, Text)] -> Package -> (SourceModule -> FilePath) -> [SourceModule] -> IO (Maybe ModuleExports)
tryReadUnitArtifacts expected package artifactPath unit = do
  result <- try (readUnitArtifacts expected package artifactPath unit) :: IO (Either IOException ModuleExports)
  pure (either (const Nothing) Just result)

readUnitArtifacts :: [(Text, Text)] -> Package -> (SourceModule -> FilePath) -> [SourceModule] -> IO ModuleExports
readUnitArtifacts expected package artifactPath unit = do
  entries <- mapM readOne unit
  pure (Map.fromList entries)
  where
    readOne source = do
      let path = artifactPath source
      bytes <- BS.readFile path
      artifact <- either (\message -> ioError (userError ("Invalid resolve artifact " <> path <> ": " <> message))) pure (decodeResolveArtifact bytes)
      unless (resolveArtifactInputHashes artifact == expected) (ioError (userError ("Invalid resolve artifact " <> path <> ": input hashes do not match")))
      pure (ModuleKey package (resolveArtifactModuleName artifact), resolveArtifactScope artifact)

stableHash :: [BS.ByteString] -> String
stableHash chunks = replicate (16 - length rendered) '0' <> rendered
  where
    rendered = showHex (foldl' hashChunk (14695981039346656037 :: Word64) chunks) ""
    hashChunk :: Word64 -> BS.ByteString -> Word64
    hashChunk = BS.foldl' (\hash byte -> (hash `xor` fromIntegral byte) * 1099511628211)
