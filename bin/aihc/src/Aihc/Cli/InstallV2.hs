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
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact, encodeTypeArtifact, encodeTypeInterface)
import Aihc.Hackage.Cabal qualified as HackageCabal
import Aihc.Hackage.Util qualified as HackageUtil
import Aihc.Parser.Syntax (ImportDecl (..), Module, Name (..), moduleName)
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Resolve
  ( ModuleExports,
    ModuleKey (..),
    Package (..),
    PackageId (..),
    ResolveResult (..),
    ResolvedName (..),
    Scope (..),
    extractInterfaceWithDeps,
    modulesInPackage,
    resolveWithDeps,
  )
import Aihc.Tc
  ( ClassInfo (..),
    DataTypeInfo (..),
    TcInterface (..),
    TcTermKey (..),
    TyConInfo (..),
    tcConfig,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterfaceConfig,
  )
import Aihc.Tc.Types (tyConModuleName, tyConPackageId)
import Control.Exception (IOException, try)
import Control.Monad (foldM, unless, when)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Distribution.Package qualified as CabalPackage
import Distribution.PackageDescription (package, packageDescription)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Pretty (prettyShow)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing)
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
  verbose ("Parse " <> show (length files) <> " library modules")
  parsed <- mapM (parseSource root) files
  let packageHash = stableHash ["aihc-dependencies-v1"]
      packageDirectory = T.unpack packageNameText <> "-" <> T.unpack packageVersionText <> "-" <> packageHash
      storePath = storeRoot </> packageDirectory
      resolvePackage = Package packageNameText (PackageId (T.pack packageDirectory))
      units = sourceModuleSccs parsed
  verbose ("Compute " <> show (length units) <> " SCC units")
  (_, _, _, _, written, reused) <- foldM (installUnit verbose storePath resolvePackage root) (Map.empty, Map.empty, mempty, Map.empty, Set.empty, Set.empty) units
  pure (InstallV2Result storePath (Set.toAscList written) (Set.toAscList reused))

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

installUnit :: (String -> IO ()) -> FilePath -> Package -> FilePath -> (ModuleExports, Map.Map Text Text, TcInterface, Map.Map Text Text, Set.Set Text, Set.Set Text) -> [SourceModule] -> IO (ModuleExports, Map.Map Text Text, TcInterface, Map.Map Text Text, Set.Set Text, Set.Set Text)
installUnit verbose storePath resolvePackage root (dependencyExports, scopeHashes, dependencyTypes, typeHashes, written, reused) unit = do
  let packageModules = modulesInPackage resolvePackage (map sourceModuleAst unit)
      unitNames = map sourceName unit
      importedNames = nub (concatMap (map importDeclModule . Syntax.moduleImports . sourceModuleAst) unit)
      dependencyHashes = Map.fromList [("scope:" <> name, digest) | name <- importedNames, name `notElem` unitNames, Just digest <- [Map.lookup name scopeHashes]]
      sourceHashes = [("source:" <> T.pack (makeRelative root (sourceModulePath source)), sourceModuleHash source) | source <- unit]
      hashes = sortOn fst (sourceHashes <> Map.toList dependencyHashes)
      resolvePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "resolve.cbor"
      typePath source = storePath </> moduleDirectory (sourceModuleAst source) </> "type.cbor"
  cachedExports <- tryReadUnitArtifacts hashes resolvePackage resolvePath unit
  (diskExports, resolveResult, resolveChanged) <- case cachedExports of
    Just exports -> do
      mapM_ (verbose . ("Reuse resolve context: " <>) . T.unpack) unitNames
      pure (exports, Nothing, False)
    Nothing -> do
      let result = resolveWithDeps dependencyExports packageModules
      unless (null (resolveErrors result)) (ioError (userError ("Name resolution failed: " <> show (resolveErrors result))))
      let exports = extractInterfaceWithDeps dependencyExports result
      mapM_ (\source -> writeArtifact verbose hashes exports resolvePackage (resolvePath source) source) unit
      storedExports <- readUnitArtifacts hashes resolvePackage resolvePath unit
      pure (storedExports, Just result, True)
  updatedScopeHashes <- updateScopeHashes resolvePath scopeHashes unit
  let typeInputs =
        sortOn fst $
          sourceHashes
            <> Map.toList dependencyHashes
            <> [("type:" <> name, digest) | name <- importedNames, name `notElem` unitNames, Just digest <- [Map.lookup name typeHashes]]
  cachedTypes <- tryReadTypeArtifacts typeInputs typePath unit
  (unitTypes, typeChanged) <- case cachedTypes of
    Just interfaces -> do
      mapM_ (verbose . ("Reuse type interface: " <>) . T.unpack) unitNames
      pure (interfaces, False)
    Nothing -> do
      resolved <- case resolveResult of
        Just result -> pure result
        Nothing -> do
          let result = resolveWithDeps dependencyExports packageModules
          unless (null (resolveErrors result)) (ioError (userError ("Name resolution failed: " <> show (resolveErrors result))))
          pure result
      let (checkedModules, completeInterface) =
            typecheckModuleSccWithInterfaceConfig
              (tcConfig (packageId resolvePackage))
              dependencyTypes
              (map snd (resolvedModules resolved))
      unless (all tcModuleSuccess checkedModules) (ioError (userError ("Type check failed: " <> show (concatMap tcModuleDiagnostics checkedModules))))
      let interfaces = map (moduleTypeInterface diskExports resolvePackage completeInterface) unit
      mapM_ (uncurry (writeTypeArtifact verbose typeInputs typePath)) (zip unit interfaces)
      storedInterfaces <- readTypeArtifacts typeInputs typePath unit
      pure (storedInterfaces, True)
  updatedTypeHashes <- updateTypeHashes typePath typeHashes unit
  let changed = resolveChanged || typeChanged
      unitSet = Set.fromList unitNames
      written' = if changed then written <> unitSet else written
      reused' = if changed then reused else reused <> unitSet
  pure
    ( diskExports `Map.union` dependencyExports,
      updatedScopeHashes,
      dependencyTypes <> mconcat unitTypes,
      updatedTypeHashes,
      written',
      reused'
    )
  where
    sourceName = fromMaybe "Main" . moduleName . sourceModuleAst

moduleTypeInterface :: ModuleExports -> Package -> TcInterface -> SourceModule -> TcInterface
moduleTypeInterface exports package interface source =
  interface
    { tcInterfaceTerms = filter visibleTerm (tcInterfaceTerms interface),
      tcInterfaceTyCons = filter visibleTyCon (tcInterfaceTyCons interface),
      tcInterfaceDataTypes = filter (visibleTypeIdentity . dtiNameAndOrigin) (tcInterfaceDataTypes interface),
      tcInterfaceClasses = filter visibleClass (tcInterfaceClasses interface)
    }
  where
    name = fromMaybe "Main" (moduleName (sourceModuleAst source))
    scope = Map.findWithDefault (error "missing resolve scope") (ModuleKey package name) exports
    termIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTerms scope)))
    typeIdentities = Set.fromList (mapMaybe resolvedIdentity (Map.elems (scopeTypes scope)))
    localIdentity identifier = (packageId package, name, identifier)
    visibleTerm (TcTermGlobal packageId' moduleName' identifier, _) =
      let identity = (packageId', moduleName', identifier)
       in Map.member identifier (scopeTerms scope) || identity `Set.member` termIdentities || identity == localIdentity identifier
    visibleTerm (TcTermLocal {}, _) = False
    visibleTyCon info =
      let tyCon = tciTyCon info
          identity = (tyConPackageId tyCon, tyConModuleName tyCon, tciName info)
       in Map.member (tciName info) (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity (tciName info)
    dtiNameAndOrigin info =
      let tyCon = dtiTyCon info
       in (tyConPackageId tyCon, tyConModuleName tyCon, dtiName info)
    visibleTypeIdentity identity = Map.member (third identity) (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity (third identity)
    visibleClass info =
      case ciOrigin info of
        Just (packageIdText, moduleName') ->
          let identity = (PackageId packageIdText, moduleName', ciName info)
           in Map.member (ciName info) (scopeTypes scope) || identity `Set.member` typeIdentities || identity == localIdentity (ciName info)
        Nothing -> False
    third (_, _, value) = value
    resolvedIdentity resolved = case resolved of
      ResolvedTopLevel packageId' resolvedName -> Just (packageId', fromMaybe name (nameQualifier resolvedName), nameText resolvedName)
      _ -> Nothing

writeTypeArtifact :: (String -> IO ()) -> [(Text, Text)] -> (SourceModule -> FilePath) -> SourceModule -> TcInterface -> IO ()
writeTypeArtifact verbose hashes artifactPath source interface = do
  let path = artifactPath source
      name = fromMaybe "Main" (moduleName (sourceModuleAst source))
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (encodeTypeArtifact (TypeArtifact name hashes interface))
  verbose ("Write type interface: " <> T.unpack name)

tryReadTypeArtifacts :: [(Text, Text)] -> (SourceModule -> FilePath) -> [SourceModule] -> IO (Maybe [TcInterface])
tryReadTypeArtifacts expected artifactPath unit = do
  result <- try (readTypeArtifacts expected artifactPath unit) :: IO (Either IOException [TcInterface])
  pure (either (const Nothing) Just result)

readTypeArtifacts :: [(Text, Text)] -> (SourceModule -> FilePath) -> [SourceModule] -> IO [TcInterface]
readTypeArtifacts expected artifactPath = mapM readOne
  where
    readOne source = do
      let path = artifactPath source
      bytes <- BS.readFile path
      artifact <- either (\message -> ioError (userError ("Invalid type artifact " <> path <> ": " <> message))) pure (decodeTypeArtifact bytes)
      unless (typeArtifactInputHashes artifact == expected) (ioError (userError ("Invalid type artifact " <> path <> ": input hashes do not match")))
      pure (typeArtifactInterface artifact)

updateTypeHashes :: (SourceModule -> FilePath) -> Map.Map Text Text -> [SourceModule] -> IO (Map.Map Text Text)
updateTypeHashes artifactPath previous unit = do
  hashes <- mapM artifactHash unit
  pure (foldl' (\result (name, digest) -> Map.insert name digest result) previous hashes)
  where
    artifactHash source = do
      bytes <- BS.readFile (artifactPath source)
      artifact <- either (\message -> ioError (userError ("Invalid type artifact while hashing interface: " <> message))) pure (decodeTypeArtifact bytes)
      pure (typeArtifactModuleName artifact, T.pack (stableHash [BL.toStrict (encodeTypeInterface (typeArtifactInterface artifact))]))

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
