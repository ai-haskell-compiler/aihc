{-# LANGUAGE OverloadedStrings #-}

module Aihc.Cli.BuildExe
  ( runBuildExe,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.Options (BuildExeOptions (..), GarbageCollector)
import Aihc.Cli.PackageManifest (PackageManifest (..), packageManifestPath, readPackageManifest)
import Aihc.Cli.ResolveArtifact (ResolveArtifact (..), decodeResolveArtifact)
import Aihc.Cli.Runtime (prepareEntryArchive, prepareRuntimeArchive, readWasmClangProcessWithExitCode, runtimeGarbageCollector)
import Aihc.Cli.Store (defaultStoreRoot, installedEntryArchivePath, installedRuntimeArchivePath)
import Aihc.Cli.TypeArtifact (TypeArtifact (..), decodeTypeArtifact)
import Aihc.Fc (DesugarConfig (..), FcDesugarResult (..), desugarModuleFc)
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Llvm qualified as Llvm
import Aihc.Native (NativeTarget (..), backendCompiler, nativeTargetStoreDirectory)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (ImplicitPrelude),
    ImportDecl (..),
    LanguageEdition (Haskell98Edition),
    Module,
    SourceSpan,
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    moduleName,
  )
import Aihc.Parser.Syntax qualified as Syntax
import Aihc.Parser.Token (readModuleHeaderPragmas)
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
import Aihc.Tc
  ( TcInterface,
    tcConfig,
    tcInterfaceBindings,
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
  )
import Aihc.Wasm qualified as Wasm
import Control.DeepSeq (force)
import Control.Exception (bracket, evaluate)
import Control.Monad (filterM, foldM, forM, unless, when, zipWithM)
import Data.ByteString qualified as BS
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Distribution.Package (unPackageName)
import Distribution.Parsec (simpleParsec)
import Distribution.Types.Dependency (Dependency (..))
import Distribution.Version (Version, VersionRange, withinRange)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getTemporaryDirectory,
    listDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, takeDirectory, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

data InstalledPackage = InstalledPackage
  { installedManifest :: !PackageManifest,
    installedRoot :: !FilePath,
    installedVersion :: !Version
  }

data PackageConstraint = PackageConstraint
  { constraintName :: !Text,
    constraintRange :: !VersionRange
  }

data SourceModule = SourceModule
  { sourcePath :: !FilePath,
    sourceModuleName :: !Text,
    sourceDependencies :: ![SourceDependency],
    sourceParseResult :: ([(SourceSpan, Text)], Module)
  }

data SourceDependency = SourceDependency
  { sourceDependencyPackage :: !(Maybe Text),
    sourceDependencyModule :: !Text
  }
  deriving (Eq, Ord, Show)

data InstalledModule = InstalledModule
  { installedModulePackage :: !InstalledPackage,
    installedModuleName :: !Text
  }

type InstalledModuleIndex = Map.Map Text [InstalledModule]

data CompileState = CompileState
  { compileExports :: !ModuleExports,
    compileTypes :: !TcInterface,
    compileObjects :: ![FilePath],
    compileLoadedModules :: !(Set.Set (PackageId, Text))
  }

runBuildExe :: BuildExeOptions -> IO ()
runBuildExe options = do
  storeRoot <- maybe defaultStoreRoot pure (buildExeStoreRoot options)
  let target = buildExeTarget options
      targetStoreRoot = storeRoot </> nativeTargetStoreDirectory target
      sourceDirectories = case buildExeSourceDirectories options of [] -> ["."]; values -> values
      output = fromMaybe (dropExtension (buildExeSourceFile options)) (buildExeOutputFile options)
  available <- readInstalledPackages targetStoreRoot
  constraints <- mapM parsePackageConstraint (buildExePackageConstraints options)
  selected <- resolvePackages available (constraints <> map implicitConstraint ["aihc-base", "aihc-prim"])
  primIdentity <- requireInstalledPackageIdentity "aihc-prim" selected
  mapM_ requirePackageArchive selected
  let moduleIndex = buildInstalledModuleIndex selected
  sources <- discoverSources sourceDirectories moduleIndex (buildExeSourceFile options)
  runtime <- ensureRuntime storeRoot target (buildExeGarbageCollector options)
  entry <- ensureEntry storeRoot target
  withTemporaryDirectory "aihc-build-exe" $ \directory -> do
    objects <- compileSources target directory moduleIndex primIdentity (buildExeLint options) sources
    createDirectoryIfMissing True (takeDirectory output)
    linkExecutable target output objects (map packageArchive selected) entry runtime

requireInstalledPackageIdentity :: Text -> [InstalledPackage] -> IO PackageId
requireInstalledPackageIdentity wanted packages =
  case [ PackageId (packageManifestIdentity (installedManifest package))
       | package <- packages,
         packageManifestName (installedManifest package) == wanted
       ] of
    [identity] -> pure identity
    [] -> ioError (userError ("The dependency plan does not include " <> T.unpack wanted))
    _ -> ioError (userError ("The dependency plan selects more than one build of " <> T.unpack wanted))

implicitConstraint :: Text -> PackageConstraint
implicitConstraint name =
  case simpleParsec (T.unpack name) of
    Just (Dependency _ versionRange _) -> PackageConstraint name versionRange
    Nothing -> error "invalid implicit package constraint"

parsePackageConstraint :: String -> IO PackageConstraint
parsePackageConstraint input =
  case simpleParsec input of
    Just (Dependency name versionRange _) -> pure (PackageConstraint (T.pack (unPackageName name)) versionRange)
    Nothing -> ioError (userError ("Invalid package constraint: " <> input))

readInstalledPackages :: FilePath -> IO [InstalledPackage]
readInstalledPackages targetRoot = do
  exists <- doesDirectoryExist targetRoot
  unless exists (ioError (userError ("No libraries are compiled for the target in " <> targetRoot)))
  entries <- listDirectory targetRoot
  fmap concat . forM entries $ \entry -> do
    let root = targetRoot </> entry
        path = packageManifestPath root
    existsManifest <- doesFileExist path
    if not existsManifest
      then pure []
      else do
        decoded <- readPackageManifest path
        manifest <- either (ioError . userError . (("Invalid package manifest " <> path <> ": ") <>)) pure decoded
        version <-
          maybe
            (ioError (userError ("Invalid installed package version: " <> T.unpack (packageManifestVersion manifest))))
            pure
            (simpleParsec (T.unpack (packageManifestVersion manifest)))
        pure [InstalledPackage manifest root version]

resolvePackages :: [InstalledPackage] -> [PackageConstraint] -> IO [InstalledPackage]
resolvePackages available constraints = do
  roots <- mapM select grouped
  closure <- foldM addPackage [] roots
  validateSelectedPackageNames closure
  validateSelectedConstraints closure grouped
  pure closure
  where
    grouped =
      [ (name, Map.findWithDefault [] name rangesByName)
      | name <- nub (map constraintName constraints)
      ]
    rangesByName =
      Map.fromListWith
        (<>)
        [(constraintName constraint, [constraintRange constraint]) | constraint <- constraints]
    select (name, ranges) =
      case sortOn installedVersion (filter (matches name ranges) available) of
        [] -> ioError (userError ("No compiled library fulfills the constraint for " <> T.unpack name))
        matches' ->
          case filter ((== installedVersion (last matches')) . installedVersion) matches' of
            [package] -> pure package
            _ -> ioError (userError ("More than one compiled build fulfills the constraint for " <> T.unpack name))
    matches name ranges package =
      packageManifestName (installedManifest package) == name
        && all (installedVersion package `withinRange`) ranges
    addPackage selected package
      | identity package `elem` map identity selected = pure selected
      | otherwise = do
          dependencies <- mapM requireIdentity (packageManifestDependencies (installedManifest package))
          foldM addPackage (selected <> [package]) dependencies
    requireIdentity wanted =
      maybe
        (ioError (userError ("A required compiled library is absent: " <> T.unpack wanted)))
        pure
        (find ((== wanted) . identity) available)
    identity = packageManifestIdentity . installedManifest
    validateSelectedPackageNames selected =
      mapM_ validateName (Map.toList packagesByName)
      where
        packagesByName =
          Map.fromListWith
            (<>)
            [ (packageManifestName (installedManifest package), [package])
            | package <- selected
            ]
        validateName (_, [_]) = pure ()
        validateName (name, _) = ioError (userError ("The dependency plan selects more than one build of " <> T.unpack name))
    validateSelectedConstraints selected =
      mapM_ $ \(name, ranges) ->
        case filter ((== name) . packageManifestName . installedManifest) selected of
          [package]
            | all (installedVersion package `withinRange`) ranges -> pure ()
            | otherwise -> conflict name
          [] -> conflict name
          _ -> ioError (userError ("The dependency plan selects more than one version of " <> T.unpack name))
    conflict name = ioError (userError ("The installed dependency plan does not fulfill the constraint for " <> T.unpack name))

packageArchive :: InstalledPackage -> FilePath
packageArchive package =
  installedRoot package
    </> "lib"
    </> "lib"
      <> T.unpack (packageManifestName (installedManifest package))
      <> ".a"

requirePackageArchive :: InstalledPackage -> IO ()
requirePackageArchive package = do
  let archive = packageArchive package
  exists <- doesFileExist archive
  unless exists $
    ioError
      ( userError
          ( "The library "
              <> T.unpack (packageManifestName (installedManifest package))
              <> " is not compiled for the target: "
              <> archive
          )
      )

buildInstalledModuleIndex :: [InstalledPackage] -> InstalledModuleIndex
buildInstalledModuleIndex packages =
  Map.fromListWith (<>) [(installedModuleName entry, [entry]) | entry <- entries]
  where
    entries =
      [ InstalledModule package name
      | package <- packages,
        name <- packageManifestModules (installedManifest package)
      ]

discoverSources :: [FilePath] -> InstalledModuleIndex -> FilePath -> IO [SourceModule]
discoverSources sourceDirectories moduleIndex mainPath = do
  mainSource <- parseSource mainPath
  unless (sourceModuleName mainSource == "Main") (ioError (userError ("The input file does not define module Main: " <> mainPath)))
  discovered <- visit Map.empty mainSource
  when (Map.member "Aihc.Entry" discovered) (ioError (userError "Source module conflicts with generated module Aihc.Entry"))
  entrySource <- parseSourceText "<aihc-entry>" entryText
  pure (Map.elems discovered <> [entrySource])
  where
    visit found source = do
      let name = sourceModuleName source
      case Map.lookup name found of
        Just previous
          | sourcePath previous == sourcePath source -> pure found
          | otherwise -> ioError (userError ("More than one source file defines module " <> T.unpack name))
        Nothing -> do
          let found' = Map.insert name source found
          foldM visitImport found' (sourceDependencies source)
    visitImport found dependency
      | not (isLocalSourceDependency dependency), Map.member name moduleIndex = pure found
      | isNothing (sourceDependencyPackage dependency), Map.member name moduleIndex = pure found
      | Map.member name found = pure found
      | not (isLocalSourceDependency dependency) = pure found
      | otherwise = do
          path <- findSourceFile sourceDirectories name
          parseSource path >>= visit found
      where
        name = sourceDependencyModule dependency
    entryText =
      T.unlines
        [ "{-# LANGUAGE NoImplicitPrelude #-}",
          "module Aihc.Entry where",
          "import qualified Main",
          "import GHC.TopHandler (runMainIO)",
          "entry = runMainIO Main.main"
        ]

findSourceFile :: [FilePath] -> Text -> IO FilePath
findSourceFile directories name = do
  let relative = foldl (</>) "" (map T.unpack (T.splitOn "." name)) <> ".hs"
      candidates = map (</> relative) directories
  matches <- filterM doesFileExist candidates
  case matches of
    [path] -> pure path
    [] -> ioError (userError ("Source module not found: " <> T.unpack name))
    _ -> ioError (userError ("More than one source file provides module " <> T.unpack name))

parseSource :: FilePath -> IO SourceModule
parseSource path = TIO.readFile path >>= parseSourceText path

parseSourceText :: FilePath -> Text -> IO SourceModule
parseSourceText path source = do
  let extensions = sourceExtensions source
      parsed = parseModule (parserConfig path source) source
      modu = snd parsed
      name = fromMaybe "Main" (moduleName modu)
      dependencies =
        nub
          ( map importDependency (Syntax.moduleImports modu)
              <> implicitSourceDependencies "exe" extensions
          )
  pure
    SourceModule
      { sourcePath = path,
        sourceModuleName = name,
        sourceDependencies = dependencies,
        sourceParseResult = parsed
      }

importDependency :: ImportDecl -> SourceDependency
importDependency importDecl =
  SourceDependency
    { sourceDependencyPackage = importDeclPackage importDecl,
      sourceDependencyModule = importDeclModule importDecl
    }

implicitSourceDependencies :: Text -> [Extension] -> [SourceDependency]
implicitSourceDependencies currentPackage extensions =
  compilerDependencies
    <> [ SourceDependency (Just "aihc-base") "Prelude"
       | currentPackage /= "aihc-base",
         ImplicitPrelude `elem` extensions
       ]

compilerDependencies :: [SourceDependency]
compilerDependencies =
  [ SourceDependency (Just "aihc-prim") "GHC.Types",
    SourceDependency (Just "aihc-base") "GHC.Base",
    SourceDependency (Just "aihc-prim") "GHC.Classes",
    SourceDependency (Just "aihc-base") "GHC.Num"
  ]

isLocalSourceDependency :: SourceDependency -> Bool
isLocalSourceDependency dependency =
  isNothing (sourceDependencyPackage dependency)
    || sourceDependencyPackage dependency == Just "this"

parserConfig :: FilePath -> Text -> ParserConfig
parserConfig path source =
  defaultConfig
    { parserSourceName = path,
      parserExtensions = sourceExtensions source
    }

sourceExtensions :: Text -> [Extension]
sourceExtensions source = effectiveExtensions language (headerExtensionSettings header)
  where
    header = readModuleHeaderPragmas source
    language = fromMaybe Haskell98Edition (headerLanguageEdition header)

forceSourceAst :: SourceModule -> IO Module
forceSourceAst source = do
  (errors, modu) <- evaluate (force (sourceParseResult source))
  unless (null errors) (ioError (userError ("Failed to parse " <> sourcePath source <> ": " <> show errors)))
  let parsedName = fromMaybe "Main" (moduleName modu)
  unless (parsedName == sourceModuleName source) $
    ioError (userError ("The parsed module name changed for " <> sourcePath source))
  pure modu

compileSources :: NativeTarget -> FilePath -> InstalledModuleIndex -> PackageId -> Bool -> [SourceModule] -> IO [FilePath]
compileSources target buildRoot moduleIndex primIdentity lint sources = do
  final <- foldM compileUnit initial (moduleSccs sources)
  pure (compileObjects final)
  where
    executablePackage = Package "exe" (PackageId "exe")
    localNames = Set.fromList (map sourceModuleName sources)
    initial = CompileState Map.empty mempty [] Set.empty
    moduleSccs values = map flatten (stronglyConnComp (map node values))
      where
        node source =
          ( source,
            sourceModuleName source,
            [ sourceDependencyModule dependency
            | dependency <- sourceDependencies source,
              isLocalSourceDependency dependency,
              sourceDependencyModule dependency `Set.member` localNames
            ]
          )
        flatten (AcyclicSCC value) = [value]
        flatten (CyclicSCC members) = members
    compileUnit state unit = do
      modules <- mapM forceSourceAst unit
      stateWithDependencies <- loadUnitDependencies state unit
      let packageModules = modulesInPackage executablePackage modules
          resolved = resolveWithDeps (compileExports stateWithDependencies) packageModules
      unless (null (resolveErrors resolved)) $
        ioError
          ( userError
              ( "Name resolution failed: "
                  <> show (resolveErrors resolved)
                  <> "\nAvailable library modules: "
                  <> show (map moduleKeyName (Map.keys (compileExports stateWithDependencies)))
              )
          )
      let (checkedModules, completeInterface) =
            typecheckModuleSccWithInterface
              (tcConfig primIdentity)
              (compileTypes stateWithDependencies)
              (map snd (resolvedModules resolved))
      unless (all tcModuleSuccess checkedModules) (ioError (userError ("Type check failed: " <> show (concatMap tcModuleDiagnostics checkedModules))))
      let bindings = tcInterfaceBindings completeInterface <> concatMap tcModuleBindings checkedModules
          results = map (desugarModuleFc (DesugarConfig primIdentity) bindings completeInterface) checkedModules
      unless (all dsSuccess results) (ioError (userError ("Core generation failed: " <> unlines (concatMap dsErrors results))))
      let programs = map dsProgram results
      when lint $ do
        let lintErrors = concatMap Fc.lintProgram programs
        unless (null lintErrors) (ioError (userError ("Core lint failed: " <> show lintErrors)))
      objects <- zipWithM writeObject checkedModules programs
      let localExports = extractInterfaceWithDeps (compileExports stateWithDependencies) resolved `Map.union` compileExports stateWithDependencies
      pure
        CompileState
          { compileExports = localExports,
            compileTypes = completeInterface,
            compileObjects = compileObjects stateWithDependencies <> objects,
            compileLoadedModules = compileLoadedModules stateWithDependencies
          }
    loadUnitDependencies state unit = do
      installedModules <- mapM requireInstalledModule (installedDependencies unit)
      foldM loadInstalledModule state installedModules
    installedDependencies unit =
      nub
        [ dependency
        | source <- unit,
          dependency <- sourceDependencies source,
          not (isLocalSourceDependency dependency)
            || sourceDependencyModule dependency `Set.notMember` localNames
        ]
    requireInstalledModule dependency =
      case selectedModules of
        [] ->
          ioError
            ( userError
                ( "Required installed module not found: "
                    <> maybe "" ((<> ":") . T.unpack) requestedPackage
                    <> T.unpack requestedName
                )
            )
        [installedModule] -> pure installedModule
        _ -> ioError (userError ("Ambiguous installed module: " <> T.unpack requestedName))
      where
        requestedName = sourceDependencyModule dependency
        requestedPackage = sourceDependencyPackage dependency
        candidates = Map.findWithDefault [] requestedName moduleIndex
        selectedModules =
          case requestedPackage of
            Nothing -> candidates
            Just packageName' ->
              filter
                ((== packageName') . packageManifestName . installedManifest . installedModulePackage)
                candidates
    loadInstalledModule state installedModule
      | loadedKey `Set.member` compileLoadedModules state = pure state
      | otherwise = do
          resolveBytes <- BS.readFile resolvePath
          resolveArtifact <-
            either
              (ioError . userError . (("Invalid resolve artifact " <> resolvePath <> ": ") <>))
              pure
              (decodeResolveArtifact resolveBytes)
          unless (resolveArtifactModuleName resolveArtifact == installedModuleName installedModule) $
            ioError (userError ("Resolve artifact module name does not match " <> resolvePath))
          typeBytes <- BS.readFile typePath
          typeArtifact <-
            either
              (ioError . userError . (("Invalid type artifact " <> typePath <> ": ") <>))
              pure
              (decodeTypeArtifact typeBytes)
          unless (typeArtifactModuleName typeArtifact == installedModuleName installedModule) $
            ioError (userError ("Type artifact module name does not match " <> typePath))
          pure
            state
              { compileExports = Map.insert moduleKey (resolveArtifactScope resolveArtifact) (compileExports state),
                compileTypes = compileTypes state <> typeArtifactInterface typeArtifact,
                compileLoadedModules = Set.insert loadedKey (compileLoadedModules state)
              }
      where
        installedPackage = installedModulePackage installedModule
        manifest = installedManifest installedPackage
        packageIdentity = PackageId (packageManifestIdentity manifest)
        loadedKey = (packageIdentity, installedModuleName installedModule)
        moduleKey = ModuleKey (Package (packageManifestName manifest) packageIdentity) (installedModuleName installedModule)
        moduleRoot = installedRoot installedPackage </> moduleDirectoryText (installedModuleName installedModule)
        resolvePath = moduleRoot </> "resolve.cbor"
        typePath = moduleRoot </> "type.cbor"
    writeObject modu program = do
      grin <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram program)
      when lint $ do
        let grinErrors = Grin.lintProgram grin
        unless (null grinErrors) (ioError (userError ("GRIN lint failed: " <> show grinErrors)))
      cps <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin grin)
      let gcProgram = Grin.lowerGc cps
          name = fromMaybe "Main" (moduleName modu)
          object = buildRoot </> T.unpack (T.replace "." "-" name) <> ".o"
          source = object <> if target == Llvm then ".ll" else ".s"
      when lint $ do
        let gcErrors = Grin.lintProgram (Grin.gcGrinProgram gcProgram)
        unless (null gcErrors) (ioError (userError ("GC-GRIN lint failed: " <> show gcErrors)))
      assembly <- compileBackend target gcProgram
      TIO.writeFile source assembly
      (compiler, arguments) <- backendCompiler target
      runTool compiler (arguments <> ["-c", source, "-o", object])
      pure object

moduleDirectoryText :: Text -> FilePath
moduleDirectoryText = foldl (</>) "" . map T.unpack . T.splitOn "."

compileBackend :: NativeTarget -> Grin.GcGrinProgram -> IO Text
compileBackend target program =
  either (ioError . userError . show) pure $
    case target of
      AppleArm64 -> firstBackend (Arm64.compileModule program)
      LinuxAmd64 -> firstBackend (Amd64.compileModule program)
      Llvm -> firstBackend (Llvm.compileModule program)
      Wasm32Wasip3 -> firstBackend (Wasm.compileModule program)
  where
    firstBackend :: (Show error) => Either error Text -> Either String Text
    firstBackend = either (Left . show) Right

ensureEntry :: FilePath -> NativeTarget -> IO FilePath
ensureEntry storeRoot target = do
  let entry = installedEntryArchivePath storeRoot target
  exists <- doesFileExist entry
  if exists then pure entry else prepareEntryArchive storeRoot target

ensureRuntime :: FilePath -> NativeTarget -> GarbageCollector -> IO FilePath
ensureRuntime storeRoot target garbageCollector = do
  let runtime = installedRuntimeArchivePath storeRoot target (runtimeGarbageCollector garbageCollector)
  exists <- doesFileExist runtime
  if exists then pure runtime else prepareRuntimeArchive storeRoot target garbageCollector

linkExecutable :: NativeTarget -> FilePath -> [FilePath] -> [FilePath] -> FilePath -> FilePath -> IO ()
linkExecutable Wasm32Wasip3 output objects archives entry runtime =
  withTemporaryDirectory "aihc-wasm-link" $ \directory -> do
    let coreModule = directory </> "program.wasm"
    runTool
      "wasm-ld"
      ( ["--no-entry", "--export-memory", "--allow-undefined"]
          <> objects
          <> archives
          <> ["--whole-archive", entry, runtime, "--no-whole-archive", "-o", coreModule]
      )
    runTool "wasm-tools" ["component", "new", coreModule, "-o", output]
    runTool "wasm-tools" ["validate", output]
linkExecutable target output objects archives entry runtime = do
  (compiler, arguments) <- backendCompiler target
  runTool compiler (arguments <> objects <> archives <> [entry, runtime, "-o", output])

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  result <-
    if tool == "clang" && "--target=wasm32-unknown-unknown" `elem` arguments
      then readWasmClangProcessWithExitCode tool arguments
      else readProcessWithExitCode tool arguments ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (exitCode, stdout, stderr) -> ioError (userError (tool <> " failed (" <> show exitCode <> "): " <> if null stderr then stdout else stderr))

withTemporaryDirectory :: String -> (FilePath -> IO value) -> IO value
withTemporaryDirectory template = bracket acquire removeDirectoryRecursive
  where
    acquire = do
      temporary <- getTemporaryDirectory
      (path, handle) <- openTempFile temporary template
      hClose handle
      removeFile path
      createDirectory path
      pure path
