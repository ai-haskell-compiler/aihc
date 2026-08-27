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
import Aihc.Fc2 (DesugarConfig (..), Fc2DesugarResult (..), desugarModuleFc2)
import Aihc.Fc2 qualified as Fc2
import Aihc.Fc2.TypeOf qualified as Fc2Type
import Aihc.Grin qualified as Grin
import Aihc.Llvm qualified as Llvm
import Aihc.Native (NativeTarget (..), backendCompiler, nativeTargetStoreDirectory)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension,
    ImportDecl (..),
    LanguageEdition (Haskell98Edition),
    Module,
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
import Control.Exception (bracket)
import Control.Monad (filterM, foldM, forM, unless, zipWithM)
import Data.ByteString qualified as BS
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, nub, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
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
    sourceAst :: !Module
  }

data CompileState = CompileState
  { compileExports :: !ModuleExports,
    compileTypes :: !TcInterface,
    compilePrograms :: !(Map.Map Text Fc2.Program),
    compileObjects :: ![FilePath]
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
  mapM_ requirePackageArchive selected
  (dependencyExports, dependencyTypes) <- loadPackageInterfaces selected
  sources <- discoverSources sourceDirectories dependencyExports (buildExeSourceFile options)
  runtime <- ensureRuntime storeRoot target (buildExeGarbageCollector options)
  entry <- ensureEntry storeRoot target
  withTemporaryDirectory "aihc-build-exe" $ \directory -> do
    objects <- compileSources target targetStoreRoot directory dependencyExports dependencyTypes sources
    createDirectoryIfMissing True (takeDirectory output)
    linkExecutable target output objects (map packageArchive selected) entry runtime

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
        matches' -> pure (last matches')
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

loadPackageInterfaces :: [InstalledPackage] -> IO (ModuleExports, TcInterface)
loadPackageInterfaces packages = do
  loaded <- mapM loadOnePackageInterfaces packages
  pure (Map.unions (map fst loaded), mconcat (map snd loaded))
  where
    loadOnePackageInterfaces package = do
      resolvePaths <- listNamedFiles (installedRoot package) "resolve.cbor"
      typePaths <- listNamedFiles (installedRoot package) "type.cbor"
      scopes <- forM resolvePaths $ \path -> do
        bytes <- BS.readFile path
        artifact <- either (ioError . userError . (("Invalid resolve artifact " <> path <> ": ") <>)) pure (decodeResolveArtifact bytes)
        let manifest = installedManifest package
            key = ModuleKey (Package (packageManifestName manifest) (PackageId (packageManifestIdentity manifest))) (resolveArtifactModuleName artifact)
        pure (key, resolveArtifactScope artifact)
      interfaces <- forM typePaths $ \path -> do
        bytes <- BS.readFile path
        artifact <- either (ioError . userError . (("Invalid type artifact " <> path <> ": ") <>)) pure (decodeTypeArtifact bytes)
        pure (typeArtifactInterface artifact)
      pure (Map.fromList scopes, mconcat interfaces)

discoverSources :: [FilePath] -> ModuleExports -> FilePath -> IO [SourceModule]
discoverSources sourceDirectories dependencyExports mainPath = do
  mainSource <- parseSource mainPath
  unless (sourceName mainSource == "Main") (ioError (userError ("The input file does not define module Main: " <> mainPath)))
  discovered <- visit Map.empty mainSource
  entrySource <- parseSourceText "<aihc-entry>" entryText
  pure (Map.elems discovered <> [entrySource])
  where
    installedModules = Set.fromList (map moduleKeyName (Map.keys dependencyExports))
    visit found source = do
      let name = sourceName source
      case Map.lookup name found of
        Just previous
          | sourcePath previous == sourcePath source -> pure found
          | otherwise -> ioError (userError ("More than one source file defines module " <> T.unpack name))
        Nothing -> do
          let found' = Map.insert name source found
              imports = nub (map importDeclModule (Syntax.moduleImports (sourceAst source)))
          foldM visitImport found' imports
    visitImport found name
      | name `Set.member` installedModules = pure found
      | Map.member name found = pure found
      | otherwise = do
          path <- findSourceFile sourceDirectories name
          parseSource path >>= visit found
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
parseSourceText path source =
  case parseModule (parserConfig path source) source of
    ([], modu) -> pure (SourceModule path modu)
    (errors, _) -> ioError (userError ("Failed to parse " <> path <> ": " <> show errors))

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

sourceName :: SourceModule -> Text
sourceName = fromMaybe "Main" . moduleName . sourceAst

compileSources :: NativeTarget -> FilePath -> FilePath -> ModuleExports -> TcInterface -> [SourceModule] -> IO [FilePath]
compileSources target storeRoot buildRoot dependencyExports dependencyTypes sources = do
  final <- foldM compileUnit initial (moduleSccs sources)
  pure (compileObjects final)
  where
    executablePackage = Package "exe" (PackageId "exe")
    initial = CompileState dependencyExports dependencyTypes Map.empty []
    moduleSccs values = map flatten (stronglyConnComp (map node values))
      where
        localNames = Set.fromList (map sourceName values)
        node source =
          ( source,
            sourceName source,
            filter (`Set.member` localNames) (map importDeclModule (Syntax.moduleImports (sourceAst source)))
          )
        flatten (AcyclicSCC value) = [value]
        flatten (CyclicSCC members) = members
    compileUnit state unit = do
      let packageModules = modulesInPackage executablePackage (map sourceAst unit)
          resolved = resolveWithDeps (compileExports state) packageModules
      unless (null (resolveErrors resolved)) $
        ioError
          ( userError
              ( "Name resolution failed: "
                  <> show (resolveErrors resolved)
                  <> "\nAvailable library modules: "
                  <> show (map moduleKeyName (Map.keys (compileExports state)))
              )
          )
      let (checkedModules, completeInterface) =
            typecheckModuleSccWithInterface
              (tcConfig (primPackageIdentity state))
              (compileTypes state)
              (map snd (resolvedModules resolved))
      unless (all tcModuleSuccess checkedModules) (ioError (userError ("Type check failed: " <> show (concatMap tcModuleDiagnostics checkedModules))))
      let bindings = tcInterfaceBindings completeInterface <> concatMap tcModuleBindings checkedModules
          results = map (desugarModuleFc2 (DesugarConfig (primPackageIdentity state)) bindings completeInterface) checkedModules
      unless (all ds2Success results) (ioError (userError ("Core-v2 generation failed: " <> unlines (concatMap ds2Errors results))))
      let programs = map ds2Program results
          unitPrograms = Map.fromList (zip (map (fromMaybe "Main" . moduleName) checkedModules) programs)
          allPrograms = Map.union unitPrograms (compilePrograms state)
          installedLoader = Fc2.storeModuleLoader storeRoot
          loader package name
            | package == PackageId "exe" = pure (Map.lookup name allPrograms)
            | otherwise = installedLoader package name
      loaded <- Fc2.loadScopeClosure loader programs
      let standalonePrograms = [Fc2Type.programWithImports (filter (/= program) loaded) program | program <- programs]
          lintErrors = concatMap Fc2.lintProgram standalonePrograms
      unless (null lintErrors) (ioError (userError ("Core-v2 lint failed: " <> show lintErrors)))
      objects <- zipWithM writeObject checkedModules standalonePrograms
      let localExports = extractInterfaceWithDeps (compileExports state) resolved `Map.union` compileExports state
      pure
        CompileState
          { compileExports = localExports,
            compileTypes = completeInterface,
            compilePrograms = allPrograms,
            compileObjects = compileObjects state <> objects
          }
    primPackageIdentity state =
      fromMaybe (PackageId "aihc-prim") $
        case [ packageId package
             | ModuleKey package _ <- Map.keys (compileExports state),
               packageName package == "aihc-prim"
             ] of
          identity : _ -> Just identity
          [] -> Nothing
    writeObject modu program = do
      grin <- either (ioError . userError . ("GRIN generation failed: " <>)) pure (Grin.lowerProgram program)
      cps <- either (ioError . userError . ("CPS-GRIN generation failed: " <>) . show) pure (Grin.toCpsGrin grin)
      let gcProgram = Grin.lowerGc cps
          name = fromMaybe "Main" (moduleName modu)
          object = buildRoot </> T.unpack (T.replace "." "-" name) <> ".o"
          source = object <> if target == Llvm then ".ll" else ".s"
      assembly <- compileBackend target gcProgram
      TIO.writeFile source assembly
      (compiler, arguments) <- backendCompiler target
      runTool compiler (arguments <> ["-c", source, "-o", object])
      pure object

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

listNamedFiles :: FilePath -> FilePath -> IO [FilePath]
listNamedFiles root name = do
  entries <- listDirectory root
  concat <$> mapM visit entries
  where
    visit entry = do
      let path = root </> entry
      directory <- doesDirectoryExist path
      if directory
        then listNamedFiles path name
        else pure [path | entry == name]

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
