{-# LANGUAGE OverloadedStrings #-}

-- | Compile a standalone Haskell module through System FC and GRIN to an
-- executable through an assembly or LLVM backend.
module Aihc.Cli.Compile
  ( CompileEnvironment (..),
    CompileError (..),
    compileOutputPath,
    compileSourceToCoreWithDependencies,
    compileSourceToCpsGrinWithDependencies,
    compileSourceToGrinWithDependencies,
    compileSourceToWholeCoreWithDependencies,
    compileSourceToAssembly,
    compileSourceToAssemblyFor,
    compileSourceToAssemblyWithDependencies,
    compileSourceToAssemblyWithDependenciesFor,
    defaultCompileEnvironment,
    reachableRuntimePrimitiveNames,
    renderCompileError,
    runCompile,
    runCompileWithEnvironment,
    wasmClangCommand,
    wasmOptArguments,
  )
where

import Aihc.Amd64 qualified as Amd64
import Aihc.Arm64 qualified as Arm64
import Aihc.Cli.Compile.Dependencies
  ( CompileEnvironment (..),
    DependencyArtifact (..),
    DependencyUnit (..),
    buildDependencies,
  )
import Aihc.Cli.Options (CompileOptions (..), GarbageCollector (..))
import Aihc.Cli.Runtime (readWasmClangProcessWithExitCode, runtimeGarbageCollector, wasmClangCommand, wasmOptArguments)
import Aihc.Cli.Store (defaultStoreRoot, installedRuntimeArchivePath)
import Aihc.Fc
  ( DesugarResult (..),
    FcAlt (..),
    FcBind (..),
    FcExpr (..),
    FcProgram (..),
    FcTopBind (..),
    Var (..),
    desugarModule,
    desugarModuleWithBindings,
    eliminateDeadCode,
    extractReachabilityInterface,
    reachablePrimitiveNames,
  )
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Llvm qualified as Llvm
import Aihc.Native
  ( LinkLayout,
    NativeTarget (..),
    backendCompiler,
    buildLinkLayoutFromInterfaces,
    extendLinkLayout,
    hostNativeTarget,
  )
import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parseExpr, parseModule)
import Aihc.Parser.Syntax
  ( Decl (..),
    Extension (ImplicitPrelude),
    LanguageEdition (Haskell98Edition),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NameType (NameVarId),
    Rhs (..),
    ValueDecl (..),
    effectiveExtensions,
    headerExtensionSettings,
    headerLanguageEdition,
    mkUnqualifiedName,
    moduleName,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ModuleKey (..), ResolveResult (..), resolveWithDeps, unnamedPackage)
import Aihc.Tc (Unique (..), tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModulesWithInterface)
import Aihc.Wasm qualified as Wasm
import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.IO.Utf8 qualified as Utf8
import System.Directory (createDirectory, doesFileExist, findExecutable, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

data CompileError
  = CompileParseError !String
  | CompileFrontendError ![String]
  | CompileDependencyError !String
  | CompileCpsGrinError !Grin.CpsGrinError
  | CompileBackendError !BackendError
  | CompileTargetError !String
  | CompileClangError !ExitCode !String
  | CompileToolError !FilePath !ExitCode !String
  deriving (Eq, Show)

data BackendError
  = BackendArm64Error !Arm64.Arm64Error
  | BackendAmd64Error !Amd64.Amd64Error
  | BackendLlvmError !Llvm.LlvmError
  | BackendWasmError !Wasm.WasmError
  deriving (Eq, Show)

data CompileArtifacts = CompileArtifacts
  { compiledCore :: !Text,
    compiledGrin :: !Text,
    compiledCpsGrin :: !Text,
    compiledGcGrin :: !Text,
    compiledAssembly :: !Text,
    compiledArchives :: ![FilePath]
  }

-- | The Core and GRIN produced for one independently compiled module SCC.
data IncrementalUnit = IncrementalUnit
  { incrementalUnitCore :: !FcProgram,
    incrementalUnitGrin :: !Grin.GrinProgram,
    incrementalUnitCpsGrin :: !Grin.CpsGrinProgram
  }

-- | Per-SCC compiler output. Whole-program compilation is derived from
-- this structure; it is never an alternative frontend or lowering path.
data IncrementalCompilation = IncrementalCompilation
  { incrementalDependencyUnits :: ![IncrementalUnit],
    incrementalMainUnit :: !IncrementalUnit,
    incrementalEntryBindingName :: !Text,
    incrementalEntryName :: !Text
  }

runCompile :: CompileOptions -> IO ()
runCompile options = do
  storeRoot <- maybe defaultStoreRoot pure (compileStoreRoot options)
  runCompileWithEnvironment (CompileEnvironment storeRoot) options

runCompileWithEnvironment :: CompileEnvironment -> CompileOptions -> IO ()
runCompileWithEnvironment environment options = do
  target <-
    case compileTarget options of
      Just explicitTarget -> pure explicitTarget
      Nothing ->
        maybe
          (ioError (userError (renderCompileError (CompileTargetError "unsupported host; pass an explicit --target"))))
          pure
          hostNativeTarget
  source <- Utf8.readFile (compileSourceFile options)
  artifactsResult <- compileSourceToArtifactsWithDependencies target (compileWholeProgram options) environment (compileSourceFile options) source
  artifacts <- either (ioError . userError . renderCompileError) pure artifactsResult
  let output = compileOutputPath options
  writeIntermediateArtifacts output options artifacts
  if compileKeepAsm options
    then do
      let assemblyPath = output <> backendSourceExtension target
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble (compileInstalledStoreRoot environment) target (compileGarbageCollector options) (compileUseWasmOpt options) output assemblyPath (compiledArchives artifacts)
    else withTemporaryDirectory "aihc-compile" $ \directory -> do
      let assemblyPath = directory </> "program" <> backendSourceExtension target
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble (compileInstalledStoreRoot environment) target (compileGarbageCollector options) (compileUseWasmOpt options) output assemblyPath (compiledArchives artifacts)

writeIntermediateArtifacts :: FilePath -> CompileOptions -> CompileArtifacts -> IO ()
writeIntermediateArtifacts output options artifacts = do
  when (compileKeepCore options) $
    TIO.writeFile (output <> ".core") (compiledCore artifacts)
  when (compileKeepGrin options) $ do
    TIO.writeFile (output <> ".grin") (compiledGrin artifacts)
    TIO.writeFile (output <> ".cps.grin") (compiledCpsGrin artifacts)
    TIO.writeFile (output <> ".gc.grin") (compiledGcGrin artifacts)

-- | The installed runtime and library store used by the command-line
-- compiler.
defaultCompileEnvironment :: IO CompileEnvironment
defaultCompileEnvironment = CompileEnvironment <$> defaultStoreRoot

compileOutputPath :: CompileOptions -> FilePath
compileOutputPath options =
  fromMaybe defaultOutput (compileOutputFile options)
  where
    source = compileSourceFile options
    withoutExtension = dropExtension source
    defaultOutput
      | withoutExtension == source = source <> ".out"
      | otherwise = withoutExtension

compileSourceToAssembly :: FilePath -> Text -> Either CompileError Text
compileSourceToAssembly = compileSourceToAssemblyFor defaultCompileTarget

compileSourceToAssemblyFor :: NativeTarget -> FilePath -> Text -> Either CompileError Text
compileSourceToAssemblyFor target sourceName source = do
  parsed <- parseCompileModule sourceName source
  let desugared = desugarModule parsed
  if dsSuccess desugared
    then compiledAssembly <$> compileProgramArtifacts target (dsProgram desugared)
    else Left (CompileFrontendError (dsErrors desugared))

compileSourceToAssemblyWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToAssemblyWithDependencies = compileSourceToAssemblyWithDependenciesFor defaultCompileTarget

compileSourceToAssemblyWithDependenciesFor :: NativeTarget -> CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToAssemblyWithDependenciesFor target environment sourceName source =
  fmap (fmap compiledAssembly) (compileSourceToArtifactsWithDependencies target False environment sourceName source)

-- | Compile source to the incremental System FC program rendered by
-- @--keep-core@. Dependency declarations participate in cross-unit lowering,
-- but their implementations remain in their separately compiled artifacts.
compileSourceToCoreWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToCoreWithDependencies environment sourceName source =
  fmap (fmap compiledCore) (compileSourceToArtifactsWithDependencies defaultCompileTarget False environment sourceName source)

-- | Compile source and its dependencies to the incremental GRIN program
-- rendered by @--keep-grin@.
compileSourceToGrinWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToGrinWithDependencies environment sourceName source =
  fmap (fmap compiledGrin) (compileSourceToArtifactsWithDependencies defaultCompileTarget False environment sourceName source)

-- | Compile source to the continuation-reified GRIN consumed by native
-- backends and rendered as @.cps.grin@ by @--keep-grin@.
compileSourceToCpsGrinWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToCpsGrinWithDependencies environment sourceName source =
  fmap (fmap compiledCpsGrin) (compileSourceToArtifactsWithDependencies defaultCompileTarget False environment sourceName source)

-- | Compile source incrementally, then merge the resulting Core units and run
-- whole-program dead-code elimination. This is the Core rendered by
-- @--whole-program --keep-core@.
compileSourceToWholeCoreWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToWholeCoreWithDependencies environment sourceName source =
  fmap (fmap compiledCore) (compileSourceToArtifactsWithDependencies defaultCompileTarget True environment sourceName source)

compileSourceToArtifactsWithDependencies :: NativeTarget -> Bool -> CompileEnvironment -> FilePath -> Text -> IO (Either CompileError CompileArtifacts)
compileSourceToArtifactsWithDependencies target wholeProgram environment sourceName source =
  case parseCompileModule sourceName source of
    Left err -> pure (Left err)
    Right parsed -> do
      dependencies <- buildDependencies target environment (ImplicitPrelude `elem` sourceExtensions source) (not wholeProgram) parsed
      pure $ do
        artifact <- either (Left . CompileDependencyError) Right dependencies
        (entryBindingName, executableModule) <-
          if any ((== "GHC.TopHandler") . moduleKeyName) (Map.keys (dependencyExports artifact))
            then do
              wrapped <- addMainHandler parsed
              Right (mainEntryBindingName, wrapped)
            else Right ("main", parsed)
        compileWithDependencies target wholeProgram entryBindingName artifact executableModule

compileWithDependencies :: NativeTarget -> Bool -> Text -> DependencyArtifact -> Module -> Either CompileError CompileArtifacts
compileWithDependencies target wholeProgram entryBindingName dependencies parsed =
  case resolveWithDeps (dependencyExports dependencies) [(unnamedPackage, parsed)] of
    ResolveResult {resolveErrors = errors@(_ : _)} -> Left (CompileFrontendError ["resolve error: " <> show errors])
    ResolveResult {resolvedModules} ->
      let moduleAsts = map snd resolvedModules
          (checkedModules, _) =
            typecheckModulesWithInterface
              (dependencyTcInterface dependencies)
              moduleAsts
       in if not (all tcModuleSuccess checkedModules)
            then Left (CompileFrontendError ["typecheck error: " <> show (concatMap tcModuleDiagnostics checkedModules)])
            else
              let bindings = dependencyBindings dependencies <> concatMap tcModuleBindings checkedModules
                  desugared = zipWith (desugarModuleWithBindings bindings) checkedModules moduleAsts
               in if not (all dsSuccess desugared)
                    then Left (CompileFrontendError (concatMap dsErrors desugared))
                    else
                      let mainProgram = FcProgram (concatMap (fcTopBinds . dsProgram) desugared)
                       in do
                            incremental <- compileIncrementally entryBindingName (fromMaybe "Main" (moduleName parsed)) dependencies mainProgram
                            if wholeProgram
                              then compileWholeProgramArtifacts target incremental
                              else compileIncrementalArtifacts target dependencies incremental

-- | Compile every module SCC to its own normalized Core and GRIN unit before any
-- optional whole-program transformation is considered.
compileIncrementally :: Text -> Text -> DependencyArtifact -> FcProgram -> Either CompileError IncrementalCompilation
compileIncrementally entryBindingName mainModuleName dependencies unoptimizedMain =
  do
    mainCpsGrin <- either (Left . CompileCpsGrinError) Right (Grin.toCpsGrin mainGrin)
    pure
      IncrementalCompilation
        { incrementalDependencyUnits =
            [ IncrementalUnit (dependencyUnitProgram unit) (dependencyUnitGrin unit) (dependencyUnitCpsGrin unit)
            | unit <- dependencyUnits dependencies
            ],
          incrementalMainUnit = IncrementalUnit mainCore mainGrin mainCpsGrin,
          incrementalEntryBindingName = entryBindingName,
          incrementalEntryName = T.intercalate "\0" (["exe"] <> T.splitOn "." mainModuleName <> [entryBindingName])
        }
  where
    mainCore =
      Fc.optimizeProgram
        (Fc.lowerPseudoOps (Fc.lowerNewtypesWithInterface (dependencyNewtypeInterface dependencies) (eliminateDeadCode entryBindingName unoptimizedMain)))
    mainLinkNames = Grin.linkNamesForProgram ["exe"] (T.splitOn "." mainModuleName) mainCore
    mainGrin = Grin.lowerProgramWithInterfaceAndLinkNames mainLinkNames (dependencyGrinInterface dependencies) mainCore

-- | Link already-incremental Core units for whole-program analysis. Unique
-- namespaces are separated only while constructing the merged view.
mergeIncrementalCore :: IncrementalCompilation -> FcProgram
mergeIncrementalCore compilation =
  appendPrograms freshDependencies mainCore
  where
    mainCore = incrementalUnitCore (incrementalMainUnit compilation)
    freshDependencies = freshenPrograms (1 + maximumProgramUnique mainCore) (map incrementalUnitCore (incrementalDependencyUnits compilation))

freshenPrograms :: Int -> [FcProgram] -> FcProgram
freshenPrograms _ [] = FcProgram []
freshenPrograms nextUnique (program : programs) =
  appendPrograms shifted (freshenPrograms (1 + maximumProgramUnique shifted) programs)
  where
    shifted = shiftProgramVars nextUnique program

reachableLinkedCore :: IncrementalCompilation -> FcProgram
reachableLinkedCore compilation =
  eliminateDeadCode (incrementalEntryBindingName compilation) (mergeIncrementalCore compilation)

-- | The optional whole-program phase consumes incremental Core; it does not
-- rerun the frontend or bypass per-SCC GRIN lowering.
compileWholeProgramArtifacts :: NativeTarget -> IncrementalCompilation -> Either CompileError CompileArtifacts
compileWholeProgramArtifacts target = compileProgramArtifacts target . reachableLinkedCore

compileIncrementalArtifacts :: NativeTarget -> DependencyArtifact -> IncrementalCompilation -> Either CompileError CompileArtifacts
compileIncrementalArtifacts target dependencies compilation = do
  let mainUnit = incrementalMainUnit compilation
      mainCore = incrementalUnitCore mainUnit
      mainGrin = incrementalUnitGrin mainUnit
      mainCpsGrin = incrementalUnitCpsGrin mainUnit
      mainGcGrin = Grin.lowerGc mainCpsGrin
      dependencyLayout = buildLinkLayoutFromInterfaces (dependencyLinkInterfaces dependencies)
      layout = extendLinkLayout dependencyLayout mainGrin
      reachability = dependencyReachabilityInterface dependencies <> extractReachabilityInterface mainCore
      declaredPrimitives =
        dependencyRuntimePrimitiveNames dependencies
          <> Set.fromList [Grin.grinVarName primitive | (primitive, _) <- Grin.grinPrimitives mainGrin]
      primitives = Set.toAscList (Set.intersection (reachablePrimitiveNames (incrementalEntryBindingName compilation) reachability) declaredPrimitives)
  either (Left . CompileBackendError) Right (validateBackendPrimitiveNames target primitives)
  assembly <-
    either
      (Left . CompileBackendError)
      Right
      (compileBackendProgramWithDependencies target layout (dependencyInitializerSymbols dependencies) (incrementalEntryName compilation) mainGcGrin)
  pure
    CompileArtifacts
      { compiledCore = renderCore mainCore,
        compiledGrin = renderGrin mainGrin,
        compiledCpsGrin = renderCpsGrin mainCpsGrin,
        compiledGcGrin = renderGcGrin mainGcGrin,
        compiledAssembly = assembly,
        compiledArchives = dependencyArchivePaths dependencies
      }

-- | Source reachability includes primops that typed lowering erases. Backend
-- validation must consider only declarations that survive into some linked
-- GRIN unit, otherwise an erased operation such as unsafeCoerce# is mistaken
-- for a backend responsibility during incremental compilation.
reachableRuntimePrimitiveNames :: Text -> Fc.ReachabilityInterface -> [Grin.GrinProgram] -> Set.Set Text
reachableRuntimePrimitiveNames entry reachability programs =
  Set.intersection
    (reachablePrimitiveNames entry reachability)
    ( Set.fromList
        [ Grin.grinVarName primitive
        | program <- programs,
          (primitive, _) <- Grin.grinPrimitives program
        ]
    )

compileProgramArtifacts :: NativeTarget -> FcProgram -> Either CompileError CompileArtifacts
compileProgramArtifacts target sourceCore = do
  let core = Fc.optimizeProgram (Fc.lowerPseudoOps (Fc.lowerNewtypes sourceCore))
  let grin = Grin.lowerProgram core
  cpsGrin <- either (Left . CompileCpsGrinError) Right (Grin.toCpsGrin grin)
  let gcGrin = Grin.lowerGc cpsGrin
  assembly <- either (Left . CompileBackendError) Right (compileBackendProgram target "main" gcGrin)
  pure
    CompileArtifacts
      { compiledCore = renderCore core,
        compiledGrin = renderGrin grin,
        compiledCpsGrin = renderCpsGrin cpsGrin,
        compiledGcGrin = renderGcGrin gcGrin,
        compiledAssembly = assembly,
        compiledArchives = []
      }

validateBackendPrimitiveNames :: NativeTarget -> [Text] -> Either BackendError ()
validateBackendPrimitiveNames target names =
  case target of
    AppleArm64 -> either (Left . BackendArm64Error) Right (Arm64.validatePrimitiveNames names)
    LinuxAmd64 -> either (Left . BackendAmd64Error) Right (Amd64.validatePrimitiveNames names)
    Llvm -> either (Left . BackendLlvmError) Right (Llvm.validatePrimitiveNames names)
    Wasm32Wasip3 -> either (Left . BackendWasmError) Right (Wasm.validatePrimitiveNames names)

compileBackendProgram :: NativeTarget -> Text -> Grin.GcGrinProgram -> Either BackendError Text
compileBackendProgram target entry program =
  case target of
    AppleArm64 -> either (Left . BackendArm64Error) Right (Arm64.compileProgram entry program)
    LinuxAmd64 -> either (Left . BackendAmd64Error) Right (Amd64.compileProgram entry program)
    Llvm -> either (Left . BackendLlvmError) Right (Llvm.compileProgram entry program)
    Wasm32Wasip3 -> either (Left . BackendWasmError) Right (Wasm.compileProgram entry program)

compileBackendProgramWithDependencies :: NativeTarget -> LinkLayout -> [Text] -> Text -> Grin.GcGrinProgram -> Either BackendError Text
compileBackendProgramWithDependencies target layout initializers entry program =
  case target of
    AppleArm64 -> either (Left . BackendArm64Error) Right (Arm64.compileProgramWithDependencies layout initializers entry program)
    LinuxAmd64 -> either (Left . BackendAmd64Error) Right (Amd64.compileProgramWithDependencies layout initializers entry program)
    Llvm -> either (Left . BackendLlvmError) Right (Llvm.compileProgramWithDependencies layout initializers entry program)
    Wasm32Wasip3 -> either (Left . BackendWasmError) Right (Wasm.compileProgramWithDependencies layout initializers entry program)

renderCore :: FcProgram -> Text
renderCore = withFinalNewline . Fc.renderProgram

renderGrin :: Grin.GrinProgram -> Text
renderGrin = withFinalNewline . Grin.renderProgram

renderCpsGrin :: Grin.CpsGrinProgram -> Text
renderCpsGrin = renderGrin . Grin.cpsGrinProgram

renderGcGrin :: Grin.GcGrinProgram -> Text
renderGcGrin = renderGrin . Grin.gcGrinProgram

withFinalNewline :: String -> Text
withFinalNewline rendered = T.pack rendered <> "\n"

appendPrograms :: FcProgram -> FcProgram -> FcProgram
appendPrograms (FcProgram left) (FcProgram right) = FcProgram (left <> right)

maximumProgramUnique :: FcProgram -> Int
maximumProgramUnique = maximum . (0 :) . map varUniqueInt . programVars

varUniqueInt :: Var -> Int
varUniqueInt var = case varUnique var of Unique unique -> unique

shiftProgramVars :: Int -> FcProgram -> FcProgram
shiftProgramVars offset (FcProgram topBinds) = FcProgram (map shiftTopBind topBinds)
  where
    shiftVar var = case varUnique var of Unique unique -> var {varUnique = Unique (unique + offset)}

    shiftTopBind topBind =
      case topBind of
        FcModule {} -> topBind
        FcExternal {} -> topBind
        FcData {} -> topBind
        FcAxiom {} -> topBind
        FcNewtype {} -> topBind
        FcPrimitive var arity -> FcPrimitive (shiftVar var) arity
        FcForeignImport {} -> topBind
        FcTopBind bind -> FcTopBind (shiftBind bind)

    shiftBind bind =
      case bind of
        FcNonRec var expression -> FcNonRec (shiftVar var) (shiftExpr expression)
        FcRec bindings -> FcRec [(shiftVar var, shiftExpr expression) | (var, expression) <- bindings]

    shiftExpr expression =
      case expression of
        FcVar var -> FcVar (shiftVar var)
        FcLit {} -> expression
        FcApp function argument -> FcApp (shiftExpr function) (shiftExpr argument)
        FcTyApp inner ty -> FcTyApp (shiftExpr inner) ty
        FcLam var body -> FcLam (shiftVar var) (shiftExpr body)
        FcTyLam tyVar body -> FcTyLam tyVar (shiftExpr body)
        FcLet bind body -> FcLet (shiftBind bind) (shiftExpr body)
        FcCase scrutinee binder alternatives ->
          FcCase (shiftExpr scrutinee) (shiftVar binder) (map shiftAlt alternatives)
        FcCast inner coercion -> FcCast (shiftExpr inner) coercion
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map shiftExpr arguments)

    shiftAlt alternative =
      alternative
        { altBinders = map shiftVar (altBinders alternative),
          altRhs = shiftExpr (altRhs alternative)
        }

programVars :: FcProgram -> [Var]
programVars (FcProgram topBinds) = concatMap topBindVarsDeep topBinds

topBindVarsDeep :: FcTopBind -> [Var]
topBindVarsDeep topBind =
  case topBind of
    FcModule {} -> []
    FcExternal {} -> []
    FcData {} -> []
    FcAxiom {} -> []
    FcNewtype {} -> []
    FcPrimitive var _ -> [var]
    FcForeignImport {} -> []
    FcTopBind bind -> bindVarsDeep bind

bindVarsDeep :: FcBind -> [Var]
bindVarsDeep bind =
  case bind of
    FcNonRec var expression -> var : exprVars expression
    FcRec bindings -> concat [var : exprVars expression | (var, expression) <- bindings]

exprVars :: FcExpr -> [Var]
exprVars expression =
  case expression of
    FcVar var -> [var]
    FcLit {} -> []
    FcApp function argument -> exprVars function <> exprVars argument
    FcTyApp inner _ -> exprVars inner
    FcLam var body -> var : exprVars body
    FcTyLam _ body -> exprVars body
    FcLet bind body -> bindVarsDeep bind <> exprVars body
    FcCase scrutinee binder alternatives -> exprVars scrutinee <> (binder : concatMap altVars alternatives)
    FcCast inner _ -> exprVars inner
    FcCallForeign _ arguments -> concatMap exprVars arguments
  where
    altVars alternative = altBinders alternative <> exprVars (altRhs alternative)

parseCompileModule :: FilePath -> Text -> Either CompileError Module
parseCompileModule sourceName source =
  case parseModule config source of
    ([], modu) -> Right modu
    (errors, _) -> Left (CompileParseError (show errors))
  where
    config =
      defaultConfig
        { parserSourceName = sourceName,
          parserExtensions = sourceExtensions source
        }

mainEntryBindingName :: Text
mainEntryBindingName = "$aihc.main"

-- | Add the executable-only wrapper that gives uncaught exceptions Haskell's
-- top-level semantics. Keeping the wrapper in the surface program lets normal
-- resolution, type checking, desugaring, and dependency reachability account
-- for every value it uses.
addMainHandler :: Module -> Either CompileError Module
addMainHandler modu = do
  topHandlerImport <-
    case parseModule defaultConfig "module AihcMainWrapper where\nimport GHC.TopHandler\n" of
      ([], wrapper) ->
        case moduleImports wrapper of
          [importDecl] -> Right importDecl
          _ -> Left internalWrapperParseError
      _ -> Left internalWrapperParseError
  wrapperExpression <-
    case parseExpr defaultConfig "GHC.TopHandler.runMainIO main" of
      ParseOk expression -> Right expression
      ParseErr _ -> Left internalWrapperParseError
  let wrapperDecl =
        DeclValue $
          FunctionBind
            (mkUnqualifiedName NameVarId mainEntryBindingName)
            [ Match
                { matchAnns = [],
                  matchHeadForm = MatchHeadPrefix,
                  matchPats = [],
                  matchRhs = UnguardedRhs [] wrapperExpression Nothing
                }
            ]
  pure
    modu
      { moduleImports = moduleImports modu <> [topHandlerImport],
        moduleDecls = moduleDecls modu <> [wrapperDecl]
      }
  where
    internalWrapperParseError = CompileParseError "compiler-generated main wrapper did not parse"

sourceExtensions :: Text -> [Extension]
sourceExtensions source = effectiveExtensions language (headerExtensionSettings header)
  where
    header = readModuleHeaderPragmas source
    language = fromMaybe Haskell98Edition (headerLanguageEdition header)

renderCompileError :: CompileError -> String
renderCompileError compileError =
  case compileError of
    CompileParseError err -> "parse error: " <> err
    CompileFrontendError errors -> "frontend error: " <> unwords errors
    CompileDependencyError err -> "dependency error: " <> err
    CompileCpsGrinError err -> "CPS-GRIN error: " <> show err
    CompileBackendError err -> "backend code generation error: " <> show err
    CompileTargetError err -> "target error: " <> err
    CompileClangError exitCode err -> "clang failed (" <> show exitCode <> "): " <> err
    CompileToolError tool exitCode err -> tool <> " failed (" <> show exitCode <> "): " <> err

defaultCompileTarget :: NativeTarget
defaultCompileTarget = fromMaybe AppleArm64 hostNativeTarget

assemble :: FilePath -> NativeTarget -> GarbageCollector -> Bool -> FilePath -> FilePath -> [FilePath] -> IO ()
assemble storeRoot Wasm32Wasip3 garbageCollector useWasmOpt output assemblyPath archives =
  assembleWasip3 storeRoot garbageCollector useWasmOpt output assemblyPath archives
assemble storeRoot target garbageCollector _useWasmOpt output assemblyPath archives = do
  (compiler, targetArguments) <- backendCompiler target
  runtimeArchive <- requireInstalledRuntime storeRoot target garbageCollector
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      compiler
      ( targetArguments
          <> [assemblyPath]
          <> [runtimeArchive]
          <> archives
          <> ["-o", output]
      )
      ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (renderCompileError (CompileClangError exitCode stderr)))

assembleWasip3 :: FilePath -> GarbageCollector -> Bool -> FilePath -> FilePath -> [FilePath] -> IO ()
assembleWasip3 storeRoot garbageCollector useWasmOpt output assemblyPath archives = do
  clangOverride <- lookupEnv "AIHC_WASM_CLANG"
  wasmOpt <- if useWasmOpt then findExecutable "wasm-opt" else pure Nothing
  withTemporaryDirectory "aihc-wasip3-link" $ \directory -> do
    let programObject = directory </> "program.o"
        unoptimizedCoreModule = directory </> "core.unoptimized.wasm"
        coreModule = directory </> "core.wasm"
        linkedCoreModule = maybe coreModule (const unoptimizedCoreModule) wasmOpt
        (clang, clangTargetArguments) = wasmClangCommand clangOverride
    runWasmClangTool clang (clangTargetArguments <> ["-mtail-call", "-c", assemblyPath, "-o", programObject])
    runtimeArchive <- requireInstalledRuntime storeRoot Wasm32Wasip3 garbageCollector
    runTool
      "wasm-ld"
      ( [ "--no-entry",
          "--export-memory",
          "--allow-undefined",
          programObject
        ]
          <> ["--whole-archive", runtimeArchive, "--no-whole-archive"]
          <> archives
          <> ["-o", linkedCoreModule]
      )
    forM_ wasmOpt $ \tool -> runTool tool (wasmOptArguments linkedCoreModule coreModule)
    runTool "wasm-tools" ["component", "new", coreModule, "-o", output]
    runTool "wasm-tools" ["validate", output]

requireInstalledRuntime :: FilePath -> NativeTarget -> GarbageCollector -> IO FilePath
requireInstalledRuntime storeRoot target garbageCollector = do
  let archive = installedRuntimeArchivePath storeRoot target (runtimeGarbageCollector garbageCollector)
  exists <- doesFileExist archive
  if exists
    then pure archive
    else ioError (userError ("runtime is not installed: " <> archive <> "; run `aihc prepare-runtime` first"))

runTool :: FilePath -> [String] -> IO ()
runTool tool arguments = do
  (exitCode, _stdout, stderr) <- readProcessWithExitCode tool arguments ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (renderCompileError (CompileToolError tool exitCode stderr)))

runWasmClangTool :: FilePath -> [String] -> IO ()
runWasmClangTool clang arguments = do
  (exitCode, _stdout, stderr) <- readWasmClangProcessWithExitCode clang arguments
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (renderCompileError (CompileToolError clang exitCode stderr)))

backendSourceExtension :: NativeTarget -> String
backendSourceExtension Llvm = ".ll"
backendSourceExtension Wasm32Wasip3 = ".s"
backendSourceExtension _ = ".s"

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
