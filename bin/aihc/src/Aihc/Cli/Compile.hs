{-# LANGUAGE OverloadedStrings #-}

-- | Compile a standalone Haskell module through System FC and GRIN to a
-- native executable for the host platform.
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
    renderCompileError,
    runCompile,
    runCompileWithEnvironment,
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
import Aihc.Native
  ( LinkLayout,
    NativeTarget (..),
    buildLinkLayoutFromInterfaces,
    extendLinkLayout,
    hostNativeTarget,
    nativeTargetTriple,
    runtimeSourcePath,
  )
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax (Extension (ImplicitPrelude), LanguageEdition (Haskell98Edition), Module, effectiveExtensions, headerExtensionSettings, headerLanguageEdition)
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ResolveResult (..), resolveWithDeps)
import Aihc.Tc (Unique (..), tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModulesWithFullEnv)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (XdgDirectory (XdgCache), createDirectory, getCurrentDirectory, getTemporaryDirectory, getXdgDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, (</>))
import System.IO (hClose, openTempFile)
import System.Process (readProcessWithExitCode)

data CompileError
  = CompileParseError !String
  | CompileFrontendError ![String]
  | CompileDependencyError !String
  | CompileCpsGrinError !Grin.CpsGrinError
  | CompileNativeError !NativeError
  | CompileTargetError !String
  | CompileClangError !ExitCode !String
  deriving (Eq, Show)

data NativeError
  = NativeArm64Error !Arm64.Arm64Error
  | NativeAmd64Error !Amd64.Amd64Error
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
    incrementalMainUnit :: !IncrementalUnit
  }

runCompile :: CompileOptions -> IO ()
runCompile options = do
  environment <- defaultCompileEnvironment
  runCompileWithEnvironment environment options

runCompileWithEnvironment :: CompileEnvironment -> CompileOptions -> IO ()
runCompileWithEnvironment environment options = do
  target <-
    case compileTarget options of
      Just explicitTarget -> pure explicitTarget
      Nothing ->
        maybe
          (ioError (userError (renderCompileError (CompileTargetError "unsupported host; pass --target apple-arm64 or --target linux-amd64"))))
          pure
          hostNativeTarget
  source <- TIO.readFile (compileSourceFile options)
  artifactsResult <- compileSourceToArtifactsWithDependencies target (compileWholeProgram options) environment (compileSourceFile options) source
  artifacts <- either (ioError . userError . renderCompileError) pure artifactsResult
  let output = compileOutputPath options
  writeIntermediateArtifacts output options artifacts
  if compileKeepAsm options
    then do
      let assemblyPath = output <> ".s"
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble target (compileGarbageCollector options) output assemblyPath (compiledArchives artifacts)
    else withTemporaryDirectory "aihc-compile" $ \directory -> do
      let assemblyPath = directory </> "program.s"
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble target (compileGarbageCollector options) output assemblyPath (compiledArchives artifacts)

writeIntermediateArtifacts :: FilePath -> CompileOptions -> CompileArtifacts -> IO ()
writeIntermediateArtifacts output options artifacts = do
  when (compileKeepCore options) $
    TIO.writeFile (output <> ".core") (compiledCore artifacts)
  when (compileKeepGrin options) $ do
    TIO.writeFile (output <> ".grin") (compiledGrin artifacts)
    TIO.writeFile (output <> ".cps.grin") (compiledCpsGrin artifacts)
    TIO.writeFile (output <> ".gc.grin") (compiledGcGrin artifacts)

-- | The project-local core libraries and shared compiled-library cache used by
-- the command-line compiler.
defaultCompileEnvironment :: IO CompileEnvironment
defaultCompileEnvironment = do
  cwd <- getCurrentDirectory
  cacheDirectory <- getXdgDirectory XdgCache "aihc"
  pure (CompileEnvironment (cwd </> "core-libs") (cacheDirectory </> "libraries"))

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
        compileWithDependencies target wholeProgram artifact parsed

compileWithDependencies :: NativeTarget -> Bool -> DependencyArtifact -> Module -> Either CompileError CompileArtifacts
compileWithDependencies target wholeProgram dependencies parsed =
  case resolveWithDeps (dependencyExports dependencies) [parsed] of
    ResolveResult {resolveErrors = errors@(_ : _)} -> Left (CompileFrontendError ["resolve error: " <> show errors])
    ResolveResult {resolvedModules} ->
      let (checkedModules, _, _) =
            typecheckModulesWithFullEnv
              (dependencyTerms dependencies)
              (dependencyTyCons dependencies)
              (dependencyInstances dependencies)
              resolvedModules
       in if not (all tcModuleSuccess checkedModules)
            then Left (CompileFrontendError ["typecheck error: " <> show (concatMap tcModuleDiagnostics checkedModules)])
            else
              let bindings = dependencyBindings dependencies <> concatMap tcModuleBindings checkedModules
                  desugared = zipWith (desugarModuleWithBindings bindings) checkedModules resolvedModules
               in if not (all dsSuccess desugared)
                    then Left (CompileFrontendError (concatMap dsErrors desugared))
                    else
                      let mainProgram = FcProgram (concatMap (fcTopBinds . dsProgram) desugared)
                       in do
                            incremental <- compileIncrementally dependencies mainProgram
                            if wholeProgram
                              then compileWholeProgramArtifacts target incremental
                              else compileIncrementalArtifacts target dependencies incremental

-- | Compile every module SCC to its own normalized Core and GRIN unit before any
-- optional whole-program transformation is considered.
compileIncrementally :: DependencyArtifact -> FcProgram -> Either CompileError IncrementalCompilation
compileIncrementally dependencies unoptimizedMain =
  do
    mainCpsGrin <- either (Left . CompileCpsGrinError) Right (Grin.toCpsGrin mainGrin)
    pure
      IncrementalCompilation
        { incrementalDependencyUnits =
            [ IncrementalUnit (dependencyUnitProgram unit) (dependencyUnitGrin unit) (dependencyUnitCpsGrin unit)
            | unit <- dependencyUnits dependencies
            ],
          incrementalMainUnit = IncrementalUnit mainCore mainGrin mainCpsGrin
        }
  where
    mainCore = Fc.lowerNewtypesWithInterface (dependencyNewtypeInterface dependencies) (eliminateDeadCode "main" unoptimizedMain)
    mainGrin = Grin.lowerProgramWithInterface (dependencyGrinInterface dependencies) mainCore

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
reachableLinkedCore = eliminateDeadCode "main" . mergeIncrementalCore

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
      dependencyLayout = buildLinkLayoutFromInterfaces (dependencyLinkInterfaces dependencies)
      layout = extendLinkLayout dependencyLayout mainGrin
      reachability = dependencyReachabilityInterface dependencies <> extractReachabilityInterface mainCore
      primitives = Set.toAscList (reachablePrimitiveNames "main" reachability)
  either (Left . CompileNativeError) Right (validateNativePrimitiveNames target primitives)
  assembly <-
    either
      (Left . CompileNativeError)
      Right
      (compileNativeProgramWithDependencies target layout (dependencyInitializerSymbols dependencies) "main" mainCpsGrin)
  pure
    CompileArtifacts
      { compiledCore = renderCore mainCore,
        compiledGrin = renderGrin mainGrin,
        compiledCpsGrin = renderCpsGrin mainCpsGrin,
        compiledGcGrin = renderCpsGrin (Grin.lowerGc mainCpsGrin),
        compiledAssembly = assembly,
        compiledArchives = dependencyArchivePaths dependencies
      }

compileProgramArtifacts :: NativeTarget -> FcProgram -> Either CompileError CompileArtifacts
compileProgramArtifacts target sourceCore = do
  let core = Fc.lowerNewtypes sourceCore
  let grin = Grin.lowerProgram core
  cpsGrin <- either (Left . CompileCpsGrinError) Right (Grin.toCpsGrin grin)
  assembly <- either (Left . CompileNativeError) Right (compileNativeProgram target "main" cpsGrin)
  pure
    CompileArtifacts
      { compiledCore = renderCore core,
        compiledGrin = renderGrin grin,
        compiledCpsGrin = renderCpsGrin cpsGrin,
        compiledGcGrin = renderCpsGrin (Grin.lowerGc cpsGrin),
        compiledAssembly = assembly,
        compiledArchives = []
      }

validateNativePrimitiveNames :: NativeTarget -> [Text] -> Either NativeError ()
validateNativePrimitiveNames target names =
  case target of
    AppleArm64 -> either (Left . NativeArm64Error) Right (Arm64.validatePrimitiveNames names)
    LinuxAmd64 -> either (Left . NativeAmd64Error) Right (Amd64.validatePrimitiveNames names)

compileNativeProgram :: NativeTarget -> Text -> Grin.CpsGrinProgram -> Either NativeError Text
compileNativeProgram target entry program =
  case target of
    AppleArm64 -> either (Left . NativeArm64Error) Right (Arm64.compileProgram entry program)
    LinuxAmd64 -> either (Left . NativeAmd64Error) Right (Amd64.compileProgram entry program)

compileNativeProgramWithDependencies :: NativeTarget -> LinkLayout -> [Text] -> Text -> Grin.CpsGrinProgram -> Either NativeError Text
compileNativeProgramWithDependencies target layout initializers entry program =
  case target of
    AppleArm64 -> either (Left . NativeArm64Error) Right (Arm64.compileProgramWithDependencies layout initializers entry program)
    LinuxAmd64 -> either (Left . NativeAmd64Error) Right (Amd64.compileProgramWithDependencies layout initializers entry program)

renderCore :: FcProgram -> Text
renderCore = withFinalNewline . Fc.renderProgram

renderGrin :: Grin.GrinProgram -> Text
renderGrin = withFinalNewline . Grin.renderProgram

renderCpsGrin :: Grin.CpsGrinProgram -> Text
renderCpsGrin = renderGrin . Grin.cpsGrinProgram

withFinalNewline :: String -> Text
withFinalNewline rendered = T.pack rendered <> "\n"

appendPrograms :: FcProgram -> FcProgram -> FcProgram
appendPrograms (FcProgram left) (FcProgram right) = FcProgram (left <> right)

maximumProgramUnique :: FcProgram -> Int
maximumProgramUnique = maximum . (0 :) . map varUniqueInt . programVars

varUniqueInt :: Var -> Int
varUniqueInt Var {varUnique = Unique unique} = unique

shiftProgramVars :: Int -> FcProgram -> FcProgram
shiftProgramVars offset (FcProgram topBinds) = FcProgram (map shiftTopBind topBinds)
  where
    shiftVar var@Var {varUnique = Unique unique} = var {varUnique = Unique (unique + offset)}

    shiftTopBind topBind =
      case topBind of
        FcData {} -> topBind
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
    FcData {} -> []
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
    CompileNativeError err -> "native code generation error: " <> show err
    CompileTargetError err -> "native target error: " <> err
    CompileClangError exitCode err -> "clang failed (" <> show exitCode <> "): " <> err

defaultCompileTarget :: NativeTarget
defaultCompileTarget = fromMaybe AppleArm64 hostNativeTarget

assemble :: NativeTarget -> GarbageCollector -> FilePath -> FilePath -> [FilePath] -> IO ()
assemble target garbageCollector output assemblyPath archives = do
  runtime <- runtimeSourcePath
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "clang"
      ( [ "--target=" <> nativeTargetTriple target,
          "-std=c11",
          "-Wall",
          "-Wextra",
          "-Werror",
          garbageCollectorDefine garbageCollector,
          runtime,
          assemblyPath
        ]
          <> archives
          <> ["-o", output]
      )
      ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (renderCompileError (CompileClangError exitCode stderr)))

garbageCollectorDefine :: GarbageCollector -> String
garbageCollectorDefine garbageCollector =
  case garbageCollector of
    GcCalloc -> "-DAIHC_GC=AIHC_GC_CALLOC"
    GcSemispace -> "-DAIHC_GC=AIHC_GC_SEMISPACE"

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
