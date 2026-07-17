{-# LANGUAGE OverloadedStrings #-}

-- | Compile a standalone Haskell module through System FC and GRIN to a
-- native Darwin AArch64 executable.
module Aihc.Cli.Compile
  ( CompileEnvironment (..),
    CompileError (..),
    compileOutputPath,
    compileSourceToCoreWithDependencies,
    compileSourceToCpsGrinWithDependencies,
    compileSourceToGrinWithDependencies,
    compileSourceToWholeCoreWithDependencies,
    compileSourceToAssembly,
    compileSourceToAssemblyWithDependencies,
    defaultCompileEnvironment,
    renderCompileError,
    runCompile,
    runCompileWithEnvironment,
  )
where

import Aihc.Arm64 (Arm64Error, buildLinkLayoutFromInterfaces, compileProgram, compileProgramWithDependencies, extendLinkLayout, runtimeSourcePath, targetTriple, validatePrimitiveNames)
import Aihc.Cli.Compile.Dependencies
  ( CompileEnvironment (..),
    DependencyArtifact (..),
    DependencyUnit (..),
    buildDependencies,
  )
import Aihc.Cli.Options (CompileOptions (..))
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
  | CompileArm64Error !Arm64Error
  | CompileClangError !ExitCode !String
  deriving (Eq, Show)

data CompileArtifacts = CompileArtifacts
  { compiledCore :: !Text,
    compiledGrin :: !Text,
    compiledCpsGrin :: !Text,
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
  source <- TIO.readFile (compileSourceFile options)
  artifactsResult <- compileSourceToArtifactsWithDependencies (compileWholeProgram options) environment (compileSourceFile options) source
  artifacts <- either (ioError . userError . renderCompileError) pure artifactsResult
  let output = compileOutputPath options
  writeIntermediateArtifacts output options artifacts
  if compileKeepAsm options
    then do
      let assemblyPath = output <> ".s"
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble output assemblyPath (compiledArchives artifacts)
    else withTemporaryDirectory "aihc-compile" $ \directory -> do
      let assemblyPath = directory </> "program.s"
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble output assemblyPath (compiledArchives artifacts)

writeIntermediateArtifacts :: FilePath -> CompileOptions -> CompileArtifacts -> IO ()
writeIntermediateArtifacts output options artifacts = do
  when (compileKeepCore options) $
    TIO.writeFile (output <> ".core") (compiledCore artifacts)
  when (compileKeepGrin options) $ do
    TIO.writeFile (output <> ".grin") (compiledGrin artifacts)
    TIO.writeFile (output <> ".cps.grin") (compiledCpsGrin artifacts)

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
compileSourceToAssembly sourceName source = do
  parsed <- parseCompileModule sourceName source
  let desugared = desugarModule parsed
  if dsSuccess desugared
    then compiledAssembly <$> compileProgramArtifacts (dsProgram desugared)
    else Left (CompileFrontendError (dsErrors desugared))

compileSourceToAssemblyWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToAssemblyWithDependencies environment sourceName source =
  fmap (fmap compiledAssembly) (compileSourceToArtifactsWithDependencies False environment sourceName source)

-- | Compile source to the incremental System FC program rendered by
-- @--keep-core@. Dependency declarations participate in cross-unit lowering,
-- but their implementations remain in their separately compiled artifacts.
compileSourceToCoreWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToCoreWithDependencies environment sourceName source =
  fmap (fmap compiledCore) (compileSourceToArtifactsWithDependencies False environment sourceName source)

-- | Compile source and its dependencies to the incremental GRIN program
-- rendered by @--keep-grin@.
compileSourceToGrinWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToGrinWithDependencies environment sourceName source =
  fmap (fmap compiledGrin) (compileSourceToArtifactsWithDependencies False environment sourceName source)

-- | Compile source to the continuation-reified GRIN consumed by native
-- backends and rendered as @.cps.grin@ by @--keep-grin@.
compileSourceToCpsGrinWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToCpsGrinWithDependencies environment sourceName source =
  fmap (fmap compiledCpsGrin) (compileSourceToArtifactsWithDependencies False environment sourceName source)

-- | Compile source incrementally, then merge the resulting Core units and run
-- whole-program dead-code elimination. This is the Core rendered by
-- @--whole-program --keep-core@.
compileSourceToWholeCoreWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToWholeCoreWithDependencies environment sourceName source =
  fmap (fmap compiledCore) (compileSourceToArtifactsWithDependencies True environment sourceName source)

compileSourceToArtifactsWithDependencies :: Bool -> CompileEnvironment -> FilePath -> Text -> IO (Either CompileError CompileArtifacts)
compileSourceToArtifactsWithDependencies wholeProgram environment sourceName source =
  case parseCompileModule sourceName source of
    Left err -> pure (Left err)
    Right parsed -> do
      dependencies <- buildDependencies environment (ImplicitPrelude `elem` sourceExtensions source) (not wholeProgram) parsed
      pure $ do
        artifact <- either (Left . CompileDependencyError) Right dependencies
        compileWithDependencies wholeProgram artifact parsed

compileWithDependencies :: Bool -> DependencyArtifact -> Module -> Either CompileError CompileArtifacts
compileWithDependencies wholeProgram dependencies parsed =
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
                              then compileWholeProgramArtifacts incremental
                              else compileIncrementalArtifacts dependencies incremental

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
compileWholeProgramArtifacts :: IncrementalCompilation -> Either CompileError CompileArtifacts
compileWholeProgramArtifacts = compileProgramArtifacts . reachableLinkedCore

compileIncrementalArtifacts :: DependencyArtifact -> IncrementalCompilation -> Either CompileError CompileArtifacts
compileIncrementalArtifacts dependencies compilation = do
  let mainUnit = incrementalMainUnit compilation
      mainCore = incrementalUnitCore mainUnit
      mainGrin = incrementalUnitGrin mainUnit
      mainCpsGrin = incrementalUnitCpsGrin mainUnit
      dependencyLayout = buildLinkLayoutFromInterfaces (dependencyLinkInterfaces dependencies)
      layout = extendLinkLayout dependencyLayout mainGrin
      reachability = dependencyReachabilityInterface dependencies <> extractReachabilityInterface mainCore
      primitives = Set.toAscList (reachablePrimitiveNames "main" reachability)
  either (Left . CompileArm64Error) Right (validatePrimitiveNames primitives)
  assembly <-
    either
      (Left . CompileArm64Error)
      Right
      (compileProgramWithDependencies layout (dependencyInitializerSymbols dependencies) "main" mainCpsGrin)
  pure
    CompileArtifacts
      { compiledCore = renderCore mainCore,
        compiledGrin = renderGrin mainGrin,
        compiledCpsGrin = renderCpsGrin mainCpsGrin,
        compiledAssembly = assembly,
        compiledArchives = dependencyArchivePaths dependencies
      }

compileProgramArtifacts :: FcProgram -> Either CompileError CompileArtifacts
compileProgramArtifacts sourceCore = do
  let core = Fc.lowerNewtypes sourceCore
  let grin = Grin.lowerProgram core
  cpsGrin <- either (Left . CompileCpsGrinError) Right (Grin.toCpsGrin grin)
  assembly <- either (Left . CompileArm64Error) Right (compileProgram "main" cpsGrin)
  pure
    CompileArtifacts
      { compiledCore = renderCore core,
        compiledGrin = renderGrin grin,
        compiledCpsGrin = renderCpsGrin cpsGrin,
        compiledAssembly = assembly,
        compiledArchives = []
      }

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
    CompileArm64Error err -> "ARM64 code generation error: " <> show err
    CompileClangError exitCode err -> "clang failed (" <> show exitCode <> "): " <> err

assemble :: FilePath -> FilePath -> [FilePath] -> IO ()
assemble output assemblyPath archives = do
  runtime <- runtimeSourcePath
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "clang"
      (["--target=" <> targetTriple, "-std=c11", "-Wall", "-Wextra", "-Werror", runtime, assemblyPath] <> archives <> ["-o", output])
      ""
  case exitCode of
    ExitSuccess -> pure ()
    ExitFailure _ -> ioError (userError (renderCompileError (CompileClangError exitCode stderr)))

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
