{-# LANGUAGE OverloadedStrings #-}

-- | Compile a standalone Haskell module through System FC and GRIN to a
-- native Darwin AArch64 executable.
module Aihc.Cli.Compile
  ( CompileEnvironment (..),
    CompileError (..),
    compileOutputPath,
    compileSourceToCoreWithDependencies,
    compileSourceToAssembly,
    compileSourceToAssemblyWithDependencies,
    defaultCompileEnvironment,
    renderCompileError,
    runCompile,
    runCompileWithEnvironment,
  )
where

import Aihc.Arm64 (Arm64Error, compileProgram, runtimeSourcePath)
import Aihc.Cli.Compile.Dependencies
  ( CompileEnvironment (..),
    DependencyArtifact (..),
    buildDependencies,
  )
import Aihc.Cli.Options (CompileOptions (..))
import Aihc.Fc
  ( DesugarResult (..),
    FcAlt (..),
    FcBind (..),
    FcExpr (..),
    FcForeignCall (..),
    FcProgram (..),
    FcTopBind (..),
    Var (..),
    desugarModule,
    desugarModuleWithBindings,
    eliminateDeadCode,
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
  | CompileArm64Error !Arm64Error
  | CompileClangError !ExitCode !String
  deriving (Eq, Show)

data CompileArtifacts = CompileArtifacts
  { compiledCore :: !Text,
    compiledGrin :: !Text,
    compiledAssembly :: !Text
  }

runCompile :: CompileOptions -> IO ()
runCompile options = do
  environment <- defaultCompileEnvironment
  runCompileWithEnvironment environment options

runCompileWithEnvironment :: CompileEnvironment -> CompileOptions -> IO ()
runCompileWithEnvironment environment options = do
  source <- TIO.readFile (compileSourceFile options)
  artifactsResult <- compileSourceToArtifactsWithDependencies environment (compileSourceFile options) source
  artifacts <- either (ioError . userError . renderCompileError) pure artifactsResult
  let output = compileOutputPath options
  writeIntermediateArtifacts output options artifacts
  if compileKeepAsm options
    then do
      let assemblyPath = output <> ".s"
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble output assemblyPath
    else withTemporaryDirectory "aihc-compile" $ \directory -> do
      let assemblyPath = directory </> "program.s"
      TIO.writeFile assemblyPath (compiledAssembly artifacts)
      assemble output assemblyPath

writeIntermediateArtifacts :: FilePath -> CompileOptions -> CompileArtifacts -> IO ()
writeIntermediateArtifacts output options artifacts = do
  when (compileKeepCore options) $
    TIO.writeFile (output <> ".core") (compiledCore artifacts)
  when (compileKeepGrin options) $
    TIO.writeFile (output <> ".grin") (compiledGrin artifacts)

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
  fmap (fmap compiledAssembly) (compileSourceToArtifactsWithDependencies environment sourceName source)

-- | Compile source and its dependencies to the optimized, combined System FC
-- program rendered by @--keep-core@.
compileSourceToCoreWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToCoreWithDependencies environment sourceName source =
  fmap (fmap compiledCore) (compileSourceToArtifactsWithDependencies environment sourceName source)

compileSourceToArtifactsWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError CompileArtifacts)
compileSourceToArtifactsWithDependencies environment sourceName source =
  case parseCompileModule sourceName source of
    Left err -> pure (Left err)
    Right parsed -> do
      dependencies <- buildDependencies environment (ImplicitPrelude `elem` sourceExtensions source) parsed
      pure $ do
        artifact <- either (Left . CompileDependencyError) Right dependencies
        compileWithDependencies artifact parsed

compileWithDependencies :: DependencyArtifact -> Module -> Either CompileError CompileArtifacts
compileWithDependencies dependencies parsed =
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
                          freshDependencies = shiftProgramVars (1 + maximumProgramUnique mainProgram) (dependencyProgram dependencies)
                          program = eliminateDeadCode "main" (appendPrograms freshDependencies mainProgram)
                       in compileProgramArtifacts program

compileProgramArtifacts :: FcProgram -> Either CompileError CompileArtifacts
compileProgramArtifacts sourceCore = do
  let core = Fc.lowerNewtypes sourceCore
  let grin = Grin.lowerProgram core
  assembly <- either (Left . CompileArm64Error) Right (compileProgram "main" grin)
  pure
    CompileArtifacts
      { compiledCore = withFinalNewline (Fc.renderProgram core),
        compiledGrin = withFinalNewline (Grin.renderProgram grin),
        compiledAssembly = assembly
      }
  where
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
        FcForeignImport foreignCall ->
          FcForeignImport foreignCall {fcForeignCallVar = shiftVar (fcForeignCallVar foreignCall)}
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
        FcDictApp function argument -> FcDictApp (shiftExpr function) (shiftExpr argument)
        FcTyApp inner ty -> FcTyApp (shiftExpr inner) ty
        FcLam var body -> FcLam (shiftVar var) (shiftExpr body)
        FcTyLam tyVar body -> FcTyLam tyVar (shiftExpr body)
        FcDictLam var body -> FcDictLam (shiftVar var) (shiftExpr body)
        FcDict fields -> FcDict (map shiftExpr fields)
        FcDictSelect dictionary index -> FcDictSelect (shiftExpr dictionary) index
        FcLet bind body -> FcLet (shiftBind bind) (shiftExpr body)
        FcCase scrutinee binder alternatives ->
          FcCase (shiftExpr scrutinee) (shiftVar binder) (map shiftAlt alternatives)
        FcCast inner coercion -> FcCast (shiftExpr inner) coercion

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
    FcForeignImport foreignCall -> [fcForeignCallVar foreignCall]
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
    FcDictApp function argument -> exprVars function <> exprVars argument
    FcTyApp inner _ -> exprVars inner
    FcLam var body -> var : exprVars body
    FcTyLam _ body -> exprVars body
    FcDictLam var body -> var : exprVars body
    FcDict fields -> concatMap exprVars fields
    FcDictSelect dictionary _ -> exprVars dictionary
    FcLet bind body -> bindVarsDeep bind <> exprVars body
    FcCase scrutinee binder alternatives -> exprVars scrutinee <> (binder : concatMap altVars alternatives)
    FcCast inner _ -> exprVars inner
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
    CompileArm64Error err -> "ARM64 code generation error: " <> show err
    CompileClangError exitCode err -> "clang failed (" <> show exitCode <> "): " <> err

assemble :: FilePath -> FilePath -> IO ()
assemble output assemblyPath = do
  runtime <- runtimeSourcePath
  (exitCode, _stdout, stderr) <-
    readProcessWithExitCode
      "clang"
      ["-std=c11", "-Wall", "-Wextra", "-Werror", runtime, assemblyPath, "-o", output]
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
