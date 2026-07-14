{-# LANGUAGE OverloadedStrings #-}

-- | Compile a standalone Haskell module through System FC and GRIN to a
-- native Darwin AArch64 executable.
module Aihc.Cli.Compile
  ( CompileEnvironment (..),
    CompileError (..),
    compileOutputPath,
    compileSourceToAssembly,
    compileSourceToAssemblyWithDependencies,
    renderCompileError,
    runCompile,
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
  )
import Aihc.Grin (lowerProgram)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax (Extension (ImplicitPrelude), LanguageEdition (Haskell98Edition), Module, effectiveExtensions, headerExtensionSettings, headerLanguageEdition)
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Resolve (ResolveResult (..), resolveWithDeps)
import Aihc.Tc (Unique (..), tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModulesWithFullEnv)
import Control.Exception (bracket)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import System.Directory (createDirectory, getCurrentDirectory, getTemporaryDirectory, removeDirectoryRecursive, removeFile)
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

runCompile :: CompileOptions -> IO ()
runCompile options = do
  source <- TIO.readFile (compileSourceFile options)
  cwd <- getCurrentDirectory
  let environment = CompileEnvironment (cwd </> "core-libs") (cwd </> ".aihc-cache" </> "libraries")
  assemblyResult <- compileSourceToAssemblyWithDependencies environment (compileSourceFile options) source
  assembly <- either (ioError . userError . renderCompileError) pure assemblyResult
  let output = compileOutputPath options
  if compileKeepAsm options
    then do
      let assemblyPath = output <> ".s"
      TIO.writeFile assemblyPath assembly
      assemble output assemblyPath
    else withTemporaryDirectory "aihc-compile" $ \directory -> do
      let assemblyPath = directory </> "program.s"
      TIO.writeFile assemblyPath assembly
      assemble output assemblyPath

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
    then either (Left . CompileArm64Error) Right (compileProgram "main" (lowerProgram (dsProgram desugared)))
    else Left (CompileFrontendError (dsErrors desugared))

compileSourceToAssemblyWithDependencies :: CompileEnvironment -> FilePath -> Text -> IO (Either CompileError Text)
compileSourceToAssemblyWithDependencies environment sourceName source =
  case parseCompileModule sourceName source of
    Left err -> pure (Left err)
    Right parsed -> do
      dependencies <- buildDependencies environment (ImplicitPrelude `elem` sourceExtensions source) parsed
      pure $ do
        artifact <- either (Left . CompileDependencyError) Right dependencies
        compileWithDependencies artifact parsed

compileWithDependencies :: DependencyArtifact -> Module -> Either CompileError Text
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
                          program = retainReachableTopBinds "main" (appendPrograms freshDependencies mainProgram)
                       in either (Left . CompileArm64Error) Right (compileProgram "main" (lowerProgram program))

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

-- Native linking starts from @main@. Keeping unreachable dependency bindings
-- would force the backend to implement primitives that the executable never
-- calls and would initialize dead closures at startup.
retainReachableTopBinds :: Text -> FcProgram -> FcProgram
retainReachableTopBinds entry (FcProgram topBinds) =
  FcProgram [topBind | (index, topBind) <- indexedTopBinds, keepTopBind index topBind]
  where
    indexedTopBinds = zip [0 :: Int ..] topBinds
    definitions = Map.fromList [(varName var, expressions) | topBind <- topBinds, (var, expressions) <- topBindDefinitions topBind]
    selectedDefinitions = Map.fromList [(varName var, index) | (index, topBind) <- indexedTopBinds, var <- topBindVars topBind]
    reachable = closeReachable (Set.singleton entry)

    closeReachable names =
      let referenced = Set.unions [foldMap referencedNames (Map.findWithDefault [] name definitions) | name <- Set.toList names]
          names' = names <> Set.filter (`Map.member` definitions) referenced
       in if names' == names then names else closeReachable names'

    keepTopBind index topBind =
      case topBind of
        FcData {} -> True
        FcNewtype {} -> True
        _ -> any (isSelectedReachable index) (topBindVars topBind)

    isSelectedReachable index var =
      varName var `Set.member` reachable
        && Map.lookup (varName var) selectedDefinitions == Just index

topBindDefinitions :: FcTopBind -> [(Var, [FcExpr])]
topBindDefinitions topBind =
  case topBind of
    FcPrimitive var _ -> [(var, [])]
    FcForeignImport foreignCall -> [(fcForeignCallVar foreignCall, [])]
    FcTopBind (FcNonRec var expression) -> [(var, [expression])]
    FcTopBind (FcRec bindings) -> [(var, map snd bindings) | (var, _) <- bindings]
    FcData {} -> []
    FcNewtype {} -> []

topBindVars :: FcTopBind -> [Var]
topBindVars = map fst . topBindDefinitions

referencedNames :: FcExpr -> Set Text
referencedNames expression =
  case expression of
    FcVar var -> Set.singleton (varName var)
    FcLit _ -> Set.empty
    FcApp function argument -> referencedNames function <> referencedNames argument
    FcDictApp function argument -> referencedNames function <> referencedNames argument
    FcTyApp inner _ -> referencedNames inner
    FcLam _ body -> referencedNames body
    FcTyLam _ body -> referencedNames body
    FcDictLam _ body -> referencedNames body
    FcDict fields -> foldMap referencedNames fields
    FcDictSelect dictionary _ -> referencedNames dictionary
    FcLet bind body -> referencedBindNames bind <> referencedNames body
    FcCase scrutinee _ alternatives -> referencedNames scrutinee <> foldMap (referencedNames . altRhs) alternatives
    FcCast inner _ -> referencedNames inner

referencedBindNames :: FcBind -> Set Text
referencedBindNames bind =
  case bind of
    FcNonRec _ expression -> referencedNames expression
    FcRec bindings -> foldMap (referencedNames . snd) bindings

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
