{-# LANGUAGE OverloadedStrings #-}

-- | The compiler front end over one module, against the core libraries.
--
-- The session holds the names and the types the module of the reader can
-- see. They come from a bundle of the core libraries, written by
-- @aihc-dev play-bundle@ from a store. The helper checks each user module
-- against these facts.
module Aihc.Play.Session
  ( Session,
    sessionExports,
    sessionInterface,
    bundleSession,
    sessionBundleStats,
    Problem (..),
    problemJson,
    userPackage,
    parseUserModule,
    resolveUserModule,
    typecheckUserModule,
    sessionWiring,
    sessionPrim,
    withQuoteImports,
    checkModules,
    checkProgram,
  )
where

import Aihc.Cli.PlayBundle (BundleStats, LoadedBundle (..), loadBundle)
import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( Extension (..),
    LanguageEdition (..),
    Module (..),
    ModuleHeaderPragmas (..),
    SourceSpan,
    effectiveExtensions,
    moduleLanguagePragmas,
    moduleName,
    sourceSpanEndCol,
    sourceSpanEndLine,
    sourceSpanSourceName,
    sourceSpanStartCol,
    sourceSpanStartLine,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Play.Json (Json (..))
import Aihc.Prim.Wiring (builtinFunctionModules, primTcConfig, primTcWiring)
import Aihc.Resolve
  ( ModuleExports,
    Package (..),
    PackageId (..),
    ResolveError (..),
    ResolveFailure (..),
    ResolvedModule (..),
    ResolvedUnit (..),
    builtins,
    collectModuleExportsWithDeps,
    modulesInPackage,
  )
import Aihc.Tc
  ( MergeCheck (TrustMergedFacts),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcInterface,
    TcSeverity (..),
    TcWiring,
    mergeTcInterfaces,
    renderFunDepNames,
    renderPred,
    renderTcType,
    tcModuleDiagnostics,
    typecheckModulesWithInterface,
  )
import Aihc.Tc.Syntax qualified as TcSyntax
import Aihc.Th.Prepare (resolveUnitQuotes)
import Aihc.Th.Quote (moduleHasQuote, quoteImports)
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Session = Session
  { sessionExports :: !ModuleExports,
    sessionInterface :: !TcInterface,
    -- | The identity of the primitive package the facts name.
    sessionPrim :: !PackageId,
    sessionBundleStats :: !BundleStats
  }

-- | One problem of one phase.
data Problem = Problem
  { problemStage :: !Text,
    problemSpan :: !(Maybe SourceSpan),
    problemMessage :: !Text
  }
  deriving (Eq, Show)

problemJson :: Problem -> Json
problemJson problem =
  JObject
    ( [("stage", JString (problemStage problem)), ("message", JString (problemMessage problem))]
        <> maybe [] spanFields (problemSpan problem)
    )
  where
    spanFields span' =
      [ ("line", JInt (sourceSpanStartLine span')),
        ("col", JInt (sourceSpanStartCol span')),
        ("endLine", JInt (sourceSpanEndLine span')),
        ("endCol", JInt (sourceSpanEndCol span'))
      ]

-- | The package of the reader's module. The resolver shows an empty
-- package as the main one.
userPackage :: Package
userPackage = Package "" (PackageId "")

sessionWiring :: Session -> TcWiring
sessionWiring = primTcWiring . sessionPrim

-- | A session over a bundle of the core libraries. The header of the
-- bundle is decoded now. Each type fact is decoded when the type checker
-- first looks it up.
bundleSession :: ByteString -> Either String Session
bundleSession bytes = do
  loaded <- loadBundle bytes
  pure
    Session
      { sessionExports = loadedExports loaded,
        sessionInterface = loadedInterface loaded,
        sessionPrim = loadedPrim loaded,
        sessionBundleStats = loadedStats loaded
      }

-- | The extensions a module header declares over a language edition.
headerExtensions :: LanguageEdition -> Text -> [Extension]
headerExtensions defaultEdition source =
  effectiveExtensions edition (headerExtensionSettings header)
  where
    header = readModuleHeaderPragmas source
    edition = fromMaybe defaultEdition (headerLanguageEdition header)

parseWith :: FilePath -> [Extension] -> Text -> ([Problem], Module)
parseWith path extensions source =
  let config = defaultConfig {parserSourceName = path, parserExtensions = extensions}
      (errors, modu) = parseModule config source
   in ([Problem "parse" (Just span') message | (span', message) <- errors], modu)

-- | Parse the module of the reader. Its header pragmas apply over GHC2021.
parseUserModule :: Text -> ([Problem], Module, [Extension])
parseUserModule source =
  let extensions = headerExtensions GHC2021Edition source
      (problems, modu) = parseWith "Main.hs" extensions source
   in (problems, modu, effectiveExtensions GHC2021Edition (moduleLanguagePragmas modu))

-- | The module with the imports of the quote library when it has a
-- quote. Name resolution changes each quote into calls of that library.
withQuoteImports :: Module -> Module
withQuoteImports modu
  | moduleHasQuote modu = modu {moduleImports = moduleImports modu <> quoteImports}
  | otherwise = modu

-- | Resolve the module of the reader against the session. A failure still
-- gives the module with the resolutions that succeeded.
resolveUserModule :: Session -> Module -> [Extension] -> ([Problem], [ResolvedModule])
resolveUserModule session modu extensions =
  case resolveUnitQuotes scope visible units of
    Right (ResolvedUnit result) -> ([], result)
    Left failure -> (map resolveProblem (failureErrors failure), failureModules failure)
  where
    units = modulesInPackage userPackage [(modu, extensions)]
    visible = collectModuleExportsWithDeps (sessionExports session) units <> sessionExports session
    scope = builtins userPackage visible builtinFunctionModules

resolveProblem :: ResolveError -> Problem
resolveProblem failure =
  Problem "resolve" (resolveErrorSpan failure) (resolveErrorName failure <> ": " <> T.pack (resolveErrorMessage failure))

-- | Type check the resolved module of the reader against the session.
typecheckUserModule :: Session -> [ResolvedModule] -> ([Problem], [TcSyntax.Module])
typecheckUserModule session resolved =
  (concatMap (map tcProblem . filter isError . tcModuleDiagnostics) checked, checked)
  where
    (checked, _) = typecheckModulesWithInterface (primTcConfig (sessionPrim session)) (sessionInterface session) resolved
    isError diagnostic = diagSeverity diagnostic == TcError

tcProblem :: TcDiagnostic -> Problem
tcProblem diagnostic =
  Problem "typecheck" (diagLoc diagnostic) (T.pack (renderKind (diagKind diagnostic)))

-- | Resolve and type check modules of one package against the session.
-- The result has the checked modules and the facts of the session with the
-- facts of the modules.
checkModules :: Session -> Package -> [(Module, [Extension])] -> Either [Problem] ([TcSyntax.Module], TcInterface)
checkModules session package modules = do
  let units = modulesInPackage package modules
      visible = collectModuleExportsWithDeps (sessionExports session) units <> sessionExports session
      scope = builtins package visible builtinFunctionModules
  resolved <- case resolveUnitQuotes scope visible units of
    Left failure -> Left (map resolveProblem (failureErrors failure))
    Right (ResolvedUnit result) -> Right result
  let (checked, interface) = typecheckModulesWithInterface (primTcConfig (sessionPrim session)) (sessionInterface session) resolved
  case concatMap (map tcProblem . filter ((== TcError) . diagSeverity) . tcModuleDiagnostics) checked of
    [] -> Right (checked, mergeTcInterfaces TrustMergedFacts [sessionInterface session, interface])
    problems -> Left problems

-- | Check the user module and the executable entry with the same interface.
-- The module has the results of its splices.
checkProgram :: Session -> Module -> [Extension] -> Either [Problem] ([TcSyntax.Module], TcInterface)
checkProgram session modu extensions = do
  let entryExtensions = headerExtensions GHC2021Edition entrySource
      (entryProblems, entry) = parseWith entryPath entryExtensions entrySource
  require entryProblems
  require [Problem "input" Nothing "The module must have the name Main." | moduleName modu `notElem` [Nothing, Just "Main"]]
  either (Left . map entryProblem) Right (checkModules session (Package "exe" (PackageId "exe")) [(modu, extensions), (entry, entryExtensions)])
  where
    require [] = Right ()
    require problems = Left problems
    entryPath = "Aihc/Entry.hs"
    entryProblem problem
      | maybe False ((== T.pack entryPath) . sourceSpanSourceName) (problemSpan problem) =
          problem {problemSpan = Nothing, problemMessage = "The main function is invalid. " <> problemMessage problem}
      | otherwise = problem
    entrySource =
      T.unlines
        [ "{-# LANGUAGE NoImplicitPrelude #-}",
          "module Aihc.Entry where",
          "import qualified Main",
          "import GHC.TopHandler (runMainIO)",
          "entry = runMainIO Main.main"
        ]

renderKind :: TcErrorKind -> String
renderKind kind =
  case kind of
    UnificationError left right _ _ ->
      "could not match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError variable ty ->
      "occurs check failed: " <> renderTcType variable <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted predicate _ ->
      "unsolved constraint " <> renderPred predicate
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      "function argument " <> T.unpack name <> " has type " <> renderTcType ty <> " without a fixed runtime representation"
    FunDepUnknownTyVar className name ->
      "the functional dependency of class " <> T.unpack className <> " names " <> T.unpack name <> ", which is not a parameter of the class"
    InstanceFunDepCoverage predicate determiners determined ->
      "instance " <> renderPred predicate <> " does not determine " <> unwords (map T.unpack determined) <> " from " <> unwords (map T.unpack determiners)
    InstanceFunDepConflict predicate other determiners determined ->
      "instance " <> renderPred predicate <> " conflicts with instance " <> renderPred other <> " under the functional dependency " <> renderFunDepNames determiners determined
    OtherError message ->
      message
