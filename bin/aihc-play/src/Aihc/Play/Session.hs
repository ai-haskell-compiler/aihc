{-# LANGUAGE OverloadedStrings #-}

-- | The compiler front end over one module, against a small Prelude.
--
-- The session holds the names and the types of the primitive package of
-- aihc and of the small Prelude of the guide. The three commands of the
-- helper parse, resolve, and type check one module against it.
module Aihc.Play.Session
  ( Session,
    buildSession,
    supportModulePaths,
    Problem (..),
    problemJson,
    parseUserModule,
    resolveUserModule,
    typecheckUserModule,
    sessionWiring,
  )
where

import Aihc.Parser (ParserConfig (..), defaultConfig, parseModule)
import Aihc.Parser.Syntax
  ( DataDecl (..),
    Decl (..),
    ExportSpec (..),
    Extension (..),
    LanguageEdition (..),
    Module (..),
    ModuleHead (..),
    ModuleHeaderPragmas (..),
    Name (..),
    SourceSpan,
    UnqualifiedName (..),
    binderHeadName,
    effectiveExtensions,
    moduleLanguagePragmas,
    moduleName,
    sourceSpanEndCol,
    sourceSpanEndLine,
    sourceSpanStartCol,
    sourceSpanStartLine,
  )
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Aihc.Play.Json (Json (..))
import Aihc.Prim.Wiring (primTcConfig, primTcWiring)
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
    resolveUnit,
  )
import Aihc.Tc
  ( MergeCheck (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcInterface,
    TcSeverity (..),
    TcWiring,
    emptyTcInterface,
    mergeTcInterfaces,
    renderFunDepNames,
    renderPred,
    renderTcType,
    tcModuleDiagnostics,
    tcModuleSuccess,
    typecheckModuleSccWithInterface,
    typecheckModulesWithInterface,
  )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data Session = Session
  { sessionExports :: !ModuleExports,
    sessionInterface :: !TcInterface
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

primPackageId :: PackageId
primPackageId = PackageId "aihc-prim"

primPackage :: Package
primPackage = Package "aihc-prim" primPackageId

-- | The package of the reader's module. The resolver shows an empty
-- package as the main one.
userPackage :: Package
userPackage = Package "" (PackageId "")

sessionWiring :: TcWiring
sessionWiring = primTcWiring primPackageId

-- | The modules that the session needs, relative to the source root of
-- aihc-prim and to the prelude directory of the helper.
supportModulePaths :: ([FilePath], [FilePath])
supportModulePaths =
  ( ["GHC/Types.hs", "GHC/Prim.hs", "Aihc/Prim/IO.hs", "GHC/Tuple.hs", "GHC/CString.hs"],
    ["GHC/Classes.hs", "GHC/Num.hs", "GHC/Prim/Base.hs", "Prelude.hs"]
  )

-- | The modules whose exports the syntax reaches without an import: the
-- literal and @do@ desugaring names, and the class names of deriving.
builtinModules :: [Text]
builtinModules = ["GHC.Classes", "GHC.Num", "GHC.Prim", "GHC.Prim.Base", "GHC.Types"]

-- | Parse, resolve and type check the support modules as one unit of the
-- primitive package. The primitive modules and the small Prelude compile
-- together, so deriving and the literal syntax find their classes in the
-- package that the type checker expects.
buildSession :: [(FilePath, Text)] -> Either String Session
buildSession sources = do
  modules <- traverse (uncurry parseSupportModule) sources
  let units = modulesInPackage primPackage modules
      exports = collectModuleExportsWithDeps mempty units
      scope = builtins primPackage exports builtinModules
  resolved <-
    case resolveUnit scope exports units of
      Right (ResolvedUnit result) -> Right result
      Left failure -> Left ("support modules do not resolve: " <> show (failureErrors failure))
  let (checked, interface) = typecheckModuleSccWithInterface (primTcConfig primPackageId) emptyTcInterface resolved
  if all tcModuleSuccess checked
    then Right (Session exports interface)
    else
      Left
        ( "support modules do not type check: "
            <> unlines
              [ T.unpack (fromMaybe "?" (moduleName modu)) <> ": " <> T.unpack (problemMessage problem)
              | modu <- checked,
                problem <- map tcProblem (tcModuleDiagnostics modu)
              ]
        )

parseSupportModule :: FilePath -> Text -> Either String (Module, [Extension])
parseSupportModule path source =
  case parseWith path extensions source of
    (problems@(_ : _), _) -> Left (path <> ": " <> T.unpack (T.intercalate "; " (map problemMessage problems)))
    ([], modu) -> Right (withoutUnboxedSums modu, extensions)
  where
    -- The support modules never import the Prelude of the guide. Their
    -- header pragmas say so, and the primitive modules of aihc have no
    -- Prelude to import.
    extensions = filter (/= ImplicitPrelude) (headerExtensions GHC2021Edition source)

-- | Drop the unboxed sum types of @GHC.Types@.
--
-- @GHC.Types@ declares @Sum2#@ to @Sum63#@, with one constructor for each
-- alternative: about two thousand constructors that take most of the time
-- to check the primitive package. No chapter of the guide uses an unboxed
-- sum, so the playground leaves them out and starts in a fraction of the
-- time.
withoutUnboxedSums :: Module -> Module
withoutUnboxedSums modu =
  modu
    { moduleHead = fmap dropExports (moduleHead modu),
      moduleDecls = filter (not . isSumDecl) (moduleDecls modu)
    }
  where
    dropExports header = header {moduleHeadExports = fmap (filter (not . isSumExport)) (moduleHeadExports header)}
    isSumDecl decl =
      case decl of
        DeclAnn _ inner -> isSumDecl inner
        DeclData dataDecl -> isSumName (unqualifiedNameText (binderHeadName (dataDeclHead dataDecl)))
        DeclStandaloneKindSig name _ -> isSumName (unqualifiedNameText name)
        _ -> False
    isSumExport export =
      case export of
        ExportAnn _ inner -> isSumExport inner
        ExportAll _ _ name -> isSumName (nameText name)
        ExportAbs _ _ name -> isSumName (nameText name)
        ExportWith _ _ name _ -> isSumName (nameText name)
        ExportWithAll _ _ name _ _ -> isSumName (nameText name)
        _ -> False
    isSumName name =
      "Sum" `T.isPrefixOf` name && "#" `T.isSuffixOf` name && T.all (`elem` ['0' .. '9']) (T.dropEnd 1 (T.drop 3 name))

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

-- | Resolve the module of the reader against the session. A failure still
-- gives the module with the resolutions that succeeded.
resolveUserModule :: Session -> Module -> [Extension] -> ([Problem], [ResolvedModule])
resolveUserModule session modu extensions =
  case resolveUnit scope visible units of
    Right (ResolvedUnit result) -> ([], result)
    Left failure -> (map resolveProblem (failureErrors failure), failureModules failure)
  where
    units = modulesInPackage userPackage [(modu, extensions)]
    visible = collectModuleExportsWithDeps (sessionExports session) units <> sessionExports session
    scope = builtins userPackage visible builtinModules

resolveProblem :: ResolveError -> Problem
resolveProblem failure =
  Problem "resolve" (resolveErrorSpan failure) (resolveErrorName failure <> ": " <> T.pack (resolveErrorMessage failure))

-- | Type check the resolved module of the reader against the session.
typecheckUserModule :: Session -> [ResolvedModule] -> ([Problem], [Module])
typecheckUserModule session resolved =
  (concatMap (map tcProblem . filter isError . tcModuleDiagnostics) checked, checked)
  where
    interface = mergeTcInterfaces CheckMergedFacts [sessionInterface session]
    (checked, _) = typecheckModulesWithInterface (primTcConfig primPackageId) interface resolved
    isError diagnostic = diagSeverity diagnostic == TcError

tcProblem :: TcDiagnostic -> Problem
tcProblem diagnostic =
  Problem "typecheck" (diagLoc diagnostic) (T.pack (renderKind (diagKind diagnostic)))

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
