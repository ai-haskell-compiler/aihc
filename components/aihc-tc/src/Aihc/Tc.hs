{-# LANGUAGE ScopedTypeVariables #-}

-- | Entry point for the aihc type checker.
--
-- The type checker consumes a parsed and name-resolved AST
-- and produces the same AST annotated with typing information. It does
-- not transform the tree structure.
--
-- The implementation follows the OutsideIn(X) algorithm:
--
-- 1. Generate wanted constraints by walking the AST.
-- 2. Solve the constraints using the worklist/inert-set architecture.
-- 3. Zonk meta-variables.
-- 4. Attach type annotations to AST nodes.
module Aihc.Tc
  ( -- * Entry point
    typecheck,
    typecheckExpr,
    typecheckModule,
    typecheckModuleWithEnv,
    typecheckModuleWithEnvAndInstances,
    typecheckModulesWithEnv,
    typecheckModulesWithEnvAndInstances,
    typecheckModulesWithFullEnv,

    -- * Result types
    TcResult (..),
    TcBindingResult (..),

    -- * Module result projections
    tcModuleBindings,
    tcModuleDiagnostics,
    tcModuleInstances,
    tcModuleSuccess,

    -- * Re-exports for convenience
    TcType (..),
    Kind (..),
    TyCon (..),
    TyVarId (..),
    TypeScheme (..),
    Pred (..),
    InstanceInfo (..),
    TyConInfo (..),
    Unique (..),
    TcAnnotation (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderPred,
    renderTcSignature,
    renderTcType,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    Extension (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDeclItem (..),
    Literal (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Annotations (TcAnnotation (..), renderPred, renderTcSignature, renderTcType)
import Aihc.Tc.Env (InstanceInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), moduleBindings, moduleInstances, tcModule)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((<=<))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Data (Data, gmapM, gmapQ)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Typeable (cast)

-- | Result of type checking.
data TcResult = TcResult
  { -- | The inferred type of the top-level expression or binding.
    tcResultType :: !TcType,
    -- | Diagnostics (errors and warnings) produced.
    tcResultDiagnostics :: ![TcDiagnostic],
    -- | Whether type checking succeeded (no errors).
    tcResultSuccess :: !Bool
  }
  deriving (Show)

-- | Type-check a single expression in an empty environment.
--
-- This is the primary entry point for testing. For full module
-- type-checking, use 'typecheck'.
typecheckExpr :: Expr -> TcResult
typecheckExpr expr =
  case runTcM emptyTcEnv initTcState (typecheckExprM expr) of
    Left _abort ->
      TcResult
        { tcResultType = TcMetaTv (Unique (-1)),
          tcResultDiagnostics = [],
          tcResultSuccess = False
        }
    Right (ty, st) ->
      let diags = reverse (tcsDiagnostics st)
          hasErrors = any isError diags
       in TcResult
            { tcResultType = ty,
              tcResultDiagnostics = diags,
              tcResultSuccess = not hasErrors
            }
  where
    isError d = diagSeverity d == TcError

-- | Internal: type-check an expression in TcM.
typecheckExprM :: Expr -> TcM TcType
typecheckExprM expr = do
  -- 1. Generate constraints.
  (_expr', ty, cts) <- inferExpr expr
  -- 2. Solve constraints.
  _result <- solveConstraints cts
  -- 3. Zonk the result type.
  zonkType ty

-- | Top-level bindings recovered from a type-checked module's annotations.
tcModuleBindings :: Module -> [TcBindingResult]
tcModuleBindings =
  moduleBindings

-- | Class instances recovered from a type-checked module's annotations.
tcModuleInstances :: Module -> [InstanceInfo]
tcModuleInstances =
  moduleInstances

-- | Diagnostics recovered from type-checker annotations in a module.
tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  collectTcDiagnostics

-- | Whether an annotated module contains no type-checker errors.
tcModuleSuccess :: Module -> Bool
tcModuleSuccess =
  not . any isError . tcModuleDiagnostics
  where
    isError diagnostic = diagSeverity diagnostic == TcError

-- | Type-check a single module, processing data declarations and
-- value bindings.
typecheckModule :: Module -> Module
typecheckModule =
  typecheckModuleWithEnv []

-- | Type-check a single module with preloaded top-level term bindings.
typecheckModuleWithEnv :: [(Text, TypeScheme)] -> Module -> Module
typecheckModuleWithEnv importedTerms =
  typecheckModuleWithEnvAndInstances importedTerms []

-- | Type-check a single module with preloaded terms and class instances.
typecheckModuleWithEnvAndInstances :: [(Text, TypeScheme)] -> [InstanceInfo] -> Module -> Module
typecheckModuleWithEnvAndInstances importedTerms importedInstances m =
  case typecheckModulesWithEnvAndInstances importedTerms importedInstances [m] of
    [result] -> result
    _ ->
      annotateModuleDiagnostics [internalAbortDiagnostic "type checker returned unexpected module count"] m

-- | Type-check modules in order while sharing the accumulated top-level
-- environment. This is intentionally pragmatic: callers that have already
-- resolved a dependency-ordered module list can feed it here so later modules
-- see earlier data constructors and value bindings.
typecheckModulesWithEnv :: [(Text, TypeScheme)] -> [Module] -> [Module]
typecheckModulesWithEnv importedTerms =
  typecheckModulesWithEnvAndInstances importedTerms []

-- | Type-check modules in order with preloaded terms and class instances.
typecheckModulesWithEnvAndInstances :: [(Text, TypeScheme)] -> [InstanceInfo] -> [Module] -> [Module]
typecheckModulesWithEnvAndInstances importedTerms importedInstances =
  firstOfThree . typecheckModulesWithFullEnv importedTerms [] importedInstances

firstOfThree :: (a, b, c) -> a
firstOfThree (first, _, _) = first

-- | Type-check modules with a complete imported type-checker interface and
-- return the accumulated term schemes and type constructors for downstream
-- modules.
typecheckModulesWithFullEnv :: [(Text, TypeScheme)] -> [TyConInfo] -> [InstanceInfo] -> [Module] -> ([Module], [(Text, TypeScheme)], [TyConInfo])
typecheckModulesWithFullEnv importedTerms importedTyCons importedInstances modules =
  let (checkedModules, finalState) = go initState modules
   in ( checkedModules,
        [ (name, scheme)
        | (name, TcIdBinder scheme _) <- Map.toList (tcsGlobalTerms finalState)
        ],
        Map.elems (tcsGlobalTyCons finalState)
      )
  where
    initState =
      initTcState
        { tcsGlobalTerms =
            Map.fromList
              [ (name, TcIdBinder scheme Closed)
              | (name, scheme) <- importedTerms
              ]
              <> tcsGlobalTerms initTcState,
          tcsGlobalTyCons =
            Map.fromList
              [ (tciName tyCon, tyCon)
              | tyCon <- importedTyCons
              ]
              <> tcsGlobalTyCons initTcState,
          tcsInstances = importedInstances
        }

    go st [] = ([], st)
    go st (m : ms) =
      let (result, st') = typecheckModuleWithState st m
          (results, finalState) = go st' ms
       in (result : results, finalState)

typecheckModuleWithState :: TcState -> Module -> (Module, TcState)
typecheckModuleWithState st m =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule m) of
    Left abort ->
      ( annotateModuleDiagnostics [internalAbortDiagnostic (tcAbortMessage abort)] m,
        st
      )
    Right (annotatedModule, st') ->
      let diags = reverse (tcsDiagnostics st')
          result = annotateModuleDiagnostics diags annotatedModule
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = Map.empty,
                tcsKindSolutions = Map.empty,
                tcsEvBinds = Map.empty
              }
       in (result, nextState)
  where
    tcEnv =
      emptyTcEnv
        { tcEnvMonoLocalBinds = MonoLocalBinds `elem` enabledExtensions,
          tcEnvMonomorphismRestriction = MonomorphismRestriction `elem` enabledExtensions
        }
    enabledExtensions =
      applyImpliedExtensions $
        foldr applyExtensionSetting [MonoLocalBinds, MonomorphismRestriction] (moduleLanguagePragmas m)

-- | Type-check a list of modules.
typecheck :: [Module] -> [Module]
typecheck = typecheckModulesWithEnv []

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  let (located, unlocated) = partitionDiagnostics diagnostics
      moduleWithLocated = foldl attachLocatedDiagnostic m located
   in moduleWithLocated {moduleAnns = moduleAnns moduleWithLocated <> map mkAnnotation unlocated}

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

attachLocatedDiagnostic :: Module -> (SourceSpan, TcDiagnostic) -> Module
attachLocatedDiagnostic m (sp, diagnostic) =
  case runState (attachDiagnosticAt sp diagnostic m) False of
    (m', True) -> m'
    (_, False) ->
      error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)

-- Attach bottom-up so an exact child span wins over an exact parent span.
-- Located diagnostics must never guess: if no exact syntax span exists, abort.
attachDiagnosticAt :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticAt sp diagnostic value = do
  value' <- gmapM (attachDiagnosticAt sp diagnostic) value
  alreadyAttached <- get
  if alreadyAttached
    then pure value'
    else case attachDiagnosticHere sp diagnostic value' of
      Just value'' -> do
        put True
        pure value''
      Nothing ->
        pure value'

attachDiagnosticHere :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> Maybe a
attachDiagnosticHere sp diagnostic value =
  attachAnnotationList
    <|> attachExpr
    <|> attachPattern
    <|> attachType
    <|> attachDecl
    <|> attachDataConDecl
    <|> attachLiteral
    <|> attachGuardQualifier
    <|> attachDoStmtExpr
    <|> attachDoStmtCmd
    <|> attachCompStmt
    <|> attachArithSeq
    <|> attachClassDeclItem
    <|> attachInstanceDeclItem
    <|> attachCmd
    <|> attachExportSpec
    <|> attachImportItem
  where
    diagnosticAnn = mkAnnotation diagnostic
    atExactSpan span' wrap =
      if span' == sp
        then cast wrap
        else Nothing
    attachTyped :: forall node. (Data node) => (node -> Maybe node) -> Maybe a
    attachTyped f = do
      node <- cast value
      node' <- f node
      cast node'
    attachAnnotationList =
      attachTyped $ \(anns :: [Annotation]) ->
        atExactSpan (spanFromAnnotations anns) (anns <> [diagnosticAnn])
    attachExpr =
      attachTyped $ \(expr :: Expr) ->
        atExactSpan (wrappedSpan peelExprAnnOnce expr) (EAnn diagnosticAnn expr)
    attachPattern =
      attachTyped $ \(pat :: Pattern) ->
        atExactSpan (wrappedSpan peelPatternAnnOnce pat) (PAnn diagnosticAnn pat)
    attachType =
      attachTyped $ \(ty :: Type) ->
        atExactSpan (wrappedSpan peelTypeAnnOnce ty) (TAnn diagnosticAnn ty)
    attachDecl =
      attachTyped $ \(decl :: Decl) ->
        atExactSpan (wrappedSpan peelDeclAnnOnce decl) (DeclAnn diagnosticAnn decl)
    attachDataConDecl =
      attachTyped $ \(decl :: DataConDecl) ->
        atExactSpan (wrappedSpan peelDataConAnnOnce decl) (DataConAnn diagnosticAnn decl)
    attachLiteral =
      attachTyped $ \(lit :: Literal) ->
        atExactSpan (wrappedSpan peelLiteralAnnOnce lit) (LitAnn diagnosticAnn lit)
    attachGuardQualifier =
      attachTyped $ \(qualifier :: GuardQualifier) ->
        atExactSpan (wrappedSpan peelGuardAnnOnce qualifier) (GuardAnn diagnosticAnn qualifier)
    attachDoStmtExpr =
      attachTyped $ \(stmt :: DoStmt Expr) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachDoStmtCmd =
      attachTyped $ \(stmt :: DoStmt Cmd) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachCompStmt =
      attachTyped $ \(stmt :: CompStmt) ->
        atExactSpan (wrappedSpan peelCompAnnOnce stmt) (CompAnn diagnosticAnn stmt)
    attachArithSeq =
      attachTyped $ \(seq' :: ArithSeq) ->
        atExactSpan (wrappedSpan peelArithSeqAnnOnce seq') (ArithSeqAnn diagnosticAnn seq')
    attachClassDeclItem =
      attachTyped $ \(item :: ClassDeclItem) ->
        atExactSpan (wrappedSpan peelClassItemAnnOnce item) (ClassItemAnn diagnosticAnn item)
    attachInstanceDeclItem =
      attachTyped $ \(item :: InstanceDeclItem) ->
        atExactSpan (wrappedSpan peelInstanceItemAnnOnce item) (InstanceItemAnn diagnosticAnn item)
    attachCmd =
      attachTyped $ \(cmd :: Cmd) ->
        atExactSpan (wrappedSpan peelCmdAnnOnce cmd) (CmdAnn diagnosticAnn cmd)
    attachExportSpec =
      attachTyped $ \(spec :: ExportSpec) ->
        atExactSpan (wrappedSpan peelExportAnnOnce spec) (ExportAnn diagnosticAnn spec)
    attachImportItem =
      attachTyped $ \(item :: ImportItem) ->
        atExactSpan (wrappedSpan peelImportAnnOnce item) (ImportAnn diagnosticAnn item)

wrappedSpan :: (node -> Maybe (Annotation, node)) -> node -> SourceSpan
wrappedSpan peel =
  spanFromAnnotations . fst . peelLeading peel

peelLeading :: (node -> Maybe (Annotation, node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (ann, inner) -> go (ann : anns) inner
        Nothing -> (reverse anns, node)

peelExprAnnOnce :: Expr -> Maybe (Annotation, Expr)
peelExprAnnOnce (EAnn ann inner) = Just (ann, inner)
peelExprAnnOnce _ = Nothing

peelPatternAnnOnce :: Pattern -> Maybe (Annotation, Pattern)
peelPatternAnnOnce (PAnn ann inner) = Just (ann, inner)
peelPatternAnnOnce _ = Nothing

peelTypeAnnOnce :: Type -> Maybe (Annotation, Type)
peelTypeAnnOnce (TAnn ann inner) = Just (ann, inner)
peelTypeAnnOnce _ = Nothing

peelDeclAnnOnce :: Decl -> Maybe (Annotation, Decl)
peelDeclAnnOnce (DeclAnn ann inner) = Just (ann, inner)
peelDeclAnnOnce _ = Nothing

peelDataConAnnOnce :: DataConDecl -> Maybe (Annotation, DataConDecl)
peelDataConAnnOnce (DataConAnn ann inner) = Just (ann, inner)
peelDataConAnnOnce _ = Nothing

peelLiteralAnnOnce :: Literal -> Maybe (Annotation, Literal)
peelLiteralAnnOnce (LitAnn ann inner) = Just (ann, inner)
peelLiteralAnnOnce _ = Nothing

peelGuardAnnOnce :: GuardQualifier -> Maybe (Annotation, GuardQualifier)
peelGuardAnnOnce (GuardAnn ann inner) = Just (ann, inner)
peelGuardAnnOnce _ = Nothing

peelDoAnnOnce :: DoStmt body -> Maybe (Annotation, DoStmt body)
peelDoAnnOnce (DoAnn ann inner) = Just (ann, inner)
peelDoAnnOnce _ = Nothing

peelCompAnnOnce :: CompStmt -> Maybe (Annotation, CompStmt)
peelCompAnnOnce (CompAnn ann inner) = Just (ann, inner)
peelCompAnnOnce _ = Nothing

peelArithSeqAnnOnce :: ArithSeq -> Maybe (Annotation, ArithSeq)
peelArithSeqAnnOnce (ArithSeqAnn ann inner) = Just (ann, inner)
peelArithSeqAnnOnce _ = Nothing

peelClassItemAnnOnce :: ClassDeclItem -> Maybe (Annotation, ClassDeclItem)
peelClassItemAnnOnce (ClassItemAnn ann inner) = Just (ann, inner)
peelClassItemAnnOnce _ = Nothing

peelInstanceItemAnnOnce :: InstanceDeclItem -> Maybe (Annotation, InstanceDeclItem)
peelInstanceItemAnnOnce (InstanceItemAnn ann inner) = Just (ann, inner)
peelInstanceItemAnnOnce _ = Nothing

peelCmdAnnOnce :: Cmd -> Maybe (Annotation, Cmd)
peelCmdAnnOnce (CmdAnn ann inner) = Just (ann, inner)
peelCmdAnnOnce _ = Nothing

peelExportAnnOnce :: ExportSpec -> Maybe (Annotation, ExportSpec)
peelExportAnnOnce (ExportAnn ann inner) = Just (ann, inner)
peelExportAnnOnce _ = Nothing

peelImportAnnOnce :: ImportItem -> Maybe (Annotation, ImportItem)
peelImportAnnOnce (ImportAnn ann inner) = Just (ann, inner)
peelImportAnnOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> SourceSpan
spanFromAnnotations =
  fromMaybe NoSourceSpan . foldr ((<|>) . spanFromAnnotation) Nothing

spanFromAnnotation :: Annotation -> Maybe SourceSpan
spanFromAnnotation =
  concreteSpan <=< fromAnnotation

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan sp = Just sp

collectTcDiagnostics :: (Data a) => a -> [TcDiagnostic]
collectTcDiagnostics value =
  case cast value of
    Just ann -> maybeToList (fromAnnotation ann)
    Nothing -> concat (gmapQ collectTcDiagnostics value)

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
