{-# LANGUAGE TypeApplications #-}

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
    typecheckModulesWithEnv,

    -- * Result types
    TcResult (..),
    TcModuleResult (..),
    TcBindingResult (..),
    tcmBindings,
    tcModuleDiagnostics,

    -- * Re-exports for convenience
    TcType (..),
    Kind (..),
    TyCon (..),
    TyVarId (..),
    TypeScheme (..),
    Pred (..),
    Unique (..),
    TcAnnotation (..),
    TcBindingAnnotation (..),
    TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
    renderTcType,
    renderTcSignature,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ClassDeclItem (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    Expr (..),
    Extension (..),
    InstanceDeclItem (..),
    Module (..),
    Pattern (..),
    SourceSpan (..),
    applyExtensionSetting,
    applyImpliedExtensions,
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Annotations (TcAnnotation (..), TcBindingAnnotation (..), renderTcSignature, renderTcType)
import Aihc.Tc.Constraint (eqPrimarySpan)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Generate.Decl (TcBindingResult (..), moduleBindings, tcModule)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Data (Data, Typeable, cast, gmapM, gmapQ)
import Data.List (minimumBy)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)

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
  (ty, cts) <- inferExpr expr
  -- 2. Solve constraints.
  _result <- solveConstraints cts
  -- 3. Zonk the result type.
  zonkType ty

-- | Result of type-checking a module.
data TcModuleResult = TcModuleResult
  { -- | Module annotated with type-checker elaboration data.
    tcmModule :: !Module,
    -- | Whether type checking succeeded (no errors).
    tcmSuccess :: !Bool
  }
  deriving (Show)

tcmBindings :: TcModuleResult -> [TcBindingResult]
tcmBindings =
  moduleBindings . tcmModule

-- | Type-check a single module, processing data declarations and
-- value bindings.
typecheckModule :: Module -> TcModuleResult
typecheckModule =
  typecheckModuleWithEnv []

-- | Type-check a single module with preloaded top-level term bindings.
typecheckModuleWithEnv :: [(Text, TypeScheme)] -> Module -> TcModuleResult
typecheckModuleWithEnv importedTerms m =
  case typecheckModulesWithEnv importedTerms [m] of
    [result] -> result
    _ ->
      TcModuleResult
        { tcmModule = m,
          tcmSuccess = False
        }

-- | Type-check modules in order while sharing the accumulated top-level
-- environment. This is intentionally pragmatic: callers that have already
-- resolved a dependency-ordered module list can feed it here so later modules
-- see earlier data constructors and value bindings.
typecheckModulesWithEnv :: [(Text, TypeScheme)] -> [Module] -> [TcModuleResult]
typecheckModulesWithEnv importedTerms =
  go initState
  where
    initState =
      initTcState
        { tcsGlobalTerms =
            Map.fromList
              [ (name, TcIdBinder name scheme Closed)
              | (name, scheme) <- importedTerms
              ]
              <> tcsGlobalTerms initTcState
        }

    go _ [] = []
    go st (m : ms) =
      let (result, st') = typecheckModuleWithState st m
       in result : go st' ms

typecheckModuleWithState :: TcState -> Module -> (TcModuleResult, TcState)
typecheckModuleWithState st m =
  case runTcM tcEnv (st {tcsDiagnostics = []}) (tcModule m) of
    Left _abort ->
      ( TcModuleResult
          { tcmModule = m,
            tcmSuccess = False
          },
        st
      )
    Right (annotatedModule, st') ->
      let diags = reverse (tcsDiagnostics st')
          hasErrors = any isError diags
          result =
            TcModuleResult
              { tcmModule = annotateModuleDiagnostics diags annotatedModule,
                tcmSuccess = not hasErrors
              }
          nextState =
            st'
              { tcsDiagnostics = [],
                tcsMetaSolutions = Map.empty,
                tcsKindSolutions = Map.empty,
                tcsEvBinds = Map.empty,
                tcsOccurrenceElaborations = Map.empty,
                tcsBindingElaborations = Map.empty
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
    isError d = diagSeverity d == TcError

-- | Type-check a list of modules.
typecheck :: [Module] -> [TcModuleResult]
typecheck = typecheckModulesWithEnv []

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics modu =
  foldl attachDiagnostic modu diagnostics

attachDiagnostic :: Module -> TcDiagnostic -> Module
attachDiagnostic modu diagnostic =
  case bestDiagnosticCarrierSpan (diagnosticSpan diagnostic) modu of
    Just carrierSpan ->
      let (modu', attached) = runState (attachDiagnosticAt carrierSpan diagnostic modu) False
       in if attached
            then modu'
            else appendModuleDiagnostic diagnostic modu
    Nothing ->
      appendModuleDiagnostic diagnostic modu

appendModuleDiagnostic :: TcDiagnostic -> Module -> Module
appendModuleDiagnostic diagnostic modu =
  modu {moduleAnns = moduleAnns modu <> [mkAnnotation diagnostic]}

bestDiagnosticCarrierSpan :: SourceSpan -> Module -> Maybe SourceSpan
bestDiagnosticCarrierSpan NoSourceSpan _ = Nothing
bestDiagnosticCarrierSpan target modu =
  case filter (`spanContains` target) (collectDiagnosticCarrierSpans modu) of
    [] -> Nothing
    spans -> Just (minimumBy compareSpanSize spans)

collectDiagnosticCarrierSpans :: (Data a) => a -> [SourceSpan]
collectDiagnosticCarrierSpans node =
  annotationListSpans
    <> exprSpans
    <> declSpans
    <> patternSpans
    <> dataConSpans
    <> classItemSpans
    <> instanceItemSpans
    <> compStmtSpans
    <> concat (gmapQ collectDiagnosticCarrierSpans node)
  where
    annotationListSpans =
      maybe [] (maybeToList . concreteSpan . sourceSpanFromAnns) (cast node :: Maybe [Annotation])
    exprSpans =
      maybe [] exprAnnCarrierSpans (cast node :: Maybe Expr)
    declSpans =
      maybe [] declAnnCarrierSpans (cast node :: Maybe Decl)
    patternSpans =
      maybe [] patternAnnCarrierSpans (cast node :: Maybe Pattern)
    dataConSpans =
      maybe [] dataConAnnCarrierSpans (cast node :: Maybe DataConDecl)
    classItemSpans =
      maybe [] classItemAnnCarrierSpans (cast node :: Maybe ClassDeclItem)
    instanceItemSpans =
      maybe [] instanceItemAnnCarrierSpans (cast node :: Maybe InstanceDeclItem)
    compStmtSpans =
      maybe [] compStmtAnnCarrierSpans (cast node :: Maybe CompStmt)

attachDiagnosticAt :: (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticAt carrierSpan diagnostic node = do
  node' <- gmapM (attachDiagnosticAt carrierSpan diagnostic) node
  alreadyAttached <- get
  if alreadyAttached
    then pure node'
    else attachDiagnosticHere carrierSpan diagnostic node'

attachDiagnosticHere :: (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticHere carrierSpan diagnostic =
  tryCastM (attachAnnotationList carrierSpan diagnostic)
    >=> tryCastM (attachExprAnn carrierSpan diagnostic)
    >=> tryCastM (attachDeclAnn carrierSpan diagnostic)
    >=> tryCastM (attachPatternAnn carrierSpan diagnostic)
    >=> tryCastM (attachDataConAnn carrierSpan diagnostic)
    >=> tryCastM (attachClassItemAnn carrierSpan diagnostic)
    >=> tryCastM (attachInstanceItemAnn carrierSpan diagnostic)
    >=> tryCastM (attachCompStmtAnn carrierSpan diagnostic)

tryCastM :: forall a b. (Data a, Typeable b) => (b -> State Bool b) -> a -> State Bool a
tryCastM f node =
  case cast node of
    Just typedNode -> do
      typedNode' <- f typedNode
      pure (fromMaybe node (cast typedNode'))
    Nothing ->
      pure node

attachAnnotationList :: SourceSpan -> TcDiagnostic -> [Annotation] -> State Bool [Annotation]
attachAnnotationList carrierSpan diagnostic anns
  | sourceSpanFromAnns anns == carrierSpan = do
      put True
      pure (anns <> [mkAnnotation diagnostic])
  | otherwise =
      pure anns

attachExprAnn :: SourceSpan -> TcDiagnostic -> Expr -> State Bool Expr
attachExprAnn carrierSpan diagnostic expr@(EAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (EAnn (mkAnnotation diagnostic) expr)
attachExprAnn _ _ expr =
  pure expr

attachDeclAnn :: SourceSpan -> TcDiagnostic -> Decl -> State Bool Decl
attachDeclAnn carrierSpan diagnostic decl@(DeclAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (DeclAnn (mkAnnotation diagnostic) decl)
attachDeclAnn _ _ decl =
  pure decl

attachPatternAnn :: SourceSpan -> TcDiagnostic -> Pattern -> State Bool Pattern
attachPatternAnn carrierSpan diagnostic pattern'@(PAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (PAnn (mkAnnotation diagnostic) pattern')
attachPatternAnn _ _ pattern' =
  pure pattern'

attachDataConAnn :: SourceSpan -> TcDiagnostic -> DataConDecl -> State Bool DataConDecl
attachDataConAnn carrierSpan diagnostic dataCon@(DataConAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (DataConAnn (mkAnnotation diagnostic) dataCon)
attachDataConAnn _ _ dataCon =
  pure dataCon

attachClassItemAnn :: SourceSpan -> TcDiagnostic -> ClassDeclItem -> State Bool ClassDeclItem
attachClassItemAnn carrierSpan diagnostic item@(ClassItemAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (ClassItemAnn (mkAnnotation diagnostic) item)
attachClassItemAnn _ _ item =
  pure item

attachInstanceItemAnn :: SourceSpan -> TcDiagnostic -> InstanceDeclItem -> State Bool InstanceDeclItem
attachInstanceItemAnn carrierSpan diagnostic item@(InstanceItemAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (InstanceItemAnn (mkAnnotation diagnostic) item)
attachInstanceItemAnn _ _ item =
  pure item

attachCompStmtAnn :: SourceSpan -> TcDiagnostic -> CompStmt -> State Bool CompStmt
attachCompStmtAnn carrierSpan diagnostic stmt@(CompAnn ann inner)
  | annotationCarrierSpan ann inner == carrierSpan = do
      put True
      pure (CompAnn (mkAnnotation diagnostic) stmt)
attachCompStmtAnn _ _ stmt =
  pure stmt

exprAnnCarrierSpans :: Expr -> [SourceSpan]
exprAnnCarrierSpans (EAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
exprAnnCarrierSpans _ = []

declAnnCarrierSpans :: Decl -> [SourceSpan]
declAnnCarrierSpans (DeclAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
declAnnCarrierSpans _ = []

patternAnnCarrierSpans :: Pattern -> [SourceSpan]
patternAnnCarrierSpans (PAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
patternAnnCarrierSpans _ = []

dataConAnnCarrierSpans :: DataConDecl -> [SourceSpan]
dataConAnnCarrierSpans (DataConAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
dataConAnnCarrierSpans _ = []

classItemAnnCarrierSpans :: ClassDeclItem -> [SourceSpan]
classItemAnnCarrierSpans (ClassItemAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
classItemAnnCarrierSpans _ = []

instanceItemAnnCarrierSpans :: InstanceDeclItem -> [SourceSpan]
instanceItemAnnCarrierSpans (InstanceItemAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
instanceItemAnnCarrierSpans _ = []

compStmtAnnCarrierSpans :: CompStmt -> [SourceSpan]
compStmtAnnCarrierSpans (CompAnn ann inner) =
  maybeToList (concreteSpan (annotationCarrierSpan ann inner))
compStmtAnnCarrierSpans _ = []

sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns =
  fromMaybe NoSourceSpan . firstConcreteSpan . map spanFromAnnotation

spanFromAnnotation :: Annotation -> SourceSpan
spanFromAnnotation =
  fromMaybe NoSourceSpan . fromAnnotation @SourceSpan

annotationCarrierSpan :: (Data a) => Annotation -> a -> SourceSpan
annotationCarrierSpan annotation inner =
  case spanFromAnnotation annotation of
    NoSourceSpan -> firstSourceSpan inner
    span' -> span'

firstSourceSpan :: (Data a) => a -> SourceSpan
firstSourceSpan node =
  fromMaybe NoSourceSpan $
    concreteSpan ownSpan
      <|> firstConcreteSpan (gmapQ firstSourceSpan node)
  where
    ownSpan =
      fromMaybe NoSourceSpan $
        (spanFromAnnotation <$> cast node)
          <|> (sourceSpanFromAnns <$> cast node)

firstConcreteSpan :: [SourceSpan] -> Maybe SourceSpan
firstConcreteSpan =
  foldr ((<|>) . concreteSpan) Nothing

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan span' = Just span'

spanContains :: SourceSpan -> SourceSpan -> Bool
spanContains carrier target =
  case (carrier, target) of
    (SourceSpan carrierSource carrierStartLine carrierStartCol carrierEndLine carrierEndCol carrierStartOffset carrierEndOffset, SourceSpan targetSource targetStartLine targetStartCol targetEndLine targetEndCol targetStartOffset targetEndOffset) ->
      carrierSource == targetSource
        && (carrierStartLine, carrierStartCol, carrierStartOffset) <= (targetStartLine, targetStartCol, targetStartOffset)
        && (targetEndLine, targetEndCol, targetEndOffset) <= (carrierEndLine, carrierEndCol, carrierEndOffset)
    _ -> False

compareSpanSize :: SourceSpan -> SourceSpan -> Ordering
compareSpanSize left right =
  compare (spanSize left) (spanSize right)

spanSize :: SourceSpan -> Int
spanSize (SourceSpan _ _ _ _ _ startOffset endOffset) =
  endOffset - startOffset
spanSize NoSourceSpan =
  maxBound

diagnosticSpan :: TcDiagnostic -> SourceSpan
diagnosticSpan diagnostic =
  case diagLoc diagnostic of
    NoSourceSpan ->
      case diagKind diagnostic of
        UnificationError _ _ _ (Just provenance) -> eqPrimarySpan provenance
        _ -> NoSourceSpan
    span' -> span'

tcModuleDiagnostics :: Module -> [TcDiagnostic]
tcModuleDiagnostics =
  collectDiagnostics
  where
    collectDiagnostics :: (Data a) => a -> [TcDiagnostic]
    collectDiagnostics node =
      maybe [] annotationDiagnostics (cast node)
        <> concat (gmapQ collectDiagnostics node)

    annotationDiagnostics :: Annotation -> [TcDiagnostic]
    annotationDiagnostics annotation =
      maybeToList (fromAnnotation annotation)
