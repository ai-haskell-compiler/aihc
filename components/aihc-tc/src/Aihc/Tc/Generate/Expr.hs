{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for expressions.
--
-- This module implements bidirectional type inference/checking for the
-- surface expression language. It walks the surface AST and returns:
--
--   * An inferred type
--   * A list of wanted constraints (equalities)
--
-- The generated constraints are then passed to the solver.
module Aihc.Tc.Generate.Expr
  ( inferExpr,
    attachExprFailure,
    attachRhsFailure,
    inferRhs,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    CaseAlt (..),
    CompStmt (..),
    Decl (..),
    Expr (..),
    GuardQualifier (..),
    GuardedRhs (..),
    LambdaCaseAlt (..),
    Match (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor,
    ValueDecl (..),
    fromAnnotation,
    mkAnnotation,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Tc.Annotations (TcAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcDiagnostic, TcErrorKind (..))
import Aihc.Tc.Generate.Annotate
  ( annotateDeclWithTypeWithReport,
    annotateNameElaboration,
    annotatePatternBindingWithReport,
    annotatePatternWithReport,
    attachExprElaboration,
  )
import Aihc.Tc.Generate.Bind
  ( DeclGroup (..),
    LocalDeclPlan (..),
    generalizeLocalDeclPlan,
    localBinderTypes,
    monomorphicLocalDeclPlan,
    patternBinderName,
    prepareLocalDeclPlan,
    skolemize,
    splitFunTy,
    tiePlaceholder,
    withLocalBinders,
    withLocalDeclPlaceholders,
  )
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Monad
import Aihc.Tc.NameKey (nameOccurrenceKey)
import Aihc.Tc.Types
import Control.Monad (foldM, unless)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Circular expression generation.
--
-- The 'TcSolveReport' is intentionally an input to generation. Callers may
-- tie it recursively to the solver output:
--
-- @
-- let report = solveReportFromState solveState
--     ((expr', cts, ty), genState) = run generation report
--     solveState = run solver genState cts
--  in expr'
-- @
--
-- Constraint creation does not inspect the report. Only the returned syntax
-- thunks look up failed evidence variables, so constraints can be produced
-- before the report exists.
inferExpr :: TcSolveReport -> Expr -> TcM (Expr, [Ct], TcType)
inferExpr report =
  inferExprAt report NoSourceSpan

inferExprAt :: TcSolveReport -> SourceSpan -> Expr -> TcM (Expr, [Ct], TcType)
inferExprAt report ambient expr =
  case expr of
    EAnn ann inner -> do
      (inner', cts, ty) <- inferExprAt report (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
      pure (restoreExprAnnotation ann inner', cts, ty)
    EVar name ->
      inferVar report (exprSpan expr `orSourceSpan` ambient) name
    EInt {} ->
      pure (expr, [], numericLiteralExprType expr)
    EFloat {} ->
      pure (expr, [], doubleTyCon)
    EChar {} ->
      pure (expr, [], charTyCon)
    EString {} ->
      pure (expr, [], stringTyCon)
    ELambdaPats pats body ->
      inferLambda report (exprSpan expr `orSourceSpan` ambient) pats body
    ELambdaCase alts ->
      inferLambdaCase report (exprSpan expr `orSourceSpan` ambient) alts
    ELambdaCases alts ->
      inferLambdaCases report (exprSpan expr `orSourceSpan` ambient) alts
    EApp fun arg ->
      inferApp report (exprSpan expr `orSourceSpan` ambient) fun arg
    EInfix lhs op rhs ->
      inferInfix report (exprSpan expr `orSourceSpan` ambient) lhs op rhs
    EIf cond thenE elseE ->
      inferIf report (exprSpan expr `orSourceSpan` ambient) cond thenE elseE
    ECase scrutinee alts ->
      inferCase report (exprSpan expr `orSourceSpan` ambient) scrutinee alts
    ELetDecls decls body ->
      inferLet report (exprSpan expr `orSourceSpan` ambient) decls body
    EParen inner -> do
      (inner', cts, ty) <- inferExprAt report (exprSpan expr `orSourceSpan` ambient) inner
      pure (EParen inner', cts, ty)
    ETypeSig inner ty -> do
      (inner', cts, innerTy) <- inferExprAt report (exprSpan expr `orSourceSpan` ambient) inner
      pure (ETypeSig inner' ty, cts, innerTy)
    ENegate inner -> do
      (inner', cts, innerTy) <- inferExprAt report (exprSpan expr `orSourceSpan` ambient) inner
      pure (ENegate inner', cts, innerTy)
    ETuple flavor elems ->
      inferTuple report (exprSpan expr `orSourceSpan` ambient) flavor elems
    EList elems ->
      inferList report (exprSpan expr `orSourceSpan` ambient) elems
    EListComp body quals ->
      inferListComp report (exprSpan expr `orSourceSpan` ambient) body quals
    other -> do
      diagnostic <- emitErrorDiagnostic (exprSpan expr `orSourceSpan` ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
      ty <- freshMetaTv
      pure (attachDiagnosticToExpr diagnostic expr, [], ty)

inferVar :: TcSolveReport -> SourceSpan -> Name -> TcM (Expr, [Ct], TcType)
inferVar report ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) `orSourceSpan` ambient
      name = nameToText nameSyntax
      maybeKey = nameOccurrenceKey nameSyntax
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      let elaboration =
            OccurrenceElaboration
              { occurrenceElabType = instType inst,
                occurrenceElabTypeArgs = instTypeArgs inst,
                occurrenceElabEvidenceVars = map ctEvVar cts,
                occurrenceElabTermArgTypes = []
              }
          nameSyntax' = maybe nameSyntax (const (annotateNameElaboration report elaboration nameSyntax)) maybeKey
      pure (EVar nameSyntax', cts, instType inst)
    Just (TcMonoIdBinder _ ty) -> do
      let elaboration =
            OccurrenceElaboration
              { occurrenceElabType = ty,
                occurrenceElabTypeArgs = [],
                occurrenceElabEvidenceVars = [],
                occurrenceElabTermArgTypes = []
              }
          nameSyntax' = maybe nameSyntax (const (annotateNameElaboration report elaboration nameSyntax)) maybeKey
      pure (EVar nameSyntax', [], ty)
    Nothing -> do
      diagnostic <- emitErrorDiagnostic sp (UnboundVariable (T.unpack name))
      ty <- freshMetaTv
      pure (attachDiagnosticToExpr diagnostic (EVar nameSyntax), [], ty)

inferName :: TcSolveReport -> SourceSpan -> Name -> TcM (Name, [Ct], TcType)
inferName report ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) `orSourceSpan` ambient
      name = nameToText nameSyntax
      maybeKey = nameOccurrenceKey nameSyntax
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      let elaboration =
            OccurrenceElaboration
              { occurrenceElabType = instType inst,
                occurrenceElabTypeArgs = instTypeArgs inst,
                occurrenceElabEvidenceVars = map ctEvVar cts,
                occurrenceElabTermArgTypes = []
              }
          nameSyntax' = maybe nameSyntax (const (annotateNameElaboration report elaboration nameSyntax)) maybeKey
      pure (nameSyntax', cts, instType inst)
    Just (TcMonoIdBinder _ ty) -> do
      let elaboration =
            OccurrenceElaboration
              { occurrenceElabType = ty,
                occurrenceElabTypeArgs = [],
                occurrenceElabEvidenceVars = [],
                occurrenceElabTermArgTypes = []
              }
          nameSyntax' = maybe nameSyntax (const (annotateNameElaboration report elaboration nameSyntax)) maybeKey
      pure (nameSyntax', [], ty)
    Nothing -> do
      diagnostic <- emitErrorDiagnostic sp (UnboundVariable (T.unpack name))
      ty <- freshMetaTv
      pure (nameSyntax {nameAnns = nameAnns nameSyntax <> [mkAnnotation diagnostic]}, [], ty)

predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  mkWantedCtM p ev (OccurrenceOf name) sp

inferLambda :: TcSolveReport -> SourceSpan -> [Pattern] -> Expr -> TcM (Expr, [Ct], TcType)
inferLambda report sp pats body = do
  argTys <- mapM (const freshMetaTv) pats
  patCheck <- checkPatterns sp (zip pats argTys)
  (body', bodyCts, bodyTy) <- withPatternBindings (pcBindings patCheck) (inferExprAt report sp body)
  let funTy = foldr TcFunTy bodyTy argTys
      pats' = zipWith (annotatePatternWithReport report) argTys pats
  pure (ELambdaPats pats' body', pcWantedCts patCheck ++ bodyCts, funTy)

inferLambdaCase :: TcSolveReport -> SourceSpan -> [CaseAlt Expr] -> TcM (Expr, [Ct], TcType)
inferLambdaCase report sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  (alts', cts) <- inferCaseAlts report sp argTy resTy alts
  pure (ELambdaCase alts', cts, TcFunTy argTy resTy)

inferLambdaCases :: TcSolveReport -> SourceSpan -> [LambdaCaseAlt] -> TcM (Expr, [Ct], TcType)
inferLambdaCases report sp alts = do
  let arity = maximum (0 : map (length . lambdaCaseAltPats) alts)
  argTys <- mapM (const freshMetaTv) [1 .. arity]
  resTy <- freshMetaTv
  results <- mapM (inferLambdaCaseAlt report sp argTys resTy) alts
  let alts' = map (\(alt, _, _) -> alt) results
      cts = concatMap (\(_, altCts, _) -> altCts) results
  pure (ELambdaCases alts', cts, foldr TcFunTy resTy argTys)

inferLambdaCaseAlt :: TcSolveReport -> SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM (LambdaCaseAlt, [Ct], TcType)
inferLambdaCaseAlt report sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkPatterns sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs report rhs)
  ev <- freshEvVar
  let rhsCt = mkWantedCtAt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
      rhs'' = attachRhsFailure report rhsCt rhs'
  pure (alt {lambdaCaseAltRhs = rhs''}, pcWantedCts patCheck ++ rhsCts ++ [rhsCt], resTy)

inferApp :: TcSolveReport -> SourceSpan -> Expr -> Expr -> TcM (Expr, [Ct], TcType)
inferApp report sp fun arg = do
  (fun', funCts, funTy) <- inferExprAt report sp fun
  (arg', argCts, argTy) <- inferExprAt report sp arg
  resTy <- freshMetaTv
  ev <- freshEvVar
  eqCt <-
    mkWantedEqCtM
      TypeTrace
        { typeTraceType = funTy,
          typeTraceRole = ActualType,
          typeTraceOrigin = ExpressionTypeOrigin sp
        }
      TypeTrace
        { typeTraceType = TcFunTy argTy resTy,
          typeTraceRole = ExpectedType,
          typeTraceOrigin = ConstraintTypeOrigin (AppOrigin sp)
        }
      ev
      (AppOrigin sp)
      sp
  pure (attachExprFailure report eqCt (EApp fun' arg'), funCts ++ argCts ++ [eqCt], resTy)

inferInfix :: TcSolveReport -> SourceSpan -> Expr -> Name -> Expr -> TcM (Expr, [Ct], TcType)
inferInfix report sp lhs op rhs = do
  (op', opCts, opTy) <- inferName report sp op
  (lhs', lhsCts, lhsTy) <- inferExprAt report sp lhs
  leftResTy <- freshMetaTv
  leftEv <- freshEvVar
  leftCt <- mkWantedCtM (EqPred opTy (TcFunTy lhsTy leftResTy)) leftEv (AppOrigin sp) sp
  (rhs', rhsCts, rhsTy) <- inferExprAt report sp rhs
  resTy <- freshMetaTv
  rightEv <- freshEvVar
  rightCt <- mkWantedCtM (EqPred leftResTy (TcFunTy rhsTy resTy)) rightEv (AppOrigin sp) sp
  let rebuilt = EInfix lhs' op' rhs'
  pure (attachExprFailures report [leftCt, rightCt] rebuilt, opCts ++ lhsCts ++ rhsCts ++ [leftCt, rightCt], resTy)

inferIf :: TcSolveReport -> SourceSpan -> Expr -> Expr -> Expr -> TcM (Expr, [Ct], TcType)
inferIf report sp cond thenE elseE = do
  (cond', condCts, condTy) <- inferExprAt report sp cond
  (thenE', thenCts, thenTy) <- inferExprAt report sp thenE
  (elseE', elseCts, elseTy) <- inferExprAt report sp elseE
  condEv <- freshEvVar
  condCt <-
    mkWantedEqCtM
      TypeTrace
        { typeTraceType = condTy,
          typeTraceRole = ActualType,
          typeTraceOrigin = ExpressionTypeOrigin sp
        }
      TypeTrace
        { typeTraceType = boolTyCon,
          typeTraceRole = ExpectedType,
          typeTraceOrigin = ConstraintTypeOrigin (AppOrigin sp)
        }
      condEv
      (AppOrigin sp)
      sp
  branchEv <- freshEvVar
  branchCt <-
    mkWantedEqCtM
      TypeTrace
        { typeTraceType = elseTy,
          typeTraceRole = ActualType,
          typeTraceOrigin = ExpressionTypeOrigin sp
        }
      TypeTrace
        { typeTraceType = thenTy,
          typeTraceRole = ExpectedType,
          typeTraceOrigin = ConstraintTypeOrigin (AppOrigin sp)
        }
      branchEv
      (AppOrigin sp)
      sp
  pure
    ( EIf (attachExprFailure report condCt cond') thenE' (attachExprFailure report branchCt elseE'),
      condCts ++ thenCts ++ elseCts ++ [condCt, branchCt],
      thenTy
    )

inferCase :: TcSolveReport -> SourceSpan -> Expr -> [CaseAlt Expr] -> TcM (Expr, [Ct], TcType)
inferCase report sp scrutinee alts = do
  (scrutinee', scrutCts, scrutTy) <- inferExprAt report sp scrutinee
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAlts report sp scrutTy resTy alts
  pure (ECase scrutinee' alts', scrutCts ++ altCts, resTy)

inferCaseAlts :: TcSolveReport -> SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM ([CaseAlt Expr], [Ct])
inferCaseAlts _report _sp _scrutTy _resTy [] = pure ([], [])
inferCaseAlts report sp scrutTy resTy (firstAlt : restAlts) = do
  (firstAlt', firstBranchSp, firstRhsSp, firstRhsTy, firstCts) <- inferAlt firstAlt
  resultEv <- freshEvVar
  let resultCt =
        mkWantedEqCtAt
          TypeTrace
            { typeTraceType = firstRhsTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin firstRhsSp
            }
          TypeTrace
            { typeTraceType = resTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ConstraintTypeOrigin (CaseBranchOrigin firstBranchSp)
            }
          resultEv
          (CaseBranchOrigin firstRhsSp)
          firstRhsSp
  restResults <- mapM (inferAltAgainst firstBranchSp firstRhsTy) restAlts
  let restAlts' = map fst restResults
      restCts = concatMap snd restResults
  pure (firstAlt' : restAlts', firstCts ++ [resultCt] ++ restCts)
  where
    inferAlt (CaseAlt altAnns pat rhs) = do
      let altSp = sourceSpanFromAnns altAnns
          branchSp = combineSourceSpan altSp sp
      patCheck <- checkPattern branchSp pat scrutTy
      (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs report rhs)
      let rhsSp = rhsExprSpan rhs `orSourceSpan` branchSp
      pure (CaseAlt altAnns pat rhs', branchSp, rhsSp, rhsTy, pcWantedCts patCheck ++ rhsCts)

    inferAltAgainst expectedBranchSp expectedTy alt = do
      (CaseAlt altAnns pat rhs', branchSp, rhsSp, rhsTy, cts) <- inferAlt alt
      ev <- freshEvVar
      let rhsCt =
            mkWantedEqCtAt
              TypeTrace
                { typeTraceType = rhsTy,
                  typeTraceRole = ActualType,
                  typeTraceOrigin = ExpressionTypeOrigin rhsSp
                }
              TypeTrace
                { typeTraceType = expectedTy,
                  typeTraceRole = ExpectedType,
                  typeTraceOrigin = ConstraintTypeOrigin (CaseBranchOrigin expectedBranchSp)
                }
              ev
              (CaseBranchOrigin branchSp)
              rhsSp
          rhs'' = attachRhsFailure report rhsCt rhs'
      pure (CaseAlt altAnns pat rhs'', cts ++ [rhsCt])

inferLet :: TcSolveReport -> SourceSpan -> [Decl] -> Expr -> TcM (Expr, [Ct], TcType)
inferLet report sp decls body = do
  (decls', body', bodyTy, bodyCts) <-
    inferLocalDecls report decls $
      inferExprSyntaxAt report sp body
  pure (ELetDecls decls' body', bodyCts, bodyTy)

inferTuple :: TcSolveReport -> SourceSpan -> TupleFlavor -> [Maybe Expr] -> TcM (Expr, [Ct], TcType)
inferTuple report sp flavor elems = do
  results <- mapM inferElem elems
  let elems' = map (\(elem', _, _) -> elem') results
      tys = map (\(_, _, ty) -> ty) results
      cts = concatMap (\(_, elemCts, _) -> elemCts) results
      n = length tys
      tc = TyCon {tyConName = "(" <> T.replicate (n - 1) "," <> ")", tyConArity = n}
      tupleTy = TcTyCon tc tys
      elaboration =
        OccurrenceElaboration
          { occurrenceElabType = tupleTy,
            occurrenceElabTypeArgs = tys,
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
  pure (attachExprElaboration report elaboration (ETuple flavor elems'), cts, tupleTy)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (Nothing, [], ty)
    inferElem (Just elemExpr) = do
      (elemExpr', elemCts, elemTy) <- inferExprAt report sp elemExpr
      pure (Just elemExpr', elemCts, elemTy)

sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    sp : _ -> sp

combineSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
combineSourceSpan NoSourceSpan fallback = fallback
combineSourceSpan span' _ = span'

inferList :: TcSolveReport -> SourceSpan -> [Expr] -> TcM (Expr, [Ct], TcType)
inferList report _sp [] = do
  elemTy <- freshMetaTv
  let listTy = TcTyCon listTyCon' [elemTy]
      elaboration =
        OccurrenceElaboration
          { occurrenceElabType = listTy,
            occurrenceElabTypeArgs = [elemTy],
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
  pure (attachExprElaboration report elaboration (EList []), [], listTy)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
inferList report sp (headExpr : tailExprs) = do
  (headExpr', headCts, headTy) <- inferExprAt report sp headExpr
  tailResults <- mapM (inferTailElem report) tailExprs
  eqCts <- mapM (mkElemEq (exprSpan headExpr `orSourceSpan` sp) headTy) tailResults
  let tailExprs' = zipWith attachElemFailure eqCts tailResults
      tailCts = concatMap (\(TailElem _ elemCts _ _) -> elemCts) tailResults
      listTy = TcTyCon listTyCon' [headTy]
      elaboration =
        OccurrenceElaboration
          { occurrenceElabType = listTy,
            occurrenceElabTypeArgs = [headTy],
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
  pure (attachExprElaboration report elaboration (EList (headExpr' : tailExprs')), headCts ++ tailCts ++ eqCts, listTy)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

    inferTailElem report' elemExpr = do
      (elemExpr', elemCts, elemTy) <- inferExprAt report' sp elemExpr
      pure (TailElem elemExpr' elemCts elemTy (exprSpan elemExpr `orSourceSpan` sp))

    mkElemEq headSp headTy (TailElem _ _ elemTy elemSp) = do
      ev <- freshEvVar
      pure $
        mkWantedEqCtAt
          TypeTrace
            { typeTraceType = elemTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin elemSp
            }
          TypeTrace
            { typeTraceType = headTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ListElementTypeOrigin headSp
            }
          ev
          (AppOrigin elemSp)
          elemSp

    attachElemFailure ct (TailElem elemExpr _ _ _) =
      attachExprFailure report ct elemExpr

inferRhs :: TcSolveReport -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhs report rhs =
  case rhs of
    UnguardedRhs anns expr Nothing -> do
      (expr', cts, ty) <- inferExpr report expr
      pure (UnguardedRhs anns expr' Nothing, ty, cts)
    UnguardedRhs anns expr (Just decls) -> do
      (decls', expr', ty, cts) <-
        inferLocalDecls report decls $
          inferExprSyntax report expr
      pure (UnguardedRhs anns expr' (Just decls'), ty, cts)
    GuardedRhss anns guardedRhss Nothing -> do
      (guardedRhss', ty, cts) <- inferGuardedRhss report (sourceSpanFromAnns anns) guardedRhss
      pure (GuardedRhss anns guardedRhss' Nothing, ty, cts)
    GuardedRhss anns guardedRhss (Just decls) -> do
      (decls', guardedRhss', ty, cts) <-
        inferLocalDecls report decls $
          inferGuardedRhss report (sourceSpanFromAnns anns) guardedRhss
      pure (GuardedRhss anns guardedRhss' (Just decls'), ty, cts)

inferExprSyntax :: TcSolveReport -> Expr -> TcM (Expr, TcType, [Ct])
inferExprSyntax report expr = do
  (expr', cts, ty) <- inferExpr report expr
  pure (expr', ty, cts)

inferExprSyntaxAt :: TcSolveReport -> SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferExprSyntaxAt report sp expr = do
  (expr', cts, ty) <- inferExprAt report sp expr
  pure (expr', ty, cts)

inferLocalDecls :: TcSolveReport -> [Decl] -> TcM (a, TcType, [Ct]) -> TcM ([Decl], a, TcType, [Ct])
inferLocalDecls report decls body = do
  plan <- prepareLocalDeclPlan decls
  withLocalDeclPlaceholders plan $ do
    groupResults <-
      mapM
        (inferLocalGroup report (localDeclPlanSigs plan) (localDeclPlanPlaceholders plan))
        (localDeclPlanGroups plan)
    let bindingCts = concatMap localGroupCts groupResults
        updates = mconcat (map localGroupUpdates groupResults)
    if localDeclPlanShouldGeneralize plan
      then do
        polyBinders <- generalizeLocalDeclPlan plan bindingCts
        decls' <- applyLocalDeclUpdates report (localBinderTypes polyBinders) updates decls
        withLocalBinders polyBinders $ do
          (value, bodyTy, bodyCts) <- body
          pure (decls', value, bodyTy, bodyCts)
      else do
        monoBinders <- monomorphicLocalDeclPlan plan
        decls' <- applyLocalDeclUpdates report (localBinderTypes monoBinders) updates decls
        (value, bodyTy, bodyCts) <- withLocalBinders monoBinders body
        pure (decls', value, bodyTy, bindingCts ++ bodyCts)

data LocalGroupResult = LocalGroupResult
  { localGroupCts :: ![Ct],
    localGroupUpdates :: !LocalDeclUpdates
  }

data LocalDeclUpdates = LocalDeclUpdates
  { localFunctionUpdates :: !(Map Text [[Match]]),
    localPatternUpdates :: !(Map Text [Rhs Expr]),
    localAnonymousPatternUpdates :: ![Rhs Expr]
  }

instance Semigroup LocalDeclUpdates where
  left <> right =
    LocalDeclUpdates
      { localFunctionUpdates = Map.unionWith (<>) (localFunctionUpdates left) (localFunctionUpdates right),
        localPatternUpdates = Map.unionWith (<>) (localPatternUpdates left) (localPatternUpdates right),
        localAnonymousPatternUpdates = localAnonymousPatternUpdates left <> localAnonymousPatternUpdates right
      }

instance Monoid LocalDeclUpdates where
  mempty =
    LocalDeclUpdates
      { localFunctionUpdates = Map.empty,
        localPatternUpdates = Map.empty,
        localAnonymousPatternUpdates = []
      }

inferLocalGroup :: TcSolveReport -> Map Text TypeScheme -> Map Text TcType -> DeclGroup -> TcM LocalGroupResult
inferLocalGroup report sigs placeholders group =
  case group of
    MergedFunctionBind name matches -> do
      (ty, matches', cts) <- inferLocalFunction report sigs placeholders name matches
      cts' <- tiePlaceholder placeholders name ty cts
      pure (LocalGroupResult cts' mempty {localFunctionUpdates = Map.singleton name [matches']})
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (PatternBind _ pat rhs) ->
          case patternBinderName pat of
            Just (_displayName, name) -> do
              (rhs', ty, cts) <- inferLocalPatternBind report sigs placeholders name rhs
              cts' <- tiePlaceholder placeholders name ty cts
              pure (LocalGroupResult cts' mempty {localPatternUpdates = Map.singleton name [rhs']})
            Nothing -> do
              (rhs', _ty, cts) <- inferRhs report rhs
              pure (LocalGroupResult cts mempty {localAnonymousPatternUpdates = [rhs']})
        DeclValue (FunctionBind name matches) -> do
          (ty, matches', cts) <- inferLocalFunction report sigs placeholders (unqualifiedNameText name) matches
          cts' <- tiePlaceholder placeholders (unqualifiedNameText name) ty cts
          pure (LocalGroupResult cts' mempty {localFunctionUpdates = Map.singleton (unqualifiedNameText name) [matches']})
        _ -> pure (LocalGroupResult [] mempty)

inferLocalFunction :: TcSolveReport -> Map Text TypeScheme -> Map Text TcType -> Text -> [Match] -> TcM (TcType, [Match], [Ct])
inferLocalFunction report sigs placeholders name matches =
  case Map.lookup name sigs of
    Just scheme -> do
      sigTy <- maybe (skolemize scheme) pure (Map.lookup name placeholders)
      let nArgs =
            case matches of
              m : _ -> length (matchPats m)
              [] -> 0
          (argTys, resTy) = splitFunTy sigTy nArgs
      results <- mapM (tcLocalMatchEquation report argTys resTy) matches
      let matches' = map fst results
          cts = concatMap snd results
      pure (sigTy, matches', cts)
    Nothing ->
      tcLocalMatches report matches

inferLocalPatternBind :: TcSolveReport -> Map Text TypeScheme -> Map Text TcType -> Text -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferLocalPatternBind report sigs placeholders name rhs = do
  (rhs', rhsTy, rhsCts) <- inferRhs report rhs
  ty <-
    case Map.lookup name sigs of
      Just scheme -> maybe (skolemize scheme) pure (Map.lookup name placeholders)
      Nothing -> pure rhsTy
  pure (rhs', ty, rhsCts)

tcLocalMatches :: TcSolveReport -> [Match] -> TcM (TcType, [Match], [Ct])
tcLocalMatches _ [] = do
  ty <- freshMetaTv
  pure (ty, [], [])
tcLocalMatches report matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      (rhs0', ty0, cts0) <- inferRhs report (matchRhs m0)
      restResults <- mapM (unifyLocalMatchRhs report ty0) (drop 1 matches)
      let m0' = m0 {matchRhs = rhs0'}
          restMatches = map fst restResults
          restCts = concatMap snd restResults
      pure (ty0, m0' : restMatches, cts0 ++ restCts)
    else do
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      results <- mapM (tcLocalMatchEquation report argTys resTy) matches
      let matches' = map fst results
          cts = concatMap snd results
      pure (foldr TcFunTy resTy argTys, matches', cts)

tcLocalMatchEquation :: TcSolveReport -> [TcType] -> TcType -> Match -> TcM (Match, [Ct])
tcLocalMatchEquation report argTys resTy match = do
  let pats = matchPats match
  patCheck <- checkPatterns NoSourceSpan (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs report (matchRhs match))
  ev <- freshEvVar
  resCt <- mkWantedCtM (EqPred rhsTy resTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  let pats' = zipWith (annotatePatternBindingWithReport report) argTys pats
      rhs'' = attachRhsFailure report resCt rhs'
  pure (match {matchPats = pats', matchRhs = rhs''}, pcWantedCts patCheck ++ rhsCts ++ [resCt])

unifyLocalMatchRhs :: TcSolveReport -> TcType -> Match -> TcM (Match, [Ct])
unifyLocalMatchRhs report expectedTy match = do
  (rhs', rhsTy, rhsCts) <- inferRhs report (matchRhs match)
  ev <- freshEvVar
  eqCt <- mkWantedCtM (EqPred rhsTy expectedTy) ev (AppOrigin NoSourceSpan) NoSourceSpan
  pure (match {matchRhs = attachRhsFailure report eqCt rhs'}, rhsCts ++ [eqCt])

applyLocalDeclUpdates :: TcSolveReport -> Map Text TcType -> LocalDeclUpdates -> [Decl] -> TcM [Decl]
applyLocalDeclUpdates report binderTypes updates decls = do
  (updates', reversedDecls) <-
    foldM step (updates, []) decls
  unless (nullLocalDeclUpdates updates') $
    abortTc ("internal type annotation error: unapplied local declaration annotations " <> showLocalDeclUpdates updates')
  pure (reverse reversedDecls)
  where
    step (updatesAcc, declsAcc) decl = do
      (updatesNext, decl') <- applyLocalDeclUpdate report binderTypes updatesAcc decl
      pure (updatesNext, decl' : declsAcc)

applyLocalDeclUpdate :: TcSolveReport -> Map Text TcType -> LocalDeclUpdates -> Decl -> TcM (LocalDeclUpdates, Decl)
applyLocalDeclUpdate report binderTypes updates decl =
  case decl of
    DeclAnn ann inner -> do
      (updates', inner') <- applyLocalDeclUpdate report binderTypes updates inner
      pure (updates', DeclAnn ann inner')
    DeclValue (FunctionBind name matches) ->
      let key = unqualifiedNameText name
       in do
            maybeUpdate <- takeLocalFunctionMatches key (length matches) updates
            case maybeUpdate of
              Nothing ->
                abortTc ("internal type annotation error: missing local function update for " <> T.unpack key)
              Just (updates', matches') -> do
                let decl' = DeclValue (FunctionBind name matches')
                pure (updates', maybe decl' (\ty -> annotateDeclWithTypeWithReport report ty decl') (Map.lookup key binderTypes))
    DeclValue (PatternBind multiplicity pat _rhs) ->
      case takeLocalPatternRhs pat updates of
        Nothing ->
          abortTc "internal type annotation error: missing local pattern update"
        Just (updates', rhs') -> do
          let decl' = DeclValue (PatternBind multiplicity pat rhs')
              maybeTy = patternBinderName pat >>= (`Map.lookup` binderTypes) . snd
          pure (updates', maybe decl' (\ty -> annotateDeclWithTypeWithReport report ty decl') maybeTy)
    _ -> pure (updates, decl)

takeLocalFunctionMatches :: Text -> Int -> LocalDeclUpdates -> TcM (Maybe (LocalDeclUpdates, [Match]))
takeLocalFunctionMatches name count updates =
  case Map.lookup name (localFunctionUpdates updates) of
    Nothing -> pure Nothing
    Just functionUpdates ->
      case functionUpdates of
        [] -> pure Nothing
        matches : restUpdates ->
          if length matches < count
            then abortTc ("internal type annotation error: not enough updated local matches for " <> T.unpack name)
            else
              let (here, remainingMatches) = splitAt count matches
                  remainingUpdate = [remainingMatches | not (null remainingMatches)]
                  remaining = remainingUpdate <> restUpdates
                  updates' =
                    updates
                      { localFunctionUpdates =
                          if null remaining
                            then Map.delete name (localFunctionUpdates updates)
                            else Map.insert name remaining (localFunctionUpdates updates)
                      }
               in pure (Just (updates', here))

takeLocalPatternRhs :: Pattern -> LocalDeclUpdates -> Maybe (LocalDeclUpdates, Rhs Expr)
takeLocalPatternRhs pat updates =
  case patternBinderName pat of
    Just (_, name) ->
      case Map.lookup name (localPatternUpdates updates) of
        Just (rhs : rest) ->
          let updates' =
                updates
                  { localPatternUpdates =
                      if null rest
                        then Map.delete name (localPatternUpdates updates)
                        else Map.insert name rest (localPatternUpdates updates)
                  }
           in Just (updates', rhs)
        _ -> Nothing
    Nothing ->
      case localAnonymousPatternUpdates updates of
        [] -> Nothing
        rhs : rest ->
          Just (updates {localAnonymousPatternUpdates = rest}, rhs)

nullLocalDeclUpdates :: LocalDeclUpdates -> Bool
nullLocalDeclUpdates updates =
  Map.null (localFunctionUpdates updates)
    && Map.null (localPatternUpdates updates)
    && null (localAnonymousPatternUpdates updates)

showLocalDeclUpdates :: LocalDeclUpdates -> String
showLocalDeclUpdates updates =
  "function updates="
    <> show (Map.keys (localFunctionUpdates updates))
    <> ", pattern updates="
    <> show (Map.keys (localPatternUpdates updates), length (localAnonymousPatternUpdates updates))

inferGuardedRhss :: TcSolveReport -> SourceSpan -> [GuardedRhs Expr] -> TcM ([GuardedRhs Expr], TcType, [Ct])
inferGuardedRhss _report _sp [] = do
  ty <- freshMetaTv
  pure ([], ty, [])
inferGuardedRhss report sp (first : rest) = do
  (first', firstTy, firstCts) <- inferGuardedRhs report sp first
  restResults <- mapM (inferGuardedRhsAgainst report sp firstTy) rest
  let rest' = map (\(guardedRhs, _, _) -> guardedRhs) restResults
      restCts = concatMap (\(_, _, cts) -> cts) restResults
  pure (first' : rest', firstTy, firstCts ++ restCts)

inferGuardedRhs :: TcSolveReport -> SourceSpan -> GuardedRhs Expr -> TcM (GuardedRhs Expr, TcType, [Ct])
inferGuardedRhs report sp guardedRhs = do
  (guards', body', cts, bodyTy) <-
    inferGuardQuals report sp (guardedRhsGuards guardedRhs) $
      inferExprAt report sp (guardedRhsBody guardedRhs)
  pure (guardedRhs {guardedRhsGuards = guards', guardedRhsBody = body'}, bodyTy, cts)

inferGuardedRhsAgainst :: TcSolveReport -> SourceSpan -> TcType -> GuardedRhs Expr -> TcM (GuardedRhs Expr, TcType, [Ct])
inferGuardedRhsAgainst report sp expectedTy guardedRhs = do
  (guardedRhs', bodyTy, bodyCts) <- inferGuardedRhs report sp guardedRhs
  ev <- freshEvVar
  let bodyCt = mkWantedCtAt (EqPred bodyTy expectedTy) ev (AppOrigin sp) sp
      body' = attachExprFailure report bodyCt (guardedRhsBody guardedRhs')
  pure (guardedRhs' {guardedRhsBody = body'}, bodyTy, bodyCts ++ [bodyCt])

inferGuardQuals :: TcSolveReport -> SourceSpan -> [GuardQualifier] -> TcM (body, [Ct], TcType) -> TcM ([GuardQualifier], body, [Ct], TcType)
inferGuardQuals _report _sp [] action = do
  (body, cts, ty) <- action
  pure ([], body, cts, ty)
inferGuardQuals report sp (qual : rest) action =
  case qual of
    GuardAnn ann inner -> do
      (quals', body, cts, ty) <- inferGuardQuals report sp (inner : rest) action
      case quals' of
        inner' : rest' -> pure (GuardAnn ann inner' : rest', body, cts, ty)
        [] -> pure ([], body, cts, ty)
    GuardExpr guard -> do
      (guard', guardCts, guardTy) <- inferExprAt report sp guard
      ev <- freshEvVar
      let guardSp = exprSpan guard `orSourceSpan` sp
          guardCt = mkWantedCtAt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
      (rest', body, restCts, bodyTy) <- inferGuardQuals report sp rest action
      pure (GuardExpr (attachExprFailure report guardCt guard') : rest', body, guardCts ++ [guardCt] ++ restCts, bodyTy)
    GuardPat pat guardExpr -> do
      patTy <- freshMetaTv
      (guardExpr', guardCts, guardTy) <- inferExprAt report sp guardExpr
      patCheck <- checkPattern sp pat patTy
      ev <- freshEvVar
      let guardCt = mkWantedCtAt (EqPred guardTy patTy) ev (AppOrigin sp) sp
      (rest', body, restCts, bodyTy) <-
        withPatternBindings (pcBindings patCheck) $
          inferGuardQuals report sp rest action
      pure
        ( GuardPat pat (attachExprFailure report guardCt guardExpr') : rest',
          body,
          guardCts ++ pcWantedCts patCheck ++ [guardCt] ++ restCts,
          bodyTy
        )
    GuardLet decls -> do
      (decls', (rest', body), bodyTy, restCts) <-
        inferLocalDecls report decls $ do
          (rest', body, cts, ty) <- inferGuardQuals report sp rest action
          pure ((rest', body), ty, cts)
      pure (GuardLet decls' : rest', body, restCts, bodyTy)

inferListComp :: TcSolveReport -> SourceSpan -> Expr -> [CompStmt] -> TcM (Expr, [Ct], TcType)
inferListComp report sp body quals = do
  (quals', body', bodyTy, cts) <-
    inferCompQuals report sp quals (inferExprAt report sp body)
  pure (EListComp body' quals', cts, listType bodyTy)
  where
    listType elemTy = TcTyCon listTyCon' [elemTy]
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

inferCompQuals :: TcSolveReport -> SourceSpan -> [CompStmt] -> TcM (Expr, [Ct], TcType) -> TcM ([CompStmt], Expr, TcType, [Ct])
inferCompQuals _report _ambient [] action = do
  (body, bodyCts, bodyTy) <- action
  pure ([], body, bodyTy, bodyCts)
inferCompQuals report ambient (qual : rest) action =
  case qual of
    CompAnn ann inner -> do
      (quals', body, bodyTy, cts) <- inferCompQuals report (compStmtSpan qual `orSourceSpan` ambient) (inner : rest) action
      case quals' of
        inner' : rest' -> pure (CompAnn ann inner' : rest', body, bodyTy, cts)
        [] -> pure ([], body, bodyTy, cts)
    CompGen pat src -> do
      elemTy <- freshMetaTv
      (src', srcCts, srcTy) <- inferExprAt report ambient src
      patCheck <- checkPattern ambient pat elemTy
      ev <- freshEvVar
      let srcSp = exprSpan src `orSourceSpan` ambient
          srcListCt = mkWantedCtAt (EqPred srcTy (listType elemTy)) ev (AppOrigin srcSp) srcSp
      (rest', body, bodyTy, bodyCts) <-
        withPatternBindings (pcBindings patCheck) $
          inferCompQuals report ambient rest action
      pure
        ( CompGen (annotatePatternBindingWithReport report elemTy pat) (attachExprFailure report srcListCt src') : rest',
          body,
          bodyTy,
          srcCts ++ pcWantedCts patCheck ++ [srcListCt] ++ bodyCts
        )
    CompGuard guard -> do
      (guard', guardCts, guardTy) <- inferExprAt report ambient guard
      ev <- freshEvVar
      let guardSp = exprSpan guard `orSourceSpan` ambient
          guardCt = mkWantedCtAt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
      (rest', body, bodyTy, bodyCts) <- inferCompQuals report ambient rest action
      pure
        ( CompGuard (attachExprFailure report guardCt guard') : rest',
          body,
          bodyTy,
          guardCts ++ [guardCt] ++ bodyCts
        )
    CompLetDecls decls -> do
      (decls', (rest', body), bodyTy, bodyCts) <-
        inferLocalDecls report decls $ do
          (rest', body, bodyTy, cts) <- inferCompQuals report ambient rest action
          pure ((rest', body), bodyTy, cts)
      pure (CompLetDecls decls' : rest', body, bodyTy, bodyCts)
    CompThen expr -> unsupportedCompQual report ambient rest action qual (Just expr)
    CompThenBy expr byExpr -> unsupportedCompQual report ambient rest action qual (Just (EApp expr byExpr))
    CompGroupUsing expr -> unsupportedCompQual report ambient rest action qual (Just expr)
    CompGroupByUsing byExpr usingExpr -> unsupportedCompQual report ambient rest action qual (Just (EApp usingExpr byExpr))
  where
    listType elemTy = TcTyCon listTyCon' [elemTy]
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

unsupportedCompQual :: TcSolveReport -> SourceSpan -> [CompStmt] -> TcM (Expr, [Ct], TcType) -> CompStmt -> Maybe Expr -> TcM ([CompStmt], Expr, TcType, [Ct])
unsupportedCompQual report ambient rest action qual _maybeExpr = do
  let qualSp = compStmtSpan qual `orSourceSpan` ambient
  diagnostic <- emitErrorDiagnostic qualSp (OtherError ("unsupported list comprehension qualifier in TC MVP: " ++ take 50 (show qual)))
  (rest', body, bodyTy, bodyCts) <- inferCompQuals report ambient rest action
  pure (CompAnn (mkAnnotation diagnostic) qual : rest', body, bodyTy, bodyCts)

data TailElem = TailElem Expr [Ct] TcType SourceSpan

attachRhsFailure :: TcSolveReport -> Ct -> Rhs Expr -> Rhs Expr
attachRhsFailure report ct rhs =
  case rhs of
    UnguardedRhs anns expr maybeDecls ->
      UnguardedRhs anns (attachExprFailure report ct expr) maybeDecls
    GuardedRhss anns guardedRhss maybeDecls ->
      GuardedRhss anns (map (attachGuardedRhsFailure report ct) guardedRhss) maybeDecls

attachGuardedRhsFailure :: TcSolveReport -> Ct -> GuardedRhs Expr -> GuardedRhs Expr
attachGuardedRhsFailure report ct guardedRhs =
  guardedRhs {guardedRhsBody = attachExprFailure report ct (guardedRhsBody guardedRhs)}

attachExprFailures :: TcSolveReport -> [Ct] -> Expr -> Expr
attachExprFailures report cts expr =
  foldr (attachExprFailure report) expr cts

attachExprFailure :: TcSolveReport -> Ct -> Expr -> Expr
attachExprFailure report ct expr =
  case Map.lookup (ctEvVar ct) (tcSolveReportConstraintFailures report) of
    Nothing -> expr
    Just diagnostic -> attachDiagnosticToExpr diagnostic expr

attachDiagnosticToExpr :: TcDiagnostic -> Expr -> Expr
attachDiagnosticToExpr diagnostic =
  EAnn (mkAnnotation diagnostic)

restoreExprAnnotation :: Annotation -> Expr -> Expr
restoreExprAnnotation ann expr =
  case expr of
    EAnn generated inner
      | isGeneratedExprAnnotation generated ->
          EAnn generated (restoreExprAnnotation ann inner)
    _ -> EAnn ann expr

isGeneratedExprAnnotation :: Annotation -> Bool
isGeneratedExprAnnotation ann =
  isTcAnnotation ann || isTcDiagnostic ann
  where
    isTcAnnotation annotation =
      case fromAnnotation @TcAnnotation annotation of
        Just _ -> True
        Nothing -> False
    isTcDiagnostic annotation =
      case fromAnnotation @TcDiagnostic annotation of
        Just _ -> True
        Nothing -> False

numericLiteralExprType :: Expr -> TcType
numericLiteralExprType expr =
  case expr of
    EInt _ numericType _ -> numericLiteralType numericType
    _ -> intTyCon

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sp _ = sp

compStmtSpan :: CompStmt -> SourceSpan
compStmtSpan compStmt =
  case compStmt of
    CompAnn ann _ -> fromMaybe NoSourceSpan (fromAnnotation @SourceSpan ann)
    _ -> NoSourceSpan

rhsExprSpan :: Rhs Expr -> SourceSpan
rhsExprSpan rhs =
  case rhs of
    UnguardedRhs anns expr _ -> exprSpan expr `orSourceSpan` sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

exprSpan :: Expr -> SourceSpan
exprSpan expr =
  case expr of
    EAnn ann inner ->
      fromMaybe (exprSpan inner) (fromAnnotation @SourceSpan ann)
    _ -> NoSourceSpan

-- | Convert a surface Name to Text for lookup.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

-- | Built-in type constructors (MVP).
intTyCon :: TcType
intTyCon = TcTyCon (TyCon "Int" 0) []

intHashTyCon :: TcType
intHashTyCon = TcTyCon (TyCon "Int#" 0) []

wordHashTyCon :: TcType
wordHashTyCon = TcTyCon (TyCon "Word#" 0) []

numericLiteralType :: NumericType -> TcType
numericLiteralType numericType =
  case numericType of
    TInteger -> intTyCon
    TIntHash -> intHashTyCon
    TWordHash -> wordHashTyCon
    TInt8Hash -> primNumericTyCon "Int8#"
    TInt16Hash -> primNumericTyCon "Int16#"
    TInt32Hash -> primNumericTyCon "Int32#"
    TInt64Hash -> primNumericTyCon "Int64#"
    TWord8Hash -> primNumericTyCon "Word8#"
    TWord16Hash -> primNumericTyCon "Word16#"
    TWord32Hash -> primNumericTyCon "Word32#"
    TWord64Hash -> primNumericTyCon "Word64#"

primNumericTyCon :: Text -> TcType
primNumericTyCon name = TcTyCon (TyCon name 0) []

doubleTyCon :: TcType
doubleTyCon = TcTyCon (TyCon "Double" 0) []

charTyCon :: TcType
charTyCon = TcTyCon (TyCon "Char" 0) []

stringTyCon :: TcType
stringTyCon = TcTyCon (TyCon "[]" 1) [charTyCon]

boolTyCon :: TcType
boolTyCon = TcTyCon (TyCon "Bool" 0) []
