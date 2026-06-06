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
    inferExprWithReport,
    attachExprFailure,
    attachRhsFailure,
    inferRhsWithReport,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    CaseAlt (..),
    CompStmt (..),
    Decl,
    Expr (..),
    GuardQualifier (..),
    GuardedRhs (..),
    LambdaCaseAlt (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor,
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcDiagnostic, TcErrorKind (..))
import Aihc.Tc.Generate.Bind (inferLocalDecls, inferLocalDeclsWithResult, inferRhsWithLocals)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Monad
import Aihc.Tc.NameKey (nameOccurrenceKey, syntaxOccurrenceKey)
import Aihc.Tc.Types
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the inferred type and a list of wanted constraints.
inferExpr :: Expr -> TcM (TcType, [Ct])
inferExpr = inferExprAt NoSourceSpan

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
inferExprWithReport :: TcSolveReport -> Expr -> TcM (Expr, [Ct], TcType)
inferExprWithReport report =
  inferExprWithReportAt report NoSourceSpan

inferExprWithReportAt :: TcSolveReport -> SourceSpan -> Expr -> TcM (Expr, [Ct], TcType)
inferExprWithReportAt report ambient expr =
  case expr of
    EAnn ann inner -> do
      (inner', cts, ty) <- inferExprWithReportAt report (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
      pure (EAnn ann inner', cts, ty)
    EVar name ->
      inferVarWithReport (exprSpan expr `orSourceSpan` ambient) name
    EInt {} ->
      pure (expr, [], numericLiteralExprType expr)
    EFloat {} ->
      pure (expr, [], doubleTyCon)
    EChar {} ->
      pure (expr, [], charTyCon)
    EString {} ->
      pure (expr, [], stringTyCon)
    ELambdaPats pats body ->
      inferLambdaWithReport report (exprSpan expr `orSourceSpan` ambient) pats body
    ELambdaCase alts ->
      inferLambdaCaseWithReport report (exprSpan expr `orSourceSpan` ambient) alts
    ELambdaCases alts ->
      inferLambdaCasesWithReport report (exprSpan expr `orSourceSpan` ambient) alts
    EApp fun arg ->
      inferAppWithReport report (exprSpan expr `orSourceSpan` ambient) fun arg
    EInfix lhs op rhs ->
      inferInfixWithReport report (exprSpan expr `orSourceSpan` ambient) lhs op rhs
    EIf cond thenE elseE ->
      inferIfWithReport report (exprSpan expr `orSourceSpan` ambient) cond thenE elseE
    ECase scrutinee alts ->
      inferCaseWithReport report (exprSpan expr `orSourceSpan` ambient) scrutinee alts
    ELetDecls decls body ->
      inferLetWithReport report (exprSpan expr `orSourceSpan` ambient) decls body
    EParen inner -> do
      (inner', cts, ty) <- inferExprWithReportAt report (exprSpan expr `orSourceSpan` ambient) inner
      pure (EParen inner', cts, ty)
    ETypeSig inner ty -> do
      (inner', cts, innerTy) <- inferExprWithReportAt report (exprSpan expr `orSourceSpan` ambient) inner
      pure (ETypeSig inner' ty, cts, innerTy)
    ENegate inner -> do
      (inner', cts, innerTy) <- inferExprWithReportAt report (exprSpan expr `orSourceSpan` ambient) inner
      pure (ENegate inner', cts, innerTy)
    ETuple flavor elems ->
      inferTupleWithReport report (exprSpan expr `orSourceSpan` ambient) flavor elems
    EList elems ->
      inferListWithReport report (exprSpan expr `orSourceSpan` ambient) elems
    EListComp body quals ->
      inferListCompWithReport report (exprSpan expr `orSourceSpan` ambient) body quals
    other -> do
      diagnostic <- emitErrorDiagnostic (exprSpan expr `orSourceSpan` ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
      ty <- freshMetaTv
      pure (attachDiagnosticToExpr diagnostic expr, [], ty)

inferVarWithReport :: SourceSpan -> Name -> TcM (Expr, [Ct], TcType)
inferVarWithReport ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) `orSourceSpan` ambient
      name = nameToText nameSyntax
      maybeKey = nameOccurrenceKey nameSyntax
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      recordVarOccurrence maybeKey $
        OccurrenceElaboration
          { occurrenceElabType = instType inst,
            occurrenceElabTypeArgs = instTypeArgs inst,
            occurrenceElabEvidenceVars = map ctEvVar cts,
            occurrenceElabTermArgTypes = []
          }
      pure (EVar nameSyntax, cts, instType inst)
    Just (TcMonoIdBinder _ ty) -> do
      recordVarOccurrence maybeKey $
        OccurrenceElaboration
          { occurrenceElabType = ty,
            occurrenceElabTypeArgs = [],
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
      pure (EVar nameSyntax, [], ty)
    Nothing -> do
      diagnostic <- emitErrorDiagnostic sp (UnboundVariable (T.unpack name))
      ty <- freshMetaTv
      pure (attachDiagnosticToExpr diagnostic (EVar nameSyntax), [], ty)

inferLambdaWithReport :: TcSolveReport -> SourceSpan -> [Pattern] -> Expr -> TcM (Expr, [Ct], TcType)
inferLambdaWithReport report sp pats body = do
  argTys <- mapM (const freshMetaTv) pats
  patCheck <- checkPatterns sp (zip pats argTys)
  (body', bodyCts, bodyTy) <- withPatternBindings (pcBindings patCheck) (inferExprWithReportAt report sp body)
  let funTy = foldr TcFunTy bodyTy argTys
  recordOccurrenceElaboration
    lambdaOccurrenceKey
    OccurrenceElaboration
      { occurrenceElabType = funTy,
        occurrenceElabTypeArgs = [],
        occurrenceElabEvidenceVars = [],
        occurrenceElabTermArgTypes = argTys
      }
  pure (ELambdaPats pats body', pcWantedCts patCheck ++ bodyCts, funTy)

inferLambdaCaseWithReport :: TcSolveReport -> SourceSpan -> [CaseAlt Expr] -> TcM (Expr, [Ct], TcType)
inferLambdaCaseWithReport report sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  (alts', cts) <- inferCaseAltsWithReport report sp argTy resTy alts
  pure (ELambdaCase alts', cts, TcFunTy argTy resTy)

inferLambdaCasesWithReport :: TcSolveReport -> SourceSpan -> [LambdaCaseAlt] -> TcM (Expr, [Ct], TcType)
inferLambdaCasesWithReport report sp alts = do
  let arity = maximum (0 : map (length . lambdaCaseAltPats) alts)
  argTys <- mapM (const freshMetaTv) [1 .. arity]
  resTy <- freshMetaTv
  results <- mapM (inferLambdaCaseAltWithReport report sp argTys resTy) alts
  let alts' = map (\(alt, _, _) -> alt) results
      cts = concatMap (\(_, altCts, _) -> altCts) results
  pure (ELambdaCases alts', cts, foldr TcFunTy resTy argTys)

inferLambdaCaseAltWithReport :: TcSolveReport -> SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM (LambdaCaseAlt, [Ct], TcType)
inferLambdaCaseAltWithReport report sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkPatterns sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsWithReport report rhs)
  ev <- freshEvVar
  let rhsCt = mkWantedCtAt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
      rhs'' = attachRhsFailure report rhsCt rhs'
  pure (alt {lambdaCaseAltRhs = rhs''}, pcWantedCts patCheck ++ rhsCts ++ [rhsCt], resTy)

inferAppWithReport :: TcSolveReport -> SourceSpan -> Expr -> Expr -> TcM (Expr, [Ct], TcType)
inferAppWithReport report sp fun arg = do
  (fun', funCts, funTy) <- inferExprWithReportAt report sp fun
  (arg', argCts, argTy) <- inferExprWithReportAt report sp arg
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

inferInfixWithReport :: TcSolveReport -> SourceSpan -> Expr -> Name -> Expr -> TcM (Expr, [Ct], TcType)
inferInfixWithReport report sp lhs op rhs = do
  (opTy, opCts) <- inferVar sp op
  (lhs', lhsCts, lhsTy) <- inferExprWithReportAt report sp lhs
  leftResTy <- freshMetaTv
  leftEv <- freshEvVar
  leftCt <- mkWantedCtM (EqPred opTy (TcFunTy lhsTy leftResTy)) leftEv (AppOrigin sp) sp
  (rhs', rhsCts, rhsTy) <- inferExprWithReportAt report sp rhs
  resTy <- freshMetaTv
  rightEv <- freshEvVar
  rightCt <- mkWantedCtM (EqPred leftResTy (TcFunTy rhsTy resTy)) rightEv (AppOrigin sp) sp
  let rebuilt = EInfix lhs' op rhs'
  pure (attachExprFailures report [leftCt, rightCt] rebuilt, opCts ++ lhsCts ++ rhsCts ++ [leftCt, rightCt], resTy)

inferIfWithReport :: TcSolveReport -> SourceSpan -> Expr -> Expr -> Expr -> TcM (Expr, [Ct], TcType)
inferIfWithReport report sp cond thenE elseE = do
  (cond', condCts, condTy) <- inferExprWithReportAt report sp cond
  (thenE', thenCts, thenTy) <- inferExprWithReportAt report sp thenE
  (elseE', elseCts, elseTy) <- inferExprWithReportAt report sp elseE
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

inferCaseWithReport :: TcSolveReport -> SourceSpan -> Expr -> [CaseAlt Expr] -> TcM (Expr, [Ct], TcType)
inferCaseWithReport report sp scrutinee alts = do
  (scrutinee', scrutCts, scrutTy) <- inferExprWithReportAt report sp scrutinee
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAltsWithReport report sp scrutTy resTy alts
  pure (ECase scrutinee' alts', scrutCts ++ altCts, resTy)

inferCaseAltsWithReport :: TcSolveReport -> SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM ([CaseAlt Expr], [Ct])
inferCaseAltsWithReport _report _sp _scrutTy _resTy [] = pure ([], [])
inferCaseAltsWithReport report sp scrutTy resTy (firstAlt : restAlts) = do
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
      (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhsWithReport report rhs)
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

inferLetWithReport :: TcSolveReport -> SourceSpan -> [Decl] -> Expr -> TcM (Expr, [Ct], TcType)
inferLetWithReport report sp decls body = do
  (body', bodyTy, bodyCts) <-
    inferLocalDeclsWithResult (inferExprTypeWithReport report) decls $
      inferExprSyntaxWithReportAt report sp body
  pure (ELetDecls decls body', bodyCts, bodyTy)

inferTupleWithReport :: TcSolveReport -> SourceSpan -> TupleFlavor -> [Maybe Expr] -> TcM (Expr, [Ct], TcType)
inferTupleWithReport report sp flavor elems = do
  results <- mapM inferElem elems
  let elems' = map (\(elem', _, _) -> elem') results
      tys = map (\(_, _, ty) -> ty) results
      cts = concatMap (\(_, elemCts, _) -> elemCts) results
      n = length tys
      tc = TyCon {tyConName = "(" <> T.replicate (n - 1) "," <> ")", tyConArity = n}
      tupleTy = TcTyCon tc tys
  recordOccurrenceElaboration
    tupleOccurrenceKey
    OccurrenceElaboration
      { occurrenceElabType = tupleTy,
        occurrenceElabTypeArgs = tys,
        occurrenceElabEvidenceVars = [],
        occurrenceElabTermArgTypes = []
      }
  pure (ETuple flavor elems', cts, tupleTy)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (Nothing, [], ty)
    inferElem (Just elemExpr) = do
      (elemExpr', elemCts, elemTy) <- inferExprWithReportAt report sp elemExpr
      pure (Just elemExpr', elemCts, elemTy)

inferExprAt :: SourceSpan -> Expr -> TcM (TcType, [Ct])
inferExprAt ambient expr = case expr of
  -- Variables: look up in environment, instantiate if polymorphic.
  EVar name -> inferVar (exprSpan expr `orSourceSpan` ambient) name
  -- Boxed integer literals are monomorphic Int for the MVP. MagicHash
  -- literals keep their primitive, unboxed type.
  EInt _ numericType _ -> pure (numericLiteralType numericType, [])
  -- Float literals.
  EFloat {} -> pure (doubleTyCon, [])
  -- Char literals.
  EChar _ _ -> pure (charTyCon, [])
  -- String literals are lists of Char. The Prelude exposes
  -- @type String = [Char]@ at the source level, but FC should not see a
  -- primitive String type constructor.
  EString _ _ -> pure (stringTyCon, [])
  -- Lambda: \x -> body
  ELambdaPats pats body -> inferLambda (exprSpan expr `orSourceSpan` ambient) pats body
  -- Lambda case: \case { pat -> body; ... }
  ELambdaCase alts -> inferLambdaCase (exprSpan expr `orSourceSpan` ambient) alts
  -- Multi-argument lambda cases: \cases { p1 p2 -> body; ... }
  ELambdaCases alts -> inferLambdaCases (exprSpan expr `orSourceSpan` ambient) alts
  -- Application: f x
  EApp fun arg -> inferApp (exprSpan expr `orSourceSpan` ambient) fun arg
  -- Infix application: a `op` b
  EInfix lhs op rhs -> inferExprAt (exprSpan expr `orSourceSpan` ambient) (EApp (EApp (EVar op) lhs) rhs)
  -- If-then-else
  EIf cond thenE elseE -> inferIf (exprSpan expr `orSourceSpan` ambient) cond thenE elseE
  -- Case expression
  ECase scrutinee alts -> inferCase (exprSpan expr `orSourceSpan` ambient) scrutinee alts
  -- Let expression
  ELetDecls decls body ->
    inferLocalDecls inferExpr decls (inferExpr body)
  -- Parenthesized expression
  EParen inner -> inferExprAt (exprSpan expr `orSourceSpan` ambient) inner
  -- Type signature: (e :: T)
  ETypeSig inner _ty -> do
    -- MVP: just infer the inner expression.
    -- Full version would check against the given type.
    inferExprAt (exprSpan expr `orSourceSpan` ambient) inner
  -- Negation
  ENegate inner -> do
    (innerTy, cs) <- inferExpr inner
    -- For MVP, just return the inner type.
    pure (innerTy, cs)
  -- Annotated expression (from other passes, e.g. resolve).
  EAnn ann inner -> inferExprAt (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
  -- Tuple
  ETuple _flavor elems -> inferTuple (exprSpan expr `orSourceSpan` ambient) elems
  -- List
  EList elems -> inferList (exprSpan expr `orSourceSpan` ambient) elems
  -- List comprehension
  EListComp body quals -> inferListComp (exprSpan expr `orSourceSpan` ambient) body quals
  -- Unsupported expression forms for MVP.
  other -> do
    emitError (exprSpan expr `orSourceSpan` ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (ty, [])

-- | Infer the type of a variable reference.
inferVar :: SourceSpan -> Name -> TcM (TcType, [Ct])
inferVar ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) `orSourceSpan` ambient
      name = nameToText nameSyntax
      maybeKey = nameOccurrenceKey nameSyntax
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      recordVarOccurrence maybeKey $
        OccurrenceElaboration
          { occurrenceElabType = instType inst,
            occurrenceElabTypeArgs = instTypeArgs inst,
            occurrenceElabEvidenceVars = map ctEvVar cts,
            occurrenceElabTermArgTypes = []
          }
      let ty = instType inst
      pure (ty, cts)
    Just (TcMonoIdBinder _ ty) -> do
      recordVarOccurrence maybeKey $
        OccurrenceElaboration
          { occurrenceElabType = ty,
            occurrenceElabTypeArgs = [],
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
      pure (ty, [])
    Nothing -> do
      emitError sp (UnboundVariable (T.unpack name))
      ty <- freshMetaTv
      pure (ty, [])

recordVarOccurrence :: Maybe OccurrenceKey -> OccurrenceElaboration -> TcM ()
recordVarOccurrence maybeKey elaboration =
  case maybeKey of
    Just key -> recordOccurrenceElaboration key elaboration
    Nothing -> pure ()

-- | Convert a predicate to a wanted constraint.
predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  mkWantedCtM p ev (OccurrenceOf name) sp

-- | Infer the type of a lambda expression.
inferLambda :: SourceSpan -> [Pattern] -> Expr -> TcM (TcType, [Ct])
inferLambda sp pats body = do
  -- Create a fresh meta-variable for each pattern and bind pattern
  -- variables into the environment.
  argTys <- mapM (const freshMetaTv) pats
  patCheck <- checkPatterns sp (zip pats argTys)
  -- Infer the body under the extended environment.
  (bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferExpr body)
  let funTy = foldr TcFunTy bodyTy argTys
  recordOccurrenceElaboration
    lambdaOccurrenceKey
    OccurrenceElaboration
      { occurrenceElabType = funTy,
        occurrenceElabTypeArgs = [],
        occurrenceElabEvidenceVars = [],
        occurrenceElabTermArgTypes = argTys
      }
  pure (funTy, pcWantedCts patCheck ++ bodyCts)

-- | Infer the type of a lambda-case expression.
--
-- @\\case { pat1 -> e1; pat2 -> e2; ... }@ is treated as a function
-- from a fresh argument type to the result type of the case
-- alternatives.
inferLambdaCase :: SourceSpan -> [CaseAlt Expr] -> TcM (TcType, [Ct])
inferLambdaCase sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  cts <- inferCaseAlts sp argTy resTy alts
  pure (TcFunTy argTy resTy, cts)

-- | Infer the type of a case expression.
--
-- @case scrutinee of { pat1 -> e1; pat2 -> e2; ... }@ is inferred by
-- checking each alternative against the scrutinee type and unifying all
-- branch result types with a fresh result type.
inferCase :: SourceSpan -> Expr -> [CaseAlt Expr] -> TcM (TcType, [Ct])
inferCase sp scrutinee alts = do
  (scrutTy, scrutCts) <- inferExpr scrutinee
  resTy <- freshMetaTv
  altCts <- inferCaseAlts sp scrutTy resTy alts
  pure (resTy, scrutCts ++ altCts)

inferLambdaCases :: SourceSpan -> [LambdaCaseAlt] -> TcM (TcType, [Ct])
inferLambdaCases sp alts = do
  let arity = maximum (0 : map (length . lambdaCaseAltPats) alts)
  argTys <- mapM (const freshMetaTv) [1 .. arity]
  resTy <- freshMetaTv
  cts <- concat <$> mapM (inferLambdaCaseAlt sp argTys resTy) alts
  pure (foldr TcFunTy resTy argTys, cts)

-- | Infer constraints from case alternatives.
--
-- Each alternative's pattern is checked against the scrutinee type,
-- and each RHS must unify with the expected result type.
inferCaseAlts :: SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM [Ct]
inferCaseAlts _sp _scrutTy _resTy [] = pure []
inferCaseAlts sp scrutTy resTy (firstAlt : restAlts) = do
  (firstBranchSp, firstRhsSp, firstRhsTy, firstCts) <- inferAlt firstAlt
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
  restCts <- concat <$> mapM (inferAltAgainst firstBranchSp firstRhsTy) restAlts
  pure (firstCts ++ [resultCt] ++ restCts)
  where
    inferAlt (CaseAlt altAnns pat rhs) = do
      let altSp = sourceSpanFromAnns altAnns
          branchSp = combineSourceSpan altSp sp
      patCheck <- checkPattern branchSp pat scrutTy
      -- Infer the RHS under the pattern bindings.
      (rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
      let rhsSp = rhsExprSpan rhs `orSourceSpan` branchSp
      pure (branchSp, rhsSp, rhsTy, pcWantedCts patCheck ++ rhsCts)

    inferAltAgainst expectedBranchSp expectedTy alt = do
      (branchSp, rhsSp, rhsTy, cts) <- inferAlt alt
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
      pure (cts ++ [rhsCt])

inferLambdaCaseAlt :: SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM [Ct]
inferLambdaCaseAlt sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkPatterns sp (zip pats argTys)
  (rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
  ev <- freshEvVar
  let rhsCt = mkWantedCtAt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  pure (pcWantedCts patCheck ++ rhsCts ++ [rhsCt])

sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    sp : _ -> sp

combineSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
combineSourceSpan NoSourceSpan fallback = fallback
combineSourceSpan span' _ = span'

-- | Infer the type of a right-hand side (for case alternatives).
inferRhs :: Rhs Expr -> TcM (TcType, [Ct])
inferRhs = inferRhsWithLocals inferExpr

-- | Infer the type of a function application.
inferApp :: SourceSpan -> Expr -> Expr -> TcM (TcType, [Ct])
inferApp sp fun arg = do
  (funTy, funCts) <- inferExpr fun
  (argTy, argCts) <- inferExpr arg
  resTy <- freshMetaTv
  -- Emit equality: funTy ~ argTy -> resTy
  ev <- freshEvVar
  eqCt <- mkWantedCtM (EqPred funTy (TcFunTy argTy resTy)) ev (AppOrigin sp) sp
  pure (resTy, funCts ++ argCts ++ [eqCt])

-- | Infer the type of if-then-else.
inferIf :: SourceSpan -> Expr -> Expr -> Expr -> TcM (TcType, [Ct])
inferIf sp cond thenE elseE = do
  (condTy, condCts) <- inferExpr cond
  (thenTy, thenCts) <- inferExpr thenE
  (elseTy, elseCts) <- inferExpr elseE
  -- Condition must be Bool.
  condEv <- freshEvVar
  let condCt = mkWantedCtAt (EqPred condTy boolTyCon) condEv (AppOrigin sp) sp
  -- Then and else branches must have the same type.
  branchEv <- freshEvVar
  branchCt <- mkWantedCtM (EqPred thenTy elseTy) branchEv (AppOrigin sp) sp
  pure (thenTy, condCts ++ thenCts ++ elseCts ++ [condCt, branchCt])

-- | Infer the type of a tuple.
inferTuple :: SourceSpan -> [Maybe Expr] -> TcM (TcType, [Ct])
inferTuple _sp elems = do
  results <- mapM inferElem elems
  let tys = map fst results
  let cts = concatMap snd results
  -- Represent tuples as TcTyCon with a tuple type constructor.
  let n = length tys
  let tc = TyCon {tyConName = "(" <> T.replicate (n - 1) "," <> ")", tyConArity = n}
  let tupleTy = TcTyCon tc tys
  recordOccurrenceElaboration
    tupleOccurrenceKey
    OccurrenceElaboration
      { occurrenceElabType = tupleTy,
        occurrenceElabTypeArgs = tys,
        occurrenceElabEvidenceVars = [],
        occurrenceElabTermArgTypes = []
      }
  pure (tupleTy, cts)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (ty, [])
    inferElem (Just e) = inferExpr e

-- | Infer the type of a list literal.
inferList :: SourceSpan -> [Expr] -> TcM (TcType, [Ct])
inferList sp elems = case elems of
  [] -> do
    elemTy <- freshMetaTv
    let listTy = TcTyCon listTyCon' [elemTy]
    recordListElaboration listTy elemTy
    pure (listTy, [])
  (e : es) -> do
    let headSp = exprSpan e `orSourceSpan` sp
    (headTy, headCts) <- inferExpr e
    results <- mapM inferElem es
    let tailCts = concatMap (\(_, elemCts, _) -> elemCts) results
    -- All elements must have the same type.
    eqCts <- mapM (mkElemEq headSp headTy) results
    let listTy = TcTyCon listTyCon' [headTy]
    recordListElaboration listTy headTy
    pure (listTy, headCts ++ tailCts ++ eqCts)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
    inferElem elemExpr = do
      (elemTy, elemCts) <- inferExpr elemExpr
      pure (elemTy, elemCts, exprSpan elemExpr `orSourceSpan` sp)
    recordListElaboration listTy elemTy =
      recordOccurrenceElaboration
        listOccurrenceKey
        OccurrenceElaboration
          { occurrenceElabType = listTy,
            occurrenceElabTypeArgs = [elemTy],
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
    mkElemEq headSp headTy (elemTy, _, elemSp) = do
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

inferListWithReport :: TcSolveReport -> SourceSpan -> [Expr] -> TcM (Expr, [Ct], TcType)
inferListWithReport _report _sp [] = do
  elemTy <- freshMetaTv
  let listTy = TcTyCon listTyCon' [elemTy]
  recordListElaborationWithReport listTy elemTy
  pure (EList [], [], listTy)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
inferListWithReport report sp (headExpr : tailExprs) = do
  (headExpr', headCts, headTy) <- inferExprWithReportAt report sp headExpr
  tailResults <- mapM (inferTailElem report) tailExprs
  eqCts <- mapM (mkElemEq (exprSpan headExpr `orSourceSpan` sp) headTy) tailResults
  let tailExprs' = zipWith attachElemFailure eqCts tailResults
      tailCts = concatMap (\(TailElem _ elemCts _ _) -> elemCts) tailResults
      listTy = TcTyCon listTyCon' [headTy]
  recordListElaborationWithReport listTy headTy
  pure (EList (headExpr' : tailExprs'), headCts ++ tailCts ++ eqCts, listTy)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

    inferTailElem report' elemExpr = do
      (elemExpr', elemCts, elemTy) <- inferExprWithReportAt report' sp elemExpr
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

inferRhsWithReport :: TcSolveReport -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhsWithReport report rhs =
  case rhs of
    UnguardedRhs anns expr Nothing -> do
      (expr', cts, ty) <- inferExprWithReport report expr
      pure (UnguardedRhs anns expr' Nothing, ty, cts)
    UnguardedRhs anns expr (Just decls) -> do
      (expr', ty, cts) <-
        inferLocalDeclsWithResult (inferExprTypeWithReport report) decls $
          inferExprSyntaxWithReport report expr
      pure (UnguardedRhs anns expr' (Just decls), ty, cts)
    GuardedRhss anns guardedRhss Nothing -> do
      (guardedRhss', ty, cts) <- inferGuardedRhssWithReport report (sourceSpanFromAnns anns) guardedRhss
      pure (GuardedRhss anns guardedRhss' Nothing, ty, cts)
    GuardedRhss anns guardedRhss (Just decls) -> do
      (guardedRhss', ty, cts) <-
        inferLocalDeclsWithResult (inferExprTypeWithReport report) decls $
          inferGuardedRhssWithReport report (sourceSpanFromAnns anns) guardedRhss
      pure (GuardedRhss anns guardedRhss' (Just decls), ty, cts)

inferExprTypeWithReport :: TcSolveReport -> Expr -> TcM (TcType, [Ct])
inferExprTypeWithReport report expr = do
  (_expr', cts, ty) <- inferExprWithReport report expr
  pure (ty, cts)

inferExprSyntaxWithReport :: TcSolveReport -> Expr -> TcM (Expr, TcType, [Ct])
inferExprSyntaxWithReport report expr = do
  (expr', cts, ty) <- inferExprWithReport report expr
  pure (expr', ty, cts)

inferExprSyntaxWithReportAt :: TcSolveReport -> SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferExprSyntaxWithReportAt report sp expr = do
  (expr', cts, ty) <- inferExprWithReportAt report sp expr
  pure (expr', ty, cts)

inferGuardedRhssWithReport :: TcSolveReport -> SourceSpan -> [GuardedRhs Expr] -> TcM ([GuardedRhs Expr], TcType, [Ct])
inferGuardedRhssWithReport _report _sp [] = do
  ty <- freshMetaTv
  pure ([], ty, [])
inferGuardedRhssWithReport report sp (first : rest) = do
  (first', firstTy, firstCts) <- inferGuardedRhsWithReport report sp first
  restResults <- mapM (inferGuardedRhsAgainst report sp firstTy) rest
  let rest' = map (\(guardedRhs, _, _) -> guardedRhs) restResults
      restCts = concatMap (\(_, _, cts) -> cts) restResults
  pure (first' : rest', firstTy, firstCts ++ restCts)

inferGuardedRhsWithReport :: TcSolveReport -> SourceSpan -> GuardedRhs Expr -> TcM (GuardedRhs Expr, TcType, [Ct])
inferGuardedRhsWithReport report sp guardedRhs = do
  (guards', body', cts, bodyTy) <-
    inferGuardQualsWithReport report sp (guardedRhsGuards guardedRhs) $
      inferExprWithReportAt report sp (guardedRhsBody guardedRhs)
  pure (guardedRhs {guardedRhsGuards = guards', guardedRhsBody = body'}, bodyTy, cts)

inferGuardedRhsAgainst :: TcSolveReport -> SourceSpan -> TcType -> GuardedRhs Expr -> TcM (GuardedRhs Expr, TcType, [Ct])
inferGuardedRhsAgainst report sp expectedTy guardedRhs = do
  (guardedRhs', bodyTy, bodyCts) <- inferGuardedRhsWithReport report sp guardedRhs
  ev <- freshEvVar
  let bodyCt = mkWantedCtAt (EqPred bodyTy expectedTy) ev (AppOrigin sp) sp
      body' = attachExprFailure report bodyCt (guardedRhsBody guardedRhs')
  pure (guardedRhs' {guardedRhsBody = body'}, bodyTy, bodyCts ++ [bodyCt])

inferGuardQualsWithReport :: TcSolveReport -> SourceSpan -> [GuardQualifier] -> TcM (body, [Ct], TcType) -> TcM ([GuardQualifier], body, [Ct], TcType)
inferGuardQualsWithReport _report _sp [] action = do
  (body, cts, ty) <- action
  pure ([], body, cts, ty)
inferGuardQualsWithReport report sp (qual : rest) action =
  case qual of
    GuardAnn ann inner -> do
      (quals', body, cts, ty) <- inferGuardQualsWithReport report sp (inner : rest) action
      case quals' of
        inner' : rest' -> pure (GuardAnn ann inner' : rest', body, cts, ty)
        [] -> pure ([], body, cts, ty)
    GuardExpr guard -> do
      (guard', guardCts, guardTy) <- inferExprWithReportAt report sp guard
      ev <- freshEvVar
      let guardSp = exprSpan guard `orSourceSpan` sp
          guardCt = mkWantedCtAt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
      (rest', body, restCts, bodyTy) <- inferGuardQualsWithReport report sp rest action
      pure (GuardExpr (attachExprFailure report guardCt guard') : rest', body, guardCts ++ [guardCt] ++ restCts, bodyTy)
    GuardPat pat guardExpr -> do
      patTy <- freshMetaTv
      (guardExpr', guardCts, guardTy) <- inferExprWithReportAt report sp guardExpr
      patCheck <- checkPattern sp pat patTy
      ev <- freshEvVar
      let guardCt = mkWantedCtAt (EqPred guardTy patTy) ev (AppOrigin sp) sp
      (rest', body, restCts, bodyTy) <-
        withPatternBindings (pcBindings patCheck) $
          inferGuardQualsWithReport report sp rest action
      pure
        ( GuardPat pat (attachExprFailure report guardCt guardExpr') : rest',
          body,
          guardCts ++ pcWantedCts patCheck ++ [guardCt] ++ restCts,
          bodyTy
        )
    GuardLet decls -> do
      ((rest', body), bodyTy, restCts) <-
        inferLocalDeclsWithResult (inferExprTypeWithReport report) decls $ do
          (rest', body, cts, ty) <- inferGuardQualsWithReport report sp rest action
          pure ((rest', body), ty, cts)
      pure (GuardLet decls : rest', body, restCts, bodyTy)

inferListCompWithReport :: TcSolveReport -> SourceSpan -> Expr -> [CompStmt] -> TcM (Expr, [Ct], TcType)
inferListCompWithReport report sp body quals = do
  (quals', body', bodyTy, cts) <-
    inferCompQualsWithReport report sp quals (inferExprWithReportAt report sp body)
  pure (EListComp body' quals', cts, listType bodyTy)
  where
    listType elemTy = TcTyCon listTyCon' [elemTy]
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

inferCompQualsWithReport :: TcSolveReport -> SourceSpan -> [CompStmt] -> TcM (Expr, [Ct], TcType) -> TcM ([CompStmt], Expr, TcType, [Ct])
inferCompQualsWithReport _report _ambient [] action = do
  (body, bodyCts, bodyTy) <- action
  pure ([], body, bodyTy, bodyCts)
inferCompQualsWithReport report ambient (qual : rest) action =
  case qual of
    CompAnn ann inner -> do
      (quals', body, bodyTy, cts) <- inferCompQualsWithReport report (compStmtSpan qual `orSourceSpan` ambient) (inner : rest) action
      case quals' of
        inner' : rest' -> pure (CompAnn ann inner' : rest', body, bodyTy, cts)
        [] -> pure ([], body, bodyTy, cts)
    CompGen pat src -> do
      elemTy <- freshMetaTv
      (src', srcCts, srcTy) <- inferExprWithReportAt report ambient src
      patCheck <- checkPattern ambient pat elemTy
      ev <- freshEvVar
      let srcSp = exprSpan src `orSourceSpan` ambient
          srcListCt = mkWantedCtAt (EqPred srcTy (listType elemTy)) ev (AppOrigin srcSp) srcSp
      (rest', body, bodyTy, bodyCts) <-
        withPatternBindings (pcBindings patCheck) $
          inferCompQualsWithReport report ambient rest action
      pure
        ( CompGen pat (attachExprFailure report srcListCt src') : rest',
          body,
          bodyTy,
          srcCts ++ pcWantedCts patCheck ++ [srcListCt] ++ bodyCts
        )
    CompGuard guard -> do
      (guard', guardCts, guardTy) <- inferExprWithReportAt report ambient guard
      ev <- freshEvVar
      let guardSp = exprSpan guard `orSourceSpan` ambient
          guardCt = mkWantedCtAt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
      (rest', body, bodyTy, bodyCts) <- inferCompQualsWithReport report ambient rest action
      pure
        ( CompGuard (attachExprFailure report guardCt guard') : rest',
          body,
          bodyTy,
          guardCts ++ [guardCt] ++ bodyCts
        )
    CompLetDecls decls -> do
      ((rest', body), bodyTy, bodyCts) <-
        inferLocalDeclsWithResult (inferExprTypeWithReport report) decls $ do
          (rest', body, bodyTy, cts) <- inferCompQualsWithReport report ambient rest action
          pure ((rest', body), bodyTy, cts)
      pure (CompLetDecls decls : rest', body, bodyTy, bodyCts)
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
  _ <- emitErrorDiagnostic qualSp (OtherError ("unsupported list comprehension qualifier in TC MVP: " ++ take 50 (show qual)))
  (rest', body, bodyTy, bodyCts) <- inferCompQualsWithReport report ambient rest action
  pure (qual : rest', body, bodyTy, bodyCts)

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

recordListElaborationWithReport :: TcType -> TcType -> TcM ()
recordListElaborationWithReport listTy elemTy =
  recordOccurrenceElaboration
    listOccurrenceKey
    OccurrenceElaboration
      { occurrenceElabType = listTy,
        occurrenceElabTypeArgs = [elemTy],
        occurrenceElabEvidenceVars = [],
        occurrenceElabTermArgTypes = []
      }

numericLiteralExprType :: Expr -> TcType
numericLiteralExprType expr =
  case expr of
    EInt _ numericType _ -> numericLiteralType numericType
    _ -> intTyCon

tupleOccurrenceKey :: OccurrenceKey
tupleOccurrenceKey = syntaxOccurrenceKey "$tuple"

listOccurrenceKey :: OccurrenceKey
listOccurrenceKey = syntaxOccurrenceKey "$list"

lambdaOccurrenceKey :: OccurrenceKey
lambdaOccurrenceKey = syntaxOccurrenceKey "$lambda"

-- | Infer the type of a list comprehension.
--
-- A generator @pat <- xs@ constrains @xs@ to be a list of the pattern
-- type and brings the pattern variables into scope for later qualifiers
-- and the body. A guard must be 'Bool'. A @let@ qualifier behaves like an
-- expression-local @let@ over the remaining qualifiers and body.
inferListComp :: SourceSpan -> Expr -> [CompStmt] -> TcM (TcType, [Ct])
inferListComp sp body quals = do
  (bodyTy, cts) <- inferCompQuals sp quals (inferExpr body)
  pure (listType bodyTy, cts)
  where
    listType elemTy = TcTyCon listTyCon' [elemTy]
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

    inferCompQuals _ [] action = action
    inferCompQuals ambient (qual : rest) action =
      case qual of
        CompAnn _ inner ->
          inferCompQuals (compStmtSpan qual `orSourceSpan` ambient) (inner : rest) action
        CompGen pat src -> do
          elemTy <- freshMetaTv
          (srcTy, srcCts) <- inferExpr src
          patCheck <- checkPattern ambient pat elemTy
          ev <- freshEvVar
          let srcSp = exprSpan src `orSourceSpan` ambient
              srcListCt = mkWantedCtAt (EqPred srcTy (listType elemTy)) ev (AppOrigin srcSp) srcSp
          (bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferCompQuals ambient rest action)
          pure (bodyTy, srcCts ++ pcWantedCts patCheck ++ [srcListCt] ++ bodyCts)
        CompGuard guard -> do
          (guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          let guardSp = exprSpan guard `orSourceSpan` ambient
              guardCt = mkWantedCtAt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
          (bodyTy, bodyCts) <- inferCompQuals ambient rest action
          pure (bodyTy, guardCts ++ [guardCt] ++ bodyCts)
        CompLetDecls decls ->
          inferLocalDecls inferExpr decls (inferCompQuals ambient rest action)
        CompThen {} -> unsupportedQual qual ambient rest action
        CompThenBy {} -> unsupportedQual qual ambient rest action
        CompGroupUsing {} -> unsupportedQual qual ambient rest action
        CompGroupByUsing {} -> unsupportedQual qual ambient rest action

    unsupportedQual qual ambient rest action = do
      let qualSp = compStmtSpan qual `orSourceSpan` ambient
      emitError qualSp (OtherError ("unsupported list comprehension qualifier in TC MVP: " ++ take 50 (show qual)))
      inferCompQuals ambient rest action

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
