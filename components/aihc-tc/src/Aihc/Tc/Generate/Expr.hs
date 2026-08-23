{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for expressions.
--
-- This module implements bidirectional type inference/checking for the
-- surface expression language. It walks the surface AST and returns the same
-- expression with pending type-checker annotations attached at the exact sites
-- that produced them.
module Aihc.Tc.Generate.Expr
  ( inferExpr,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    CaseAlt (..),
    CompStmt (..),
    DoFlavor (..),
    DoStmt (..),
    Expr (..),
    LambdaCaseAlt (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    Type,
    UnqualifiedName,
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Annotations (PendingTcAnnotation (..), pendingAnnotation, pendingTypeLambdaAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvVar)
import Aihc.Tc.Generate.Bind (inferLocalDecls, inferRhsWithLocals)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch)
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (checkSurfaceType, tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Aihc.Tc.Unify (unify)
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (when)
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

-- | Infer the type of an expression.
--
-- Returns the pending-annotated expression, the inferred type, and wanted
-- constraints.
inferExpr :: Expr -> TcM (Expr, TcType, [Ct])
inferExpr = inferExprAt NoSourceSpan

inferExprAt :: SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferExprAt ambient expr = case expr of
  EAnn ann inner
    | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
      isFromIntegerResolution resolution,
      EInt _ TInteger _ <- inner ->
        inferOverloadedIntegerLiteral ambient ann resolution inner
  EVar name ->
    inferVar (exprSpan expr `orSourceSpan` ambient) name
  EInt _ numericType _ ->
    literalResult expr (numericLiteralType numericType)
  EFloat {} ->
    literalResult expr doubleTyCon
  EChar _ _ ->
    literalResult expr (resolvedType "Char")
  ECharHash _ _ ->
    literalResult expr (primType "Char#")
  EString _ _ ->
    literalResult expr stringTyCon
  EStringHash _ _ ->
    literalResult expr (primType "Addr#")
  ELambdaPats pats body ->
    inferLambda (exprSpan expr `orSourceSpan` ambient) pats body
  ELambdaCase alts ->
    inferLambdaCase (exprSpan expr `orSourceSpan` ambient) alts
  ELambdaCases alts ->
    inferLambdaCases (exprSpan expr `orSourceSpan` ambient) alts
  EApp fun arg ->
    inferApp (exprSpan expr `orSourceSpan` ambient) fun arg
  ETypeApp fun tyArg ->
    inferTypeApp (exprSpan expr `orSourceSpan` ambient) fun tyArg
  EInfix lhs op rhs ->
    inferInfix (exprSpan expr `orSourceSpan` ambient) lhs op rhs
  EIf cond thenE elseE ->
    inferIf (exprSpan expr `orSourceSpan` ambient) cond thenE elseE
  ECase scrutinee alts ->
    inferCase (exprSpan expr `orSourceSpan` ambient) scrutinee alts
  ELetDecls decls body -> do
    (decls', body', bodyTy, cts) <- inferLocalDecls inferExpr decls (inferExpr body)
    pure (ELetDecls decls' body', bodyTy, cts)
  EParen inner -> do
    (inner', ty, cts) <- inferExprAt (exprSpan expr `orSourceSpan` ambient) inner
    pure (EParen inner', ty, cts)
  ETypeSig inner tyAnn -> do
    inferTypeSig (exprSpan expr `orSourceSpan` ambient) inner tyAnn
  ENegate inner -> do
    (inner', innerTy, cs) <- inferExpr inner
    pure (ENegate inner', innerTy, cs)
  EAnn ann inner -> do
    (inner', ty, cts) <- inferExprAt (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
    pure (EAnn ann inner', ty, cts)
  ETuple flavor elems ->
    inferTuple (exprSpan expr `orSourceSpan` ambient) flavor elems
  EList elems ->
    inferList (exprSpan expr `orSourceSpan` ambient) elems
  EListComp body quals ->
    inferListComp (exprSpan expr `orSourceSpan` ambient) body quals
  EDo stmts flavor ->
    inferDo (exprSpan expr `orSourceSpan` ambient) flavor stmts
  other -> do
    emitError (exprSpan expr `orSourceSpan` ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (expr, ty, [])

literalResult :: Expr -> TcM TcType -> TcM (Expr, TcType, [Ct])
literalResult expr typeAction = do
  ty <- typeAction
  pure (annotatePendingExpr (pendingAnnotation ty [] [] []) expr, ty, [])

-- | Infer the type of a variable reference.
inferVar :: SourceSpan -> Name -> TcM (Expr, TcType, [Ct])
inferVar ambient nameSyntax = do
  (mPending, ty, cts) <- inferNameOccurrence ambient nameSyntax
  let expr =
        case mPending of
          Just pending -> annotatePendingExprAt (sourceSpanFromAnns (nameAnns nameSyntax)) pending (EVar nameSyntax)
          Nothing -> EVar nameSyntax
  pure (expr, ty, cts)

inferOperator :: SourceSpan -> Name -> TcM (Name, TcType, [Ct])
inferOperator ambient nameSyntax = do
  (mPending, ty, cts) <- inferNameOccurrence ambient nameSyntax
  let name' =
        case mPending of
          Just pending -> annotatePendingName pending nameSyntax
          Nothing -> nameSyntax
  pure (name', ty, cts)

inferNameOccurrence :: SourceSpan -> Name -> TcM (Maybe PendingTcAnnotation, TcType, [Ct])
inferNameOccurrence ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) `orSourceSpan` ambient
      name = nameToText nameSyntax
  target <- resolvedTermTarget nameSyntax
  mBinder <- lookupResolvedTerm name target
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      let typeArgs = instTypeArgs inst
          evidenceVars = map ctEvVar cts
          pending = occurrenceAnnotation (instType inst) typeArgs evidenceVars
      pure (pending, instType inst, cts)
    Just (TcMonoIdBinder ty) -> do
      (instantiatedTy, typeArgs) <- instantiateSigmaType ty
      pure (occurrenceAnnotation instantiatedTy typeArgs [], instantiatedTy, [])
    Nothing ->
      abortTc ("resolved term missing from type environment: " <> show name <> " resolved as " <> show target)

occurrenceAnnotation :: TcType -> [TcType] -> [EvVar] -> Maybe PendingTcAnnotation
occurrenceAnnotation ty typeArgs evidenceVars
  | null typeArgs && null evidenceVars = Nothing
  | otherwise = Just (pendingAnnotation ty typeArgs evidenceVars [])

inferTypeSig :: SourceSpan -> Expr -> Type -> TcM (Expr, TcType, [Ct])
inferTypeSig sp inner tyAnn = do
  (inner', innerTy, cts) <- inferExprAt sp inner
  sigTy <- checkSurfaceType Map.empty tyAnn KType
  ev <- freshEvVar
  let sigCt =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = innerTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin sp
            }
          TypeTrace
            { typeTraceType = sigTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = TypeSignatureOrigin "<expression>" sp
            }
          ev
          (SigOrigin sp)
          sp
  pure (ETypeSig inner' tyAnn, sigTy, cts <> [sigCt])

inferOverloadedIntegerLiteral :: SourceSpan -> Annotation -> ResolutionAnnotation -> Expr -> TcM (Expr, TcType, [Ct])
inferOverloadedIntegerLiteral ambient resolutionAnn resolution literalExpr = do
  let sp = resolutionSpan resolution `orSourceSpan` ambient
  (methodTy, typeArgs, methodCts) <- inferResolvedFromInteger sp resolution
  resultTy <- freshMetaTv
  ev <- freshEvVar
  integerArgTy <-
    case methodTy of
      TcFunTy argumentTy _ -> pure argumentTy
      _ -> abortTc "fromInteger does not have a function type"
  let expectedMethodTy = TcFunTy integerArgTy resultTy
      methodEq =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = methodTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ConstraintTypeOrigin (OccurrenceOf "fromInteger")
            }
          TypeTrace
            { typeTraceType = expectedMethodTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ConstraintTypeOrigin (LitOrigin sp)
            }
          ev
          (LitOrigin sp)
          sp
      pending =
        pendingAnnotation
          resultTy
          typeArgs
          (map ctEvVar methodCts)
          []
  pure (annotatePendingExprAt sp pending (EAnn resolutionAnn literalExpr), resultTy, methodCts <> [methodEq])

inferResolvedFromInteger :: SourceSpan -> ResolutionAnnotation -> TcM (TcType, [TcType], [Ct])
inferResolvedFromInteger sp resolution = do
  mBinder <- lookupResolvedTerm "fromInteger" (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp "fromInteger") (instPreds inst)
      pure (instType inst, instTypeArgs inst, cts)
    Just (TcMonoIdBinder ty) ->
      pure (ty, [], [])
    Nothing ->
      abortTc ("resolved fromInteger missing from type environment: " <> show (resolutionTarget resolution))

isFromIntegerResolution :: ResolutionAnnotation -> Bool
isFromIntegerResolution resolution =
  resolutionNamespace resolution == ResolutionNamespaceTerm
    && resolutionName resolution == "fromInteger"

isDoBindResolution :: ResolutionAnnotation -> Bool
isDoBindResolution resolution =
  resolutionNamespace resolution == ResolutionNamespaceTerm
    && resolutionName resolution == ">>="

annotatePendingExpr :: PendingTcAnnotation -> Expr -> Expr
annotatePendingExpr ann =
  EAnn (mkAnnotation ann)

annotatePendingExprAt :: SourceSpan -> PendingTcAnnotation -> Expr -> Expr
annotatePendingExprAt NoSourceSpan ann =
  annotatePendingExpr ann
annotatePendingExprAt sp ann =
  EAnn (mkAnnotation sp) . annotatePendingExpr ann

annotatePendingName :: PendingTcAnnotation -> Name -> Name
annotatePendingName ann name =
  name {nameAnns = nameAnns name <> [mkAnnotation ann]}

-- | Convert a predicate to a wanted constraint.
predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  pure $
    mkWantedCt p ev (OccurrenceOf name) sp

-- | Infer the type of a lambda expression.
inferLambda :: SourceSpan -> [Pattern] -> Expr -> TcM (Expr, TcType, [Ct])
inferLambda sp pats body = do
  argTys <- mapM (const freshMetaTv) pats
  patCheck <- checkPatterns sp (zip pats argTys)
  (body', bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferExpr body)
  remainingCts <- solvePatternBranch sp patCheck bodyTy bodyCts
  let funTy = foldr TcFunTy bodyTy argTys
      pats' = zipWith (annotateLambdaPattern (pcBindings patCheck)) argTys (pcPatterns patCheck)
  pure (ELambdaPats pats' body', funTy, remainingCts)

annotateLambdaPattern :: [(UnqualifiedName, TcType)] -> TcType -> Pattern -> Pattern
annotateLambdaPattern bindings argTy pat =
  let annotated = annotatePatternBindings bindings pat
   in if lambdaPatternCarriesBinderType annotated
        then annotated
        else PAnn (mkAnnotation (pendingAnnotation argTy [] [] [])) annotated

lambdaPatternCarriesBinderType :: Pattern -> Bool
lambdaPatternCarriesBinderType (PAnn _ inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType PVar {} = True
lambdaPatternCarriesBinderType (PParen inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType PAs {} = True
lambdaPatternCarriesBinderType (PStrict inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PIrrefutable inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PTypeSig inner _) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType _ = False

inferLambdaCase :: SourceSpan -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
inferLambdaCase sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  (alts', cts) <- inferCaseAlts sp argTy resTy alts
  pure (ELambdaCase alts', TcFunTy argTy resTy, cts)

inferCase :: SourceSpan -> Expr -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
inferCase sp scrutinee alts = do
  (scrutinee', scrutTy, scrutCts) <- inferExpr scrutinee
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAlts sp scrutTy resTy alts
  let pending = pendingAnnotation resTy [] [] []
  pure (annotatePendingExprAt sp pending (ECase scrutinee' alts'), resTy, scrutCts ++ altCts)

inferLambdaCases :: SourceSpan -> [LambdaCaseAlt] -> TcM (Expr, TcType, [Ct])
inferLambdaCases sp alts = do
  let arity = maximum (0 : map (length . lambdaCaseAltPats) alts)
  argTys <- mapM (const freshMetaTv) [1 .. arity]
  resTy <- freshMetaTv
  results <- mapM (inferLambdaCaseAlt sp argTys resTy) alts
  let alts' = map fst results
      cts = concatMap snd results
  pure (ELambdaCases alts', foldr TcFunTy resTy argTys, cts)

inferCaseAlts :: SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM ([CaseAlt Expr], [Ct])
inferCaseAlts _sp _scrutTy _resTy [] = pure ([], [])
inferCaseAlts sp scrutTy resTy alternatives = do
  results <- mapM inferAlt alternatives
  pure (map fst results, concatMap snd results)
  where
    inferAlt (CaseAlt altAnns pat rhs) = do
      let altSp = sourceSpanFromAnns altAnns
          branchSp = combineSourceSpan altSp sp
      patCheck <- checkPattern branchSp pat scrutTy
      (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
      resultEv <- freshEvVar
      let rhsSp = rhsExprSpan rhs `orSourceSpan` branchSp
          resultCt =
            mkWantedEqCt
              TypeTrace
                { typeTraceType = rhsTy,
                  typeTraceRole = ActualType,
                  typeTraceOrigin = ExpressionTypeOrigin rhsSp
                }
              TypeTrace
                { typeTraceType = resTy,
                  typeTraceRole = ExpectedType,
                  typeTraceOrigin = ConstraintTypeOrigin (CaseBranchOrigin branchSp)
                }
              resultEv
              (CaseBranchOrigin rhsSp)
              rhsSp
          pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      remainingCts <- solvePatternBranch branchSp patCheck resTy (rhsCts <> [resultCt])
      pure (CaseAlt altAnns pat' rhs', remainingCts)

inferLambdaCaseAlt :: SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM (LambdaCaseAlt, [Ct])
inferLambdaCaseAlt sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkPatterns sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
  ev <- freshEvVar
  let pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck)
      rhsCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  remainingCts <- solvePatternBranch sp patCheck resTy (rhsCts <> [rhsCt])
  pure (alt {lambdaCaseAltPats = pats', lambdaCaseAltRhs = rhs'}, remainingCts)

sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    sp : _ -> sp

combineSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
combineSourceSpan NoSourceSpan fallback = fallback
combineSourceSpan span' _ = span'

inferRhs :: Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
inferRhs = inferRhsWithLocals inferExpr

inferApp :: SourceSpan -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferApp sp fun arg = do
  (fun', funTy, funCts) <- inferExpr fun
  zonkedFunTy <- zonkType funTy
  case zonkedFunTy of
    TcFunTy expectedArgTy resultTy
      | hasLeadingForAll expectedArgTy -> do
          (arg', argCts) <- checkHigherRankArgument sp expectedArgTy arg
          pure (EApp fun' arg', resultTy, funCts <> argCts)
    _ -> do
      (arg', argTy, argCts) <- inferExpr arg
      resTy <- freshMetaTv
      ev <- freshEvVar
      let eqCt = mkWantedCt (EqPred funTy (TcFunTy argTy resTy)) ev (AppOrigin sp) sp
      pure (EApp fun' arg', resTy, funCts <> argCts <> [eqCt])

checkHigherRankArgument :: SourceSpan -> TcType -> Expr -> TcM (Expr, [Ct])
checkHigherRankArgument sp expectedTy arg = do
  boundary <- getUniqueBoundary
  (arg', actualTy, argCts) <- inferExpr arg
  (skolems, expectedBody) <- skolemizeSigmaType expectedTy
  unify sp (AppOrigin sp) actualTy expectedBody
  rejectEscapingHigherRankMetas sp boundary skolems actualTy
  let annotatedArg = annotatePendingExprAt sp (pendingTypeLambdaAnnotation expectedTy skolems) arg'
  pure (annotatedArg, argCts)

hasLeadingForAll :: TcType -> Bool
hasLeadingForAll TcForAllTy {} = True
hasLeadingForAll _ = False

instantiateSigmaType :: TcType -> TcM (TcType, [TcType])
instantiateSigmaType = go []
  where
    go arguments (TcForAllTy binder body) = do
      argument <- freshMetaTv
      go (arguments <> [argument]) (applySubst (Map.singleton (tvUnique binder) argument) body)
    go arguments ty = pure (ty, arguments)

skolemizeSigmaType :: TcType -> TcM ([TyVarId], TcType)
skolemizeSigmaType = go []
  where
    go skolems (TcForAllTy binder body) = do
      skolem <- setTyVarKind (tvKind binder) <$> freshSkolemTv (tvName binder)
      go (skolems <> [skolem]) (applySubst (Map.singleton (tvUnique binder) (TcTyVar skolem)) body)
    go skolems ty = pure (skolems, ty)

rejectEscapingHigherRankMetas :: SourceSpan -> Unique -> [TyVarId] -> TcType -> TcM ()
rejectEscapingHigherRankMetas sp (Unique boundaryInt) skolems actualTy = do
  let olderMetas = filter (isOlderThan boundaryInt) (typeMetaVariables actualTy)
  escaped <- anyM (metaMentionsAnySkolem skolems) olderMetas
  when escaped $
    emitError sp (OtherError "higher-rank type variable escapes its argument")
  where
    isOlderThan threshold (Unique metaInt) = metaInt < threshold

metaMentionsAnySkolem :: [TyVarId] -> Unique -> TcM Bool
metaMentionsAnySkolem skolems meta = do
  ty <- zonkType (TcMetaTv meta)
  pure (any (`typeMentionsTyVar` ty) skolems)

anyM :: (a -> TcM Bool) -> [a] -> TcM Bool
anyM _ [] = pure False
anyM predicate (value : values) = do
  matches <- predicate value
  if matches then pure True else anyM predicate values

typeMetaVariables :: TcType -> [Unique]
typeMetaVariables ty =
  case ty of
    TcTyVar {} -> []
    TcMetaTv meta -> [meta]
    TcTyCon _ arguments -> concatMap typeMetaVariables arguments
    TcFunTy argument result -> typeMetaVariables argument <> typeMetaVariables result
    TcForAllTy _ body -> typeMetaVariables body
    TcQualTy predicates body -> concatMap predicateMetaVariables predicates <> typeMetaVariables body
    TcAppTy function argument -> typeMetaVariables function <> typeMetaVariables argument

predicateMetaVariables :: Pred -> [Unique]
predicateMetaVariables predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeMetaVariables arguments
    EqPred left right -> typeMetaVariables left <> typeMetaVariables right

typeMentionsTyVar :: TyVarId -> TcType -> Bool
typeMentionsTyVar target ty =
  case ty of
    TcTyVar tyVar -> tyVar == target
    TcMetaTv {} -> False
    TcTyCon _ arguments -> any (typeMentionsTyVar target) arguments
    TcFunTy argument result -> typeMentionsTyVar target argument || typeMentionsTyVar target result
    TcForAllTy binder body -> binder /= target && typeMentionsTyVar target body
    TcQualTy predicates body -> any (predicateMentionsTyVar target) predicates || typeMentionsTyVar target body
    TcAppTy function argument -> typeMentionsTyVar target function || typeMentionsTyVar target argument

predicateMentionsTyVar :: TyVarId -> Pred -> Bool
predicateMentionsTyVar target predicate =
  case predicate of
    ClassPred _ arguments -> any (typeMentionsTyVar target) arguments
    EqPred left right -> typeMentionsTyVar target left || typeMentionsTyVar target right

inferTypeApp :: SourceSpan -> Expr -> Type -> TcM (Expr, TcType, [Ct])
inferTypeApp sp fun tyArg = do
  (fun', funTy, funCts) <- inferExpr fun
  explicitTy <- checkSurfaceType Map.empty tyArg KType
  case drop (visibleTypeApplicationCount fun) (pendingTypeArgs fun') of
    inferredTy : _ -> do
      ev <- freshEvVar
      let origin = InstOrigin "visible type application"
          eqCt =
            mkWantedEqCt
              TypeTrace
                { typeTraceType = inferredTy,
                  typeTraceRole = InferredType,
                  typeTraceOrigin = ConstraintTypeOrigin origin
                }
              TypeTrace
                { typeTraceType = explicitTy,
                  typeTraceRole = RequiredType,
                  typeTraceOrigin = ConstraintTypeOrigin origin
                }
              ev
              origin
              sp
      pure (ETypeApp fun' tyArg, funTy, funCts <> [eqCt])
    [] -> do
      emitError sp (OtherError "visible type application requires a polymorphic expression")
      pure (ETypeApp fun' tyArg, funTy, funCts)

visibleTypeApplicationCount :: Expr -> Int
visibleTypeApplicationCount expr =
  case expr of
    ETypeApp fun _ -> 1 + visibleTypeApplicationCount fun
    EParen inner -> visibleTypeApplicationCount inner
    EAnn _ inner -> visibleTypeApplicationCount inner
    _ -> 0

pendingTypeArgs :: Expr -> [TcType]
pendingTypeArgs expr =
  case expr of
    EAnn ann inner ->
      case fromAnnotation @PendingTcAnnotation ann of
        Just pending -> pendingTcAnnTypeArgs pending
        Nothing -> pendingTypeArgs inner
    ETypeApp fun _ -> pendingTypeArgs fun
    EParen inner -> pendingTypeArgs inner
    _ -> []

inferInfix :: SourceSpan -> Expr -> Name -> Expr -> TcM (Expr, TcType, [Ct])
inferInfix sp lhs op rhs = do
  -- Generate the same constraints as desugared binary application while
  -- keeping the operator occurrence on the surface operator node.
  (op', opTy, opCts) <- inferOperator sp op
  (lhs', lhsTy, lhsCts) <- inferExpr lhs
  midTy <- freshMetaTv
  lhsEv <- freshEvVar
  let lhsCt = mkWantedCt (EqPred opTy (TcFunTy lhsTy midTy)) lhsEv (AppOrigin sp) sp
  (rhs', rhsTy, rhsCts) <- inferExpr rhs
  resTy <- freshMetaTv
  rhsEv <- freshEvVar
  let rhsCt = mkWantedCt (EqPred midTy (TcFunTy rhsTy resTy)) rhsEv (AppOrigin sp) sp
  pure (EInfix lhs' op' rhs', resTy, opCts ++ lhsCts ++ [lhsCt] ++ rhsCts ++ [rhsCt])

inferIf :: SourceSpan -> Expr -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferIf sp cond thenE elseE = do
  (cond', condTy, condCts) <- inferExpr cond
  (thenE', thenTy, thenCts) <- inferExpr thenE
  (elseE', elseTy, elseCts) <- inferExpr elseE
  condEv <- freshEvVar
  expectedBoolTy <- boolTyCon
  let condCt = mkWantedCt (EqPred condTy expectedBoolTy) condEv (AppOrigin sp) sp
  branchEv <- freshEvVar
  let branchCt = mkWantedCt (EqPred thenTy elseTy) branchEv (AppOrigin sp) sp
  pure (EIf cond' thenE' elseE', thenTy, condCts ++ thenCts ++ elseCts ++ [condCt, branchCt])

inferTuple :: SourceSpan -> TupleFlavor -> [Maybe Expr] -> TcM (Expr, TcType, [Ct])
inferTuple sp flavor elems = do
  results <- mapM inferElem elems
  let elems' = map (\(expr, _, _) -> expr) results
      tys = map (\(_, ty, _) -> ty) results
      cts = concatMap (\(_, _, elemCts) -> elemCts) results
      n = length tys
      typeName = tupleTyConText flavor n
  maybeTyCon <- lookupTyCon typeName
  elementKinds <- mapM tcTypeKind tys
  let fallbackKind =
        case flavor of
          Boxed -> foldr KFun KType elementKinds
          Unboxed -> foldr KFun (KTYPE (TupleRep (map runtimeRepOrLifted elementKinds))) elementKinds
  tc <-
    case maybeTyCon of
      Just info -> pure (tciTyCon info)
      Nothing -> mkKnownTyCon (tupleTyConModule flavor) typeName n fallbackKind
  let tupleTy = TcTyCon tc tys
      pending = pendingAnnotation tupleTy tys [] []
  pure (annotatePendingExprAt sp pending (ETuple flavor elems'), tupleTy, cts)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (Nothing, ty, [])
    inferElem (Just e) = do
      (e', ty, cts) <- inferExpr e
      pure (Just e', ty, cts)

    runtimeRepOrLifted kind = fromRight liftedRep (runtimeRepFromKind kind)

tupleTyConText :: TupleFlavor -> Int -> Text
tupleTyConText flavor arity =
  case flavor of
    Boxed -> boxedTupleTyConName arity
    Unboxed -> unboxedTupleTyConName arity

tupleTyConModule :: TupleFlavor -> Text
tupleTyConModule flavor =
  case flavor of
    Boxed -> "GHC.Tuple"
    Unboxed -> "GHC.Types"

inferList :: SourceSpan -> [Expr] -> TcM (Expr, TcType, [Ct])
inferList sp elems = do
  nilInstantiation <- instantiateListConstructor sp "[]"
  nilCts <- mapM (predToCt sp "[]") (instPreds nilInstantiation)
  case elems of
    [] -> do
      let listTy = instType nilInstantiation
          pending = pendingAnnotation listTy (instTypeArgs nilInstantiation) (map ctEvVar nilCts) []
      pure (annotatePendingExprAt sp pending (EList []), listTy, nilCts)
    _ -> do
      results <- mapM inferElem elems
      consInstantiation <- instantiateListConstructor sp ":"
      consPredicateCts <- mapM (predToCt sp ":") (instPreds consInstantiation)
      case instType consInstantiation of
        TcFunTy sourceElemTy (TcFunTy sourceTailTy sourceResultTy) -> do
          let elems' = map (\(element, _, _, _) -> element) results
              elemCts = concatMap (\(_, _, cts, _) -> cts) results
              (firstElemTy, firstElemSp) = case results of
                (_, ty, _, elemSp) : _ -> (ty, elemSp)
                [] -> (sourceElemTy, sp)
              pending = pendingAnnotation sourceResultTy [sourceElemTy] [] []
          firstConstructorCt <- constructorEqualityCt firstElemSp firstElemTy sourceElemTy
          nilConstructorCt <- constructorEqualityCt sp (instType nilInstantiation) sourceTailTy
          resultConstructorCt <- constructorEqualityCt sp sourceResultTy sourceTailTy
          elementEqualityCts <- mapM (elementEqualityCt firstElemSp firstElemTy) (drop 1 results)
          let constructorCts = nilCts <> consPredicateCts <> [firstConstructorCt, nilConstructorCt, resultConstructorCt]
          pure (annotatePendingExprAt sp pending (EList elems'), sourceResultTy, elemCts <> elementEqualityCts <> constructorCts)
        _ -> abortTc "GHC.Types list cons constructor has an invalid type"
  where
    inferElem elemExpr = do
      (elemExpr', elemTy, elemCts) <- inferExpr elemExpr
      pure (elemExpr', elemTy, elemCts, exprSpan elemExpr `orSourceSpan` sp)
    constructorEqualityCt loc left right = do
      ev <- freshEvVar
      pure (mkWantedCt (EqPred left right) ev (AppOrigin loc) loc)
    elementEqualityCt firstElemSp firstElemTy (_, elemTy, _, elemSp) = do
      ev <- freshEvVar
      pure $
        mkWantedEqCt
          TypeTrace
            { typeTraceType = elemTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin elemSp
            }
          TypeTrace
            { typeTraceType = firstElemTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ListElementTypeOrigin firstElemSp
            }
          ev
          (AppOrigin elemSp)
          elemSp

instantiateListConstructor :: SourceSpan -> Text -> TcM Instantiation
instantiateListConstructor sp name = do
  sourceBinder <- lookupTerm name
  maybeBinder <- maybe (lookupKnownTerm "GHC.Types" name) (pure . Just) sourceBinder
  case maybeBinder of
    Just (TcIdBinder scheme _) -> instantiateWithArgs scheme
    Just TcMonoIdBinder {} ->
      abortTc ("GHC.Types list constructor is monomorphic at " <> show sp <> ": " <> show name)
    Nothing ->
      abortTc ("GHC.Types list constructor is missing at " <> show sp <> ": " <> show name)

inferListComp :: SourceSpan -> Expr -> [CompStmt] -> TcM (Expr, TcType, [Ct])
inferListComp sp body quals = do
  listTyCon' <- resolvedListTyCon
  (quals', body', bodyTy, cts) <- inferCompQuals listTyCon' sp quals (inferExpr body)
  let resultTy = listType listTyCon' bodyTy
      pending = pendingAnnotation resultTy [bodyTy] [] []
  pure (annotatePendingExprAt sp pending (EListComp body' quals'), resultTy, cts)
  where
    listType tyCon elemTy = TcTyCon tyCon [elemTy]
    inferCompQuals _ _ [] action = do
      (body', bodyTy, bodyCts) <- action
      pure ([], body', bodyTy, bodyCts)
    inferCompQuals listTyCon' ambient (qual : rest) action =
      case qual of
        CompAnn ann inner -> do
          (stmts', body', bodyTy, cts) <- inferCompQuals listTyCon' (compStmtSpan qual `orSourceSpan` ambient) (inner : rest) action
          case stmts' of
            inner' : rest' -> pure (CompAnn ann inner' : rest', body', bodyTy, cts)
            [] -> pure ([], body', bodyTy, cts)
        CompGen pat src -> do
          elemTy <- freshMetaTv
          (src', srcTy, srcCts) <- inferExpr src
          patCheck <- checkPattern ambient pat elemTy
          ev <- freshEvVar
          let srcSp = exprSpan src `orSourceSpan` ambient
              srcListCt = mkWantedCt (EqPred srcTy (listType listTyCon' elemTy)) ev (AppOrigin srcSp) srcSp
          (rest', body', bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferCompQuals listTyCon' ambient rest action)
          remainingCts <- solvePatternBranch ambient patCheck bodyTy bodyCts
          pure (CompGen (annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)) src' : rest', body', bodyTy, srcCts ++ [srcListCt] ++ remainingCts)
        CompGuard guard -> do
          (guard', guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          expectedBoolTy <- boolTyCon
          let guardSp = exprSpan guard `orSourceSpan` ambient
              guardCt = mkWantedCt (EqPred guardTy expectedBoolTy) ev (AppOrigin guardSp) guardSp
          (rest', body', bodyTy, bodyCts) <- inferCompQuals listTyCon' ambient rest action
          pure (CompGuard guard' : rest', body', bodyTy, guardCts ++ [guardCt] ++ bodyCts)
        CompLetDecls decls -> do
          (decls', (rest', body'), bodyTy, bodyCts) <-
            inferLocalDecls inferExpr decls $ do
              (rest', body', bodyTy, bodyCts) <- inferCompQuals listTyCon' ambient rest action
              pure ((rest', body'), bodyTy, bodyCts)
          pure (CompLetDecls decls' : rest', body', bodyTy, bodyCts)
        CompThen {} -> unsupportedQual listTyCon' qual ambient rest action
        CompThenBy {} -> unsupportedQual listTyCon' qual ambient rest action
        CompGroupUsing {} -> unsupportedQual listTyCon' qual ambient rest action
        CompGroupByUsing {} -> unsupportedQual listTyCon' qual ambient rest action

    unsupportedQual listTyCon' qual ambient rest action = do
      let qualSp = compStmtSpan qual `orSourceSpan` ambient
      emitError qualSp (OtherError ("unsupported list comprehension qualifier in TC MVP: " ++ take 50 (show qual)))
      inferCompQuals listTyCon' ambient rest action

resolvedListTyCon :: TcM TyCon
resolvedListTyCon = do
  maybeInfo <- lookupTyCon "[]"
  maybe (mkKnownTyCon "GHC.Types" "[]" 1 (KFun KType KType)) (pure . tciTyCon) maybeInfo

inferDo :: SourceSpan -> DoFlavor -> [DoStmt Expr] -> TcM (Expr, TcType, [Ct])
inferDo sp flavor stmts =
  case flavor of
    DoPlain -> do
      (stmts', resultTy, cts) <- inferDoStmts sp stmts
      let pending = pendingAnnotation resultTy [] [] []
      pure (annotatePendingExprAt sp pending (EDo stmts' flavor), resultTy, cts)
    _ -> do
      emitError sp (OtherError ("unsupported do flavor in TC MVP: " ++ show flavor))
      resultTy <- freshMetaTv
      pure (EDo stmts flavor, resultTy, [])

inferDoStmts :: SourceSpan -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmts sp stmts =
  case stmts of
    [] -> do
      emitError sp (OtherError "empty do block in TC MVP")
      resultTy <- freshMetaTv
      pure ([], resultTy, [])
    [stmt] -> inferLastDoStmt sp stmt
    stmt : rest -> inferDoStmt sp stmt rest

inferLastDoStmt :: SourceSpan -> DoStmt Expr -> TcM ([DoStmt Expr], TcType, [Ct])
inferLastDoStmt ambient stmt =
  case stmt of
    DoAnn ann inner -> do
      (stmts', resultTy, cts) <- inferLastDoStmt (doStmtSpan stmt `orSourceSpan` ambient) inner
      case stmts' of
        [inner'] -> pure ([DoAnn ann inner'], resultTy, cts)
        _ -> pure (stmts', resultTy, cts)
    DoExpr body -> do
      (body', bodyTy, cts) <- inferExprAt ambient body
      pure ([DoExpr body'], bodyTy, cts)
    _ -> do
      emitError ambient (OtherError "the last statement in a do block must be an expression")
      resultTy <- freshMetaTv
      pure ([stmt], resultTy, [])

inferDoStmt :: SourceSpan -> DoStmt Expr -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmt ambient stmt rest =
  case stmt of
    DoAnn ann inner
      | Just resolution <- fromAnnotation @ResolutionAnnotation ann,
        isDoBindResolution resolution ->
          inferResolvedDoStmt ambient ann resolution inner rest
    DoAnn ann inner -> do
      (stmts', resultTy, cts) <- inferDoStmt (doStmtSpan stmt `orSourceSpan` ambient) inner rest
      case stmts' of
        inner' : rest' -> pure (DoAnn ann inner' : rest', resultTy, cts)
        [] -> pure ([], resultTy, cts)
    DoBind pat action -> do
      monadTy <- freshMetaTv
      itemTy <- freshMetaTv
      resultItemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      patCheck <- checkPattern ambient pat itemTy
      (rest', resultTy, restCts) <-
        withPatternBindings (pcBindings patCheck) (inferDoStmts ambient rest)
      actionEq <- wantedDoEq ambient actionTy (TcAppTy monadTy itemTy)
      resultEq <- wantedDoEq ambient resultTy (TcAppTy monadTy resultItemTy)
      monadCt <- wantedMonad ambient monadTy
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      remainingCts <- solvePatternBranch ambient patCheck resultTy restCts
      pure
        ( DoBind pat' action' : rest',
          resultTy,
          actionCts <> remainingCts <> [actionEq, resultEq, monadCt]
        )
    DoExpr action -> do
      monadTy <- freshMetaTv
      itemTy <- freshMetaTv
      resultItemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      (rest', resultTy, restCts) <- inferDoStmts ambient rest
      actionEq <- wantedDoEq ambient actionTy (TcAppTy monadTy itemTy)
      resultEq <- wantedDoEq ambient resultTy (TcAppTy monadTy resultItemTy)
      monadCt <- wantedMonad ambient monadTy
      pure
        ( DoExpr action' : rest',
          resultTy,
          actionCts <> restCts <> [actionEq, resultEq, monadCt]
        )
    DoLetDecls decls -> do
      (decls', rest', resultTy, cts) <-
        inferLocalDecls inferExpr decls $ do
          (rest', resultTy, restCts) <- inferDoStmts ambient rest
          pure (rest', resultTy, restCts)
      pure (DoLetDecls decls' : rest', resultTy, cts)
    DoRecStmt _ -> do
      emitError ambient (OtherError "recursive do statements are unsupported in TC MVP")
      (rest', resultTy, cts) <- inferDoStmts ambient rest
      pure (stmt : rest', resultTy, cts)

inferResolvedDoStmt :: SourceSpan -> Annotation -> ResolutionAnnotation -> DoStmt Expr -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferResolvedDoStmt ambient resolutionAnn resolution stmt rest =
  case stmt of
    DoBind pat action -> do
      itemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      patCheck <- checkPattern ambient pat itemTy
      (rest', resultTy, restCts) <-
        withPatternBindings (pcBindings patCheck) (inferDoStmts ambient rest)
      (pending, methodCts) <- inferDoBindMethod ambient resolution actionTy itemTy resultTy
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
          stmt' = DoAnn (mkAnnotation pending) (DoAnn resolutionAnn (DoBind pat' action'))
      remainingCts <- solvePatternBranch ambient patCheck resultTy restCts
      pure (stmt' : rest', resultTy, actionCts <> remainingCts <> methodCts)
    DoExpr action -> do
      itemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      (rest', resultTy, restCts) <- inferDoStmts ambient rest
      (pending, methodCts) <- inferDoBindMethod ambient resolution actionTy itemTy resultTy
      let stmt' = DoAnn (mkAnnotation pending) (DoAnn resolutionAnn (DoExpr action'))
      pure (stmt' : rest', resultTy, actionCts <> restCts <> methodCts)
    _ -> do
      emitError ambient (OtherError "internal do-bind annotation on a non-action statement")
      inferDoStmt ambient stmt rest

inferDoBindMethod :: SourceSpan -> ResolutionAnnotation -> TcType -> TcType -> TcType -> TcM (PendingTcAnnotation, [Ct])
inferDoBindMethod sp resolution actionTy itemTy resultTy = do
  mBinder <- lookupResolvedTerm ">>=" (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      methodCts <- mapM (predToCt sp ">>=") (instPreds inst)
      ev <- freshEvVar
      let expectedTy = TcFunTy actionTy (TcFunTy (TcFunTy itemTy resultTy) resultTy)
          methodEq = mkWantedCt (EqPred (instType inst) expectedTy) ev (OccurrenceOf ">>=") sp
          pending = pendingAnnotation (instType inst) (instTypeArgs inst) (map ctEvVar methodCts) []
      pure (pending, methodCts <> [methodEq])
    Just (TcMonoIdBinder ty) -> do
      ev <- freshEvVar
      let expectedTy = TcFunTy actionTy (TcFunTy (TcFunTy itemTy resultTy) resultTy)
          methodEq = mkWantedCt (EqPred ty expectedTy) ev (OccurrenceOf ">>=") sp
      pure (pendingAnnotation ty [] [] [], [methodEq])
    Nothing ->
      abortTc ("resolved >>= missing from type environment: " <> show (resolutionTarget resolution))

wantedDoEq :: SourceSpan -> TcType -> TcType -> TcM Ct
wantedDoEq sp actual expected = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred actual expected) ev (AppOrigin sp) sp)

wantedMonad :: SourceSpan -> TcType -> TcM Ct
wantedMonad sp monadTy = do
  ev <- freshEvVar
  maybeMonad <- lookupTyCon "Monad"
  case maybeMonad of
    Just monadInfo -> pure (mkWantedCt (ClassPred (tciTyCon monadInfo) [monadTy]) ev (AppOrigin sp) sp)
    Nothing -> abortTc "missing checked type constructor for Monad"

orSourceSpan :: SourceSpan -> SourceSpan -> SourceSpan
orSourceSpan NoSourceSpan fallback = fallback
orSourceSpan sp _ = sp

compStmtSpan :: CompStmt -> SourceSpan
compStmtSpan compStmt =
  case compStmt of
    CompAnn ann _ -> fromMaybe NoSourceSpan (fromAnnotation @SourceSpan ann)
    _ -> NoSourceSpan

doStmtSpan :: DoStmt body -> SourceSpan
doStmtSpan stmt =
  case stmt of
    DoAnn ann _ -> fromMaybe NoSourceSpan (fromAnnotation @SourceSpan ann)
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

nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

intTyCon :: TcM TcType
intTyCon = knownTyConType "GHC.Types" "Int"

numericLiteralType :: NumericType -> TcM TcType
numericLiteralType numericType =
  case numericType of
    TInteger -> intTyCon
    TIntHash -> primType "Int#"
    TWordHash -> primType "Word#"
    TInt8Hash -> primType "Int8#"
    TInt16Hash -> primType "Int16#"
    TInt32Hash -> primType "Int32#"
    TInt64Hash -> primType "Int64#"
    TWord8Hash -> primType "Word8#"
    TWord16Hash -> primType "Word16#"
    TWord32Hash -> primType "Word32#"
    TWord64Hash -> primType "Word64#"

primType :: Text -> TcM TcType
primType = knownTyConType "GHC.Prim"

knownTyConType :: Text -> Text -> TcM TcType
knownTyConType moduleName name = do
  maybeInfo <- lookupTyCon name
  tyCon <- maybe (mkKnownTyCon moduleName name 0 typeKindType) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [])

doubleTyCon :: TcM TcType
doubleTyCon = do
  maybeInfo <- lookupTyCon "Double"
  case maybeInfo of
    Just info -> pure (TcTyCon (tciTyCon info) [])
    Nothing -> TcTyCon <$> mkKnownTyCon "GHC.Types" "Double" 0 typeKindType <*> pure []

resolvedType :: Text -> TcM TcType
resolvedType name = do
  maybeInfo <- lookupTyCon name
  tyCon <- maybe (mkKnownTyCon "GHC.Types" name 0 typeKindType) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [])

stringTyCon :: TcM TcType
stringTyCon = do
  listTyCon <- resolvedListTyCon
  charType <- resolvedType "Char"
  pure (TcTyCon listTyCon [charType])

boolTyCon :: TcM TcType
boolTyCon = do
  maybeInfo <- lookupTyCon "Bool"
  case maybeInfo of
    Just info -> pure (TcTyCon (tciTyCon info) [])
    Nothing -> TcTyCon <$> mkKnownTyCon "GHC.Types" "Bool" 0 typeKindType <*> pure []
