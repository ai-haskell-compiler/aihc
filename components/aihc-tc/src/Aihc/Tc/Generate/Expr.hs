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
    Expr (..),
    LambdaCaseAlt (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve (ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Annotations (PendingTcAnnotation (..), pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Generate.Bind (inferLocalDecls, inferRhsWithLocals)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

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
    pure (expr, numericLiteralType numericType, [])
  EFloat {} ->
    pure (expr, doubleTyCon, [])
  EChar _ _ ->
    pure (expr, charTyCon, [])
  EString _ _ ->
    pure (expr, stringTyCon, [])
  ELambdaPats pats body ->
    inferLambda (exprSpan expr `orSourceSpan` ambient) pats body
  ELambdaCase alts ->
    inferLambdaCase (exprSpan expr `orSourceSpan` ambient) alts
  ELambdaCases alts ->
    inferLambdaCases (exprSpan expr `orSourceSpan` ambient) alts
  EApp fun arg ->
    inferApp (exprSpan expr `orSourceSpan` ambient) fun arg
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
    (inner', ty, cts) <- inferExprAt (exprSpan expr `orSourceSpan` ambient) inner
    pure (ETypeSig inner' tyAnn, ty, cts)
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
  other -> do
    emitError (exprSpan expr `orSourceSpan` ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (expr, ty, [])

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
      let pending =
            pendingAnnotation
              (instType inst)
              (instTypeArgs inst)
              (map ctEvVar cts)
              []
      pure (elaborationAnnotation pending, instType inst, cts)
    Just (TcMonoIdBinder ty) ->
      pure (Nothing, ty, [])
    Nothing ->
      abortTc ("resolved term missing from type environment: " <> show name <> " resolved as " <> show target)

inferOverloadedIntegerLiteral :: SourceSpan -> Annotation -> ResolutionAnnotation -> Expr -> TcM (Expr, TcType, [Ct])
inferOverloadedIntegerLiteral ambient resolutionAnn resolution literalExpr = do
  let sp = resolutionSpan resolution `orSourceSpan` ambient
  (methodTy, typeArgs, methodCts) <- inferResolvedFromInteger sp resolution
  resultTy <- freshMetaTv
  ev <- freshEvVar
  let integerArgTy = TcTyCon (TyCon "Integer" 0) []
      expectedMethodTy = TcFunTy integerArgTy resultTy
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

-- | Occurrence annotations are for elaboration facts consumed by FC, not for
-- restating the occurrence type. Binder types live on binder annotations.
elaborationAnnotation :: PendingTcAnnotation -> Maybe PendingTcAnnotation
elaborationAnnotation pending
  | null (pendingTcAnnTypeArgs pending)
      && null (pendingTcAnnEvidenceVars pending)
      && null (pendingTcAnnTermArgTypes pending) =
      Nothing
  | otherwise = Just pending

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
  let funTy = foldr TcFunTy bodyTy argTys
      pats' = map (annotatePatternBindings (pcBindings patCheck)) pats
  pure (ELambdaPats pats' body', funTy, pcWantedCts patCheck ++ bodyCts)

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
  pure (ECase scrutinee' alts', resTy, scrutCts ++ altCts)

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
inferCaseAlts sp scrutTy resTy (firstAlt : restAlts) = do
  (firstAlt', firstBranchSp, firstRhsSp, firstRhsTy, firstCts) <- inferAlt firstAlt
  resultEv <- freshEvVar
  let resultCt =
        mkWantedEqCt
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
      (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
      let rhsSp = rhsExprSpan rhs `orSourceSpan` branchSp
          pat' = annotatePatternBindings (pcBindings patCheck) pat
      pure (CaseAlt altAnns pat' rhs', branchSp, rhsSp, rhsTy, pcWantedCts patCheck ++ rhsCts)

    inferAltAgainst expectedBranchSp expectedTy alt = do
      (alt', branchSp, rhsSp, rhsTy, cts) <- inferAlt alt
      ev <- freshEvVar
      let rhsCt =
            mkWantedEqCt
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
      pure (alt', cts ++ [rhsCt])

inferLambdaCaseAlt :: SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM (LambdaCaseAlt, [Ct])
inferLambdaCaseAlt sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkPatterns sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternBindings (pcBindings patCheck) (inferRhs rhs)
  ev <- freshEvVar
  let pats' = map (annotatePatternBindings (pcBindings patCheck)) pats
      rhsCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  pure (alt {lambdaCaseAltPats = pats', lambdaCaseAltRhs = rhs'}, pcWantedCts patCheck ++ rhsCts ++ [rhsCt])

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
  (arg', argTy, argCts) <- inferExpr arg
  resTy <- freshMetaTv
  ev <- freshEvVar
  let eqCt = mkWantedCt (EqPred funTy (TcFunTy argTy resTy)) ev (AppOrigin sp) sp
  pure (EApp fun' arg', resTy, funCts ++ argCts ++ [eqCt])

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
  let condCt = mkWantedCt (EqPred condTy boolTyCon) condEv (AppOrigin sp) sp
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
      tc = TyCon {tyConName = tupleConText flavor n, tyConArity = n}
      tupleTy = TcTyCon tc tys
      pending = pendingAnnotation tupleTy tys [] []
  pure (annotatePendingExprAt sp pending (ETuple flavor elems'), tupleTy, cts)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (Nothing, ty, [])
    inferElem (Just e) = do
      (e', ty, cts) <- inferExpr e
      pure (Just e', ty, cts)

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor arity =
  case flavor of
    Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
    Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"

inferList :: SourceSpan -> [Expr] -> TcM (Expr, TcType, [Ct])
inferList sp elems = case elems of
  [] -> do
    elemTy <- freshMetaTv
    let listTy = TcTyCon listTyCon' [elemTy]
        pending = pendingAnnotation listTy [elemTy] [] []
    pure (annotatePendingExprAt sp pending (EList []), listTy, [])
  (e : es) -> do
    let headSp = exprSpan e `orSourceSpan` sp
    (headExpr, headTy, headCts) <- inferExpr e
    results <- mapM inferElem es
    let elems' = headExpr : map (\(expr, _, _, _) -> expr) results
        tailCts = concatMap (\(_, _, elemCts, _) -> elemCts) results
    eqCts <- mapM (mkElemEq headSp headTy) results
    let listTy = TcTyCon listTyCon' [headTy]
        pending = pendingAnnotation listTy [headTy] [] []
    pure (annotatePendingExprAt sp pending (EList elems'), listTy, headCts ++ tailCts ++ eqCts)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
    inferElem elemExpr = do
      (elemExpr', elemTy, elemCts) <- inferExpr elemExpr
      pure (elemExpr', elemTy, elemCts, exprSpan elemExpr `orSourceSpan` sp)
    mkElemEq headSp headTy (_, elemTy, _, elemSp) = do
      ev <- freshEvVar
      pure $
        mkWantedEqCt
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

inferListComp :: SourceSpan -> Expr -> [CompStmt] -> TcM (Expr, TcType, [Ct])
inferListComp sp body quals = do
  (quals', body', bodyTy, cts) <- inferCompQuals sp quals (inferExpr body)
  let resultTy = listType bodyTy
      pending = pendingAnnotation resultTy [bodyTy] [] []
  pure (annotatePendingExprAt sp pending (EListComp body' quals'), resultTy, cts)
  where
    listType elemTy = TcTyCon listTyCon' [elemTy]
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}

    inferCompQuals _ [] action = do
      (body', bodyTy, bodyCts) <- action
      pure ([], body', bodyTy, bodyCts)
    inferCompQuals ambient (qual : rest) action =
      case qual of
        CompAnn ann inner -> do
          (stmts', body', bodyTy, cts) <- inferCompQuals (compStmtSpan qual `orSourceSpan` ambient) (inner : rest) action
          case stmts' of
            inner' : rest' -> pure (CompAnn ann inner' : rest', body', bodyTy, cts)
            [] -> pure ([], body', bodyTy, cts)
        CompGen pat src -> do
          elemTy <- freshMetaTv
          (src', srcTy, srcCts) <- inferExpr src
          patCheck <- checkPattern ambient pat elemTy
          ev <- freshEvVar
          let srcSp = exprSpan src `orSourceSpan` ambient
              srcListCt = mkWantedCt (EqPred srcTy (listType elemTy)) ev (AppOrigin srcSp) srcSp
          (rest', body', bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferCompQuals ambient rest action)
          pure (CompGen (annotatePatternBindings (pcBindings patCheck) pat) src' : rest', body', bodyTy, srcCts ++ pcWantedCts patCheck ++ [srcListCt] ++ bodyCts)
        CompGuard guard -> do
          (guard', guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          let guardSp = exprSpan guard `orSourceSpan` ambient
              guardCt = mkWantedCt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
          (rest', body', bodyTy, bodyCts) <- inferCompQuals ambient rest action
          pure (CompGuard guard' : rest', body', bodyTy, guardCts ++ [guardCt] ++ bodyCts)
        CompLetDecls decls -> do
          (decls', (rest', body'), bodyTy, bodyCts) <-
            inferLocalDecls inferExpr decls $ do
              (rest', body', bodyTy, bodyCts) <- inferCompQuals ambient rest action
              pure ((rest', body'), bodyTy, bodyCts)
          pure (CompLetDecls decls' : rest', body', bodyTy, bodyCts)
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

nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

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
