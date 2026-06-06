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
    fromAnnotation,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Generate.Bind (inferLocalDecls, inferRhsWithLocals)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Monad
import Aihc.Tc.NameKey (nameOccurrenceKey, syntaxOccurrenceKey)
import Aihc.Tc.NodeId (tcNodeIdFromAnnotations, tcNodeIdFromExpr, tcNodeIdFromExprRhs)
import Aihc.Tc.Types
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the inferred type and a list of wanted constraints.
inferExpr :: Expr -> TcM (TcType, [Ct])
inferExpr = inferExprAt NoSourceSpan

inferExprAt :: SourceSpan -> Expr -> TcM (TcType, [Ct])
inferExprAt ambient expr = withDiagnosticTarget (tcNodeIdFromExpr expr) $ case expr of
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
inferVar ambient nameSyntax = withDiagnosticTarget (tcNodeIdFromAnnotations (nameAnns nameSyntax)) $ do
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
          (tcNodeIdFromExprRhs (caseAltRhs firstAlt))
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
              (tcNodeIdFromExprRhs (caseAltRhs alt))
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
  let rhsCt = mkWantedCtAt (tcNodeIdFromExprRhs rhs) (EqPred rhsTy resTy) ev (AppOrigin sp) sp
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
  let condCt = mkWantedCtAt (tcNodeIdFromExpr cond) (EqPred condTy boolTyCon) condEv (AppOrigin sp) sp
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
    let tailCts = concatMap (\(_, elemCts, _, _) -> elemCts) results
    -- All elements must have the same type.
    eqCts <- mapM (mkElemEq headSp headTy) results
    let listTy = TcTyCon listTyCon' [headTy]
    recordListElaboration listTy headTy
    pure (listTy, headCts ++ tailCts ++ eqCts)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
    inferElem elemExpr = do
      (elemTy, elemCts) <- inferExpr elemExpr
      pure (elemTy, elemCts, exprSpan elemExpr `orSourceSpan` sp, tcNodeIdFromExpr elemExpr)
    recordListElaboration listTy elemTy =
      recordOccurrenceElaboration
        listOccurrenceKey
        OccurrenceElaboration
          { occurrenceElabType = listTy,
            occurrenceElabTypeArgs = [elemTy],
            occurrenceElabEvidenceVars = [],
            occurrenceElabTermArgTypes = []
          }
    mkElemEq headSp headTy (elemTy, _, elemSp, elemTarget) = do
      ev <- freshEvVar
      pure $
        mkWantedEqCtAt
          elemTarget
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
              srcListCt = mkWantedCtAt (tcNodeIdFromExpr src) (EqPred srcTy (listType elemTy)) ev (AppOrigin srcSp) srcSp
          (bodyTy, bodyCts) <- withPatternBindings (pcBindings patCheck) (inferCompQuals ambient rest action)
          pure (bodyTy, srcCts ++ pcWantedCts patCheck ++ [srcListCt] ++ bodyCts)
        CompGuard guard -> do
          (guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          let guardSp = exprSpan guard `orSourceSpan` ambient
              guardCt = mkWantedCtAt (tcNodeIdFromExpr guard) (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
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
