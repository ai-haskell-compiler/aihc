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
    UnqualifiedName (..),
    fromAnnotation,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Generate.Bind (inferLocalDecls, inferRhsWithLocals)
import Aihc.Tc.Instantiate (instantiate)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the inferred type and a list of wanted constraints.
inferExpr :: Expr -> TcM (TcType, [Ct])
inferExpr expr = case expr of
  -- Variables: look up in environment, instantiate if polymorphic.
  EVar name -> inferVar NoSourceSpan (nameToText name)
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
  ELambdaPats pats body -> inferLambda NoSourceSpan pats body
  -- Lambda case: \case { pat -> body; ... }
  ELambdaCase alts -> inferLambdaCase NoSourceSpan alts
  -- Multi-argument lambda cases: \cases { p1 p2 -> body; ... }
  ELambdaCases alts -> inferLambdaCases NoSourceSpan alts
  -- Application: f x
  EApp fun arg -> inferApp NoSourceSpan fun arg
  -- Infix application: a `op` b
  EInfix lhs op rhs -> inferExpr (EApp (EApp (EVar op) lhs) rhs)
  -- If-then-else
  EIf cond thenE elseE -> inferIf NoSourceSpan cond thenE elseE
  -- Case expression
  ECase scrutinee alts -> inferCase NoSourceSpan scrutinee alts
  -- Let expression
  ELetDecls decls body ->
    inferLocalDecls inferExpr decls (inferExpr body)
  -- Parenthesized expression
  EParen inner -> inferExpr inner
  -- Type signature: (e :: T)
  ETypeSig inner _ty -> do
    -- MVP: just infer the inner expression.
    -- Full version would check against the given type.
    inferExpr inner
  -- Negation
  ENegate inner -> do
    (innerTy, cs) <- inferExpr inner
    -- For MVP, just return the inner type.
    pure (innerTy, cs)
  -- Annotated expression (from other passes, e.g. resolve).
  EAnn _ann inner -> inferExpr inner
  -- Tuple
  ETuple _flavor elems -> inferTuple NoSourceSpan elems
  -- List
  EList elems -> inferList NoSourceSpan elems
  -- List comprehension
  EListComp body quals -> inferListComp NoSourceSpan body quals
  -- Unsupported expression forms for MVP.
  other -> do
    emitError NoSourceSpan (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (ty, [])

-- | Infer the type of a variable reference.
inferVar :: SourceSpan -> Text -> TcM (TcType, [Ct])
inferVar sp name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme _) -> do
      (ty, preds) <- instantiate scheme
      cts <- mapM (predToCt sp name) preds
      pure (ty, cts)
    Just (TcMonoIdBinder _ ty) ->
      pure (ty, [])
    Nothing -> do
      emitError sp (UnboundVariable (T.unpack name))
      ty <- freshMetaTv
      pure (ty, [])

-- | Convert a predicate to a wanted constraint.
predToCt :: SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  pure $
    mkWantedCt p ev (OccurrenceOf name) sp

-- | Infer the type of a lambda expression.
inferLambda :: SourceSpan -> [Pattern] -> Expr -> TcM (TcType, [Ct])
inferLambda _sp pats body = do
  -- Create a fresh meta-variable for each pattern and bind pattern
  -- variables into the environment.
  argTys <- mapM (const freshMetaTv) pats
  let bindings = concatMap extractPatternBindings (zip pats argTys)
  -- Infer the body under the extended environment.
  (bodyTy, bodyCts) <- withPatternBindings bindings (inferExpr body)
  let funTy = foldr TcFunTy bodyTy argTys
  pure (funTy, bodyCts)

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
inferCaseAlts sp scrutTy resTy alts = concat <$> mapM inferAlt alts
  where
    inferAlt (CaseAlt altAnns pat rhs) = do
      let altSp = sourceSpanFromAnns altAnns
          branchSp = combineSourceSpan altSp sp
      let bindings = extractPatternBindings (pat, scrutTy)
      -- Emit constraint: pattern's constructor result type ~ scrutinee type.
      patCts <- inferPatternConstraints branchSp scrutTy pat
      -- Infer the RHS under the pattern bindings.
      (rhsTy, rhsCts) <- withPatternBindings bindings (inferRhs rhs)
      -- RHS must match the expected result type.
      ev <- freshEvVar
      let rhsCt = mkWantedCt (EqPred rhsTy resTy) ev (CaseBranchOrigin branchSp) branchSp
      pure (patCts ++ rhsCts ++ [rhsCt])

inferLambdaCaseAlt :: SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM [Ct]
inferLambdaCaseAlt sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
      bindings = concatMap extractPatternBindings (zip pats argTys)
  patCts <- concat <$> sequence [inferPatternConstraints sp argTy pat | (pat, argTy) <- zip pats argTys]
  (rhsTy, rhsCts) <- withPatternBindings bindings (inferRhs rhs)
  ev <- freshEvVar
  let rhsCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  pure (patCts ++ rhsCts ++ [rhsCt])

-- | Infer constraints from a pattern.
--
-- For constructor patterns, we emit an equality between the constructor's
-- result type and the scrutinee type. For variable/wildcard/literal patterns,
-- no extra constraints are needed (the variable just gets the scrutinee type).
inferPatternConstraints :: SourceSpan -> TcType -> Pattern -> TcM [Ct]
inferPatternConstraints sp scrutTy pat = case pat of
  PCon name _typeArgs _subPats -> do
    -- Look up the constructor; if found, emit scrutTy ~ constructor result type.
    let conName = nameToText name
    mBinder <- lookupTerm conName
    case mBinder of
      Just (TcIdBinder _ scheme _) -> do
        (conTy, _preds) <- instantiate scheme
        -- For a nullary constructor (no args), conTy is the result type.
        -- For a constructor with args, conTy is a function type whose
        -- result is the data type. We need to extract the result type.
        let conResTy = resultType conTy
        ev <- freshEvVar
        pure [mkWantedCt (EqPred scrutTy conResTy) ev (AppOrigin sp) sp]
      _ -> pure []
  PAnn _ann inner -> inferPatternConstraints sp scrutTy inner
  PParen inner -> inferPatternConstraints sp scrutTy inner
  PStrict inner -> inferPatternConstraints sp scrutTy inner
  PIrrefutable inner -> inferPatternConstraints sp scrutTy inner
  _ -> pure []

-- | Extract the result type from a (possibly nested) function type.
resultType :: TcType -> TcType
resultType (TcFunTy _ res) = resultType res
resultType ty = ty

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
  let eqCt = mkWantedCt (EqPred funTy (TcFunTy argTy resTy)) ev (AppOrigin sp) sp
  pure (resTy, funCts ++ argCts ++ [eqCt])

-- | Infer the type of if-then-else.
inferIf :: SourceSpan -> Expr -> Expr -> Expr -> TcM (TcType, [Ct])
inferIf sp cond thenE elseE = do
  (condTy, condCts) <- inferExpr cond
  (thenTy, thenCts) <- inferExpr thenE
  (elseTy, elseCts) <- inferExpr elseE
  -- Condition must be Bool.
  condEv <- freshEvVar
  let condCt = mkWantedCt (EqPred condTy boolTyCon) condEv (AppOrigin sp) sp
  -- Then and else branches must have the same type.
  branchEv <- freshEvVar
  let branchCt = mkWantedCt (EqPred thenTy elseTy) branchEv (AppOrigin sp) sp
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
  pure (TcTyCon tc tys, cts)
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
    pure (TcTyCon listTyCon' [elemTy], [])
  (e : es) -> do
    (headTy, headCts) <- inferExpr e
    results <- mapM inferExpr es
    let tailCts = concatMap snd results
    -- All elements must have the same type.
    eqCts <- mapM (mkElemEq headTy) results
    pure (TcTyCon listTyCon' [headTy], headCts ++ tailCts ++ eqCts)
  where
    listTyCon' = TyCon {tyConName = "[]", tyConArity = 1}
    mkElemEq headTy (elemTy, _) = do
      ev <- freshEvVar
      pure $ mkWantedCt (EqPred headTy elemTy) ev (AppOrigin sp) sp

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
          patCts <- inferPatternConstraints ambient elemTy pat
          ev <- freshEvVar
          let srcSp = exprSpan src `orSourceSpan` ambient
              srcListCt = mkWantedCt (EqPred srcTy (listType elemTy)) ev (AppOrigin srcSp) srcSp
              bindings = extractPatternBindings (pat, elemTy)
          (bodyTy, bodyCts) <- withPatternBindings bindings (inferCompQuals ambient rest action)
          pure (bodyTy, srcCts ++ patCts ++ [srcListCt] ++ bodyCts)
        CompGuard guard -> do
          (guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          let guardSp = exprSpan guard `orSourceSpan` ambient
              guardCt = mkWantedCt (EqPred guardTy boolTyCon) ev (AppOrigin guardSp) guardSp
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

-- | Extract variable bindings from a pattern paired with its expected type.
--
-- For a 'PVar', we bind the variable name to the given type.
-- For a 'PCon' (constructor pattern), we return bindings for sub-patterns
-- but assign fresh types to them (the constructor arg types are not yet
-- tracked in the MVP).
-- Other patterns are handled minimally for the MVP.
extractPatternBindings :: (Pattern, TcType) -> [(Text, TcType)]
extractPatternBindings (pat, ty) = case pat of
  PVar uname -> [(unqualifiedNameText uname, ty)]
  PAnn _ann inner -> extractPatternBindings (inner, ty)
  PParen inner -> extractPatternBindings (inner, ty)
  PWildcard {} -> []
  PLit {} -> []
  PNegLit {} -> []
  PAs name inner -> (unqualifiedNameText name, ty) : extractPatternBindings (inner, ty)
  PStrict inner -> extractPatternBindings (inner, ty)
  PIrrefutable inner -> extractPatternBindings (inner, ty)
  -- For constructor patterns like (True), (Just x), etc. the overall
  -- pattern type doesn't directly give us the sub-pattern types. But
  -- we can still extract the variable names for binding purposes.
  PCon _name _typeArgs subPats ->
    -- Each sub-pattern gets an unknown type (we'd need constructor info
    -- to assign proper types). For the MVP, they're not needed since
    -- constructor pattern matching in function heads is handled by tcMatches.
    concatMap (\p -> extractPatternBindings (p, ty)) subPats
  PInfix lhs op rhs
    | nameText op == ":" ->
        let elemTy = listElementType ty
         in extractPatternBindings (lhs, elemTy) ++ extractPatternBindings (rhs, ty)
    | otherwise ->
        extractPatternBindings (lhs, ty) ++ extractPatternBindings (rhs, ty)
  _ -> []

listElementType :: TcType -> TcType
listElementType ty =
  case ty of
    TcTyCon (TyCon "[]" 1) [elemTy] -> elemTy
    _ -> ty

-- | Run a computation with pattern bindings in scope.
withPatternBindings :: [(Text, TcType)] -> TcM a -> TcM a
withPatternBindings [] m = m
withPatternBindings ((name, ty) : rest) m =
  extendTermEnv name (TcMonoIdBinder name ty) (withPatternBindings rest m)
