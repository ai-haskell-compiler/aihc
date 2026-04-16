{-# LANGUAGE OverloadedStrings #-}

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
  ( CaseAlt (..),
    Expr (..),
    Name (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    UnqualifiedName (..),
    getSourceSpan,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Instantiate (instantiate)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the inferred type and a list of wanted constraints.
inferExpr :: Expr -> TcM (TcType, [Ct])
inferExpr expr = case expr of
  -- Variables: look up in environment, instantiate if polymorphic.
  EVar name -> inferVar (getSourceSpan expr) (nameToText name)
  -- Integer literals: monomorphic Int for MVP.
  -- (Full version would use Num constraint.)
  EInt _ _ -> pure (intTyCon, [])
  EIntBase _ _ -> pure (intTyCon, [])
  -- Float literals.
  EFloat _ _ -> pure (doubleTyCon, [])
  -- Char literals.
  EChar _ _ -> pure (charTyCon, [])
  -- String literals.
  EString _ _ -> pure (stringTyCon, [])
  -- Lambda: \x -> body
  ELambdaPats pats body -> inferLambda (getSourceSpan expr) pats body
  -- Lambda case: \case { pat -> body; ... }
  ELambdaCase alts -> inferLambdaCase (getSourceSpan expr) alts
  -- Application: f x
  EApp fun arg -> inferApp (getSourceSpan expr) fun arg
  -- If-then-else
  EIf cond thenE elseE -> inferIf (getSourceSpan expr) cond thenE elseE
  -- Let expression
  ELetDecls _decls body -> do
    -- MVP: infer body only (let bindings not yet processed).
    -- Full version would typecheck declarations and extend env.
    inferExpr body
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
  ETuple _flavor elems -> inferTuple (getSourceSpan expr) elems
  -- List
  EList elems -> inferList (getSourceSpan expr) elems
  -- Unsupported expression forms for MVP.
  other -> do
    let sp = getSourceSpan other
    emitError sp (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (ty, [])

-- | Infer the type of a variable reference.
inferVar :: SourceSpan -> Text -> TcM (TcType, [Ct])
inferVar sp name = do
  mBinder <- lookupTerm name
  case mBinder of
    Just (TcIdBinder _ scheme) -> do
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
inferLambdaCase :: SourceSpan -> [CaseAlt] -> TcM (TcType, [Ct])
inferLambdaCase sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  cts <- inferCaseAlts sp argTy resTy alts
  pure (TcFunTy argTy resTy, cts)

-- | Infer constraints from case alternatives.
--
-- Each alternative's pattern is checked against the scrutinee type,
-- and each RHS must unify with the expected result type.
inferCaseAlts :: SourceSpan -> TcType -> TcType -> [CaseAlt] -> TcM [Ct]
inferCaseAlts sp scrutTy resTy alts = concat <$> mapM inferAlt alts
  where
    inferAlt (CaseAlt _altSp pat rhs) = do
      let bindings = extractPatternBindings (pat, scrutTy)
      -- Emit constraint: pattern's constructor result type ~ scrutinee type.
      patCts <- inferPatternConstraints sp scrutTy pat
      -- Infer the RHS under the pattern bindings.
      (rhsTy, rhsCts) <- withPatternBindings bindings (inferRhs rhs)
      -- RHS must match the expected result type.
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
      Just (TcIdBinder _ scheme) -> do
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

-- | Infer the type of a right-hand side (for case alternatives).
inferRhs :: Rhs -> TcM (TcType, [Ct])
inferRhs (UnguardedRhs _sp expr _decls) = inferExpr expr
inferRhs (GuardedRhss _sp _guards _decls) = do
  ty <- freshMetaTv
  pure (ty, [])

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

-- | Convert a surface Name to Text for lookup.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

-- | Built-in type constructors (MVP).
intTyCon :: TcType
intTyCon = TcTyCon (TyCon "Int" 0) []

doubleTyCon :: TcType
doubleTyCon = TcTyCon (TyCon "Double" 0) []

charTyCon :: TcType
charTyCon = TcTyCon (TyCon "Char" 0) []

stringTyCon :: TcType
stringTyCon = TcTyCon (TyCon "String" 0) []

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
  PAs name inner -> (name, ty) : extractPatternBindings (inner, ty)
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
  PInfix lhs _name rhs ->
    extractPatternBindings (lhs, ty) ++ extractPatternBindings (rhs, ty)
  _ -> []

-- | Run a computation with pattern bindings in scope.
withPatternBindings :: [(Text, TcType)] -> TcM a -> TcM a
withPatternBindings [] m = m
withPatternBindings ((name, ty) : rest) m =
  extendTermEnv name (TcMonoIdBinder name ty) (withPatternBindings rest m)
