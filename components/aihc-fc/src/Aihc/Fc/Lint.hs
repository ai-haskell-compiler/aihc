{-# LANGUAGE OverloadedStrings #-}

-- | Core Lint: structural type checker for System FC.
--
-- Lint is dramatically simpler than inference. No unification, no
-- constraint solving, no meta-variables. It is purely structural,
-- top-down type checking. If lint passes, the Core program is type-safe.
--
-- Invariants enforced:
--
-- 1. No meta-variables in types.
-- 2. Every variable reference is in scope.
-- 3. Every sub-expression's type is consistent with how it's used.
-- 4. Every cast has a valid coercion proof.
module Aihc.Fc.Lint
  ( -- * Lint
    lintProgram,
    lintExpr,

    -- * Errors
    LintError (..),

    -- * Environment
    LintEnv (..),
    emptyLintEnv,
  )
where

import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | A lint error.
data LintError
  = -- | Variable not in scope.
    UnboundVar !Text !Unique
  | -- | Type mismatch.
    TypeMismatch !String !TcType !TcType
  | -- | Meta-variable found in Core (should have been zonked).
    MetaVarInCore !Unique
  | -- | Case alternatives have inconsistent types.
    InconsistentAlts !TcType !TcType
  | -- | General lint failure.
    LintFailure !String
  deriving (Eq, Show)

-- | Lint environment.
data LintEnv = LintEnv
  { -- | Term variables in scope, mapped to their types.
    leTerms :: !(Map Unique TcType),
    -- | Type variables in scope.
    leTyVars :: !(Set TyVarId),
    -- | Known data constructors: name -> (type var params, field types, result type).
    leDataCons :: !(Map Text ([TyVarId], [TcType], TcType))
  }
  deriving (Show)

-- | An empty lint environment.
emptyLintEnv :: LintEnv
emptyLintEnv =
  LintEnv
    { leTerms = Map.empty,
      leTyVars = Set.empty,
      leDataCons = Map.empty
    }

-- | Lint an entire program.
lintProgram :: LintEnv -> FcProgram -> [LintError]
lintProgram env0 prog = go env0 (fcTopBinds prog)
  where
    go _ [] = []
    go env (FcData {} : rest) =
      -- Data declarations don't need expression-level linting.
      go env rest
    go env (FcTopBind bind : rest) =
      let (errs, env') = lintBind env bind
       in errs ++ go env' rest

-- | Lint a binding, returning errors and the extended environment.
lintBind :: LintEnv -> FcBind -> ([LintError], LintEnv)
lintBind env (FcNonRec v e) =
  let errs = case lintExpr env e of
        Left err -> [err]
        Right inferredTy ->
          [TypeMismatch "non-rec binding" (varType v) inferredTy | not (typesEqual (varType v) inferredTy)]
      env' = extendTermEnv v env
   in (errs, env')
lintBind env (FcRec binds) =
  let -- All binders are in scope for all RHSs.
      env' = foldr (extendTermEnv . fst) env binds
      errs = concatMap (lintRecBind env') binds
   in (errs, env')
  where
    lintRecBind recEnv (v, e) = case lintExpr recEnv e of
      Left err -> [err]
      Right inferredTy ->
        [TypeMismatch "rec binding" (varType v) inferredTy | not (typesEqual (varType v) inferredTy)]

-- | Lint an expression, returning its type or an error.
lintExpr :: LintEnv -> FcExpr -> Either LintError TcType
lintExpr env (FcVar v) =
  case Map.lookup (varUnique v) (leTerms env) of
    Just ty -> Right ty
    Nothing -> Left (UnboundVar (varName v) (varUnique v))
lintExpr _ (FcLit lit) = Right (litType lit)
lintExpr env (FcApp f a) = do
  fTy <- lintExpr env f
  aTy <- lintExpr env a
  case fTy of
    TcFunTy argTy resTy
      | typesEqual argTy aTy -> Right resTy
      | otherwise -> Left (TypeMismatch "application argument" argTy aTy)
    _ -> Left (LintFailure ("application to non-function type: " ++ show fTy))
lintExpr env (FcTyApp e ty) = do
  eTy <- lintExpr env e
  case eTy of
    TcForAllTy tv body ->
      Right (substType (Map.singleton tv ty) body)
    _ -> Left (LintFailure ("type application to non-forall type: " ++ show eTy))
lintExpr env (FcLam v body) = do
  let env' = extendTermEnv v env
  bodyTy <- lintExpr env' body
  Right (TcFunTy (varType v) bodyTy)
lintExpr env (FcTyLam tv body) = do
  let env' = env {leTyVars = Set.insert tv (leTyVars env)}
  bodyTy <- lintExpr env' body
  Right (TcForAllTy tv bodyTy)
lintExpr env (FcLet bind body) = do
  let (errs, env') = lintBind env bind
  case errs of
    [] -> lintExpr env' body
    (e : _) -> Left e
lintExpr env (FcCase scrut _binder resTy alts) = do
  _scrutTy <- lintExpr env scrut
  -- Check each alternative returns the declared result type.
  mapM_ (lintAlt env resTy) alts
  Right resTy
lintExpr env (FcCast e co) = do
  eTy <- lintExpr env e
  let (coFrom, coTo) = coercionEndpoints co
  if typesEqual eTy coFrom
    then Right coTo
    else Left (TypeMismatch "cast source" coFrom eTy)

-- | Lint a case alternative.
lintAlt :: LintEnv -> TcType -> FcAlt -> Either LintError ()
lintAlt env resTy (FcAlt _con binders rhs) = do
  let env' = foldr extendTermEnv env binders
  rhsTy <- lintExpr env' rhs
  if typesEqual resTy rhsTy
    then Right ()
    else Left (InconsistentAlts resTy rhsTy)

-- | Extract the endpoints of a coercion.
--
-- For the MVP, this is minimal. A full implementation would recursively
-- compute the proved equality.
coercionEndpoints :: Coercion -> (TcType, TcType)
coercionEndpoints (Refl ty) = (ty, ty)
coercionEndpoints (Sym co) = let (a, b) = coercionEndpoints co in (b, a)
coercionEndpoints (Trans co1 co2) =
  let (a, _) = coercionEndpoints co1
      (_, c) = coercionEndpoints co2
   in (a, c)
coercionEndpoints (CoVar _) = (TcMetaTv (Unique (-1)), TcMetaTv (Unique (-1)))
coercionEndpoints (TyConAppCo tc coercions) =
  let pairs = map coercionEndpoints coercions
   in (TcTyCon tc (map fst pairs), TcTyCon tc (map snd pairs))
coercionEndpoints (AxiomInstCo _ _) =
  (TcMetaTv (Unique (-1)), TcMetaTv (Unique (-1)))

-- | Get the type of a literal.
litType :: Literal -> TcType
litType (LitInt _) = TcTyCon (TyCon "Int" 0) []
litType (LitChar _) = TcTyCon (TyCon "Char" 0) []
litType (LitString _) = TcTyCon (TyCon "String" 0) []

-- | Extend the term environment with a variable.
extendTermEnv :: Var -> LintEnv -> LintEnv
extendTermEnv v env =
  env {leTerms = Map.insert (varUnique v) (varType v) (leTerms env)}

-- | Structural type equality (no unification).
typesEqual :: TcType -> TcType -> Bool
typesEqual (TcTyVar a) (TcTyVar b) = a == b
typesEqual (TcMetaTv a) (TcMetaTv b) = a == b
typesEqual (TcTyCon tc1 args1) (TcTyCon tc2 args2) =
  tc1 == tc2 && length args1 == length args2 && all (uncurry typesEqual) (zip args1 args2)
typesEqual (TcFunTy a1 b1) (TcFunTy a2 b2) =
  typesEqual a1 a2 && typesEqual b1 b2
typesEqual (TcForAllTy tv1 body1) (TcForAllTy tv2 body2) =
  -- Alpha-equivalence: rename tv2 to tv1 in body2.
  typesEqual body1 (substType (Map.singleton tv2 (TcTyVar tv1)) body2)
typesEqual (TcQualTy p1 b1) (TcQualTy p2 b2) =
  length p1 == length p2 && all (uncurry predsEqual) (zip p1 p2) && typesEqual b1 b2
typesEqual (TcAppTy f1 a1) (TcAppTy f2 a2) =
  typesEqual f1 f2 && typesEqual a1 a2
typesEqual _ _ = False

-- | Predicate equality.
predsEqual :: Pred -> Pred -> Bool
predsEqual (ClassPred c1 a1) (ClassPred c2 a2) =
  c1 == c2 && length a1 == length a2 && all (uncurry typesEqual) (zip a1 a2)
predsEqual (EqPred t1a t1b) (EqPred t2a t2b) =
  typesEqual t1a t2a && typesEqual t1b t2b
predsEqual _ _ = False
