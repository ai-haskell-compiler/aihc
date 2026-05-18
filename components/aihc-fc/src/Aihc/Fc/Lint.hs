{-# LANGUAGE OverloadedStrings #-}

-- | Core Lint: structural validator for System FC.
--
-- Type checking belongs in @aihc-tc@. This pass deliberately avoids
-- reconstructing result types, substituting type variables, or solving any
-- typing problem. It only validates Core structure that is independent of
-- expression type inference.
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

import Aihc.Fc.Syntax
import Aihc.Tc.Types (TcType, TyVarId, Unique)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | A lint error.
data LintError
  = -- | Variable not in scope.
    UnboundVar !Text !Unique
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
      go env rest
    go env (FcTopBind bind : rest) =
      let (errs, env') = lintBind env bind
       in errs ++ go env' rest

-- | Lint a binding, returning errors and the extended environment.
lintBind :: LintEnv -> FcBind -> ([LintError], LintEnv)
lintBind env (FcNonRec v e) =
  (lintExpr env e, extendTermEnv v env)
lintBind env (FcRec binds) =
  let env' = foldr (extendTermEnv . fst) env binds
   in (concatMap (lintExpr env' . snd) binds, env')

-- | Lint an expression structurally.
lintExpr :: LintEnv -> FcExpr -> [LintError]
lintExpr env (FcVar v) =
  case Map.lookup (varUnique v) (leTerms env) of
    Just _ty -> []
    Nothing -> [UnboundVar (varName v) (varUnique v)]
lintExpr _ (FcLit _) = []
lintExpr env (FcApp f a) =
  lintExpr env f <> lintExpr env a
lintExpr env (FcDictApp f a) =
  lintExpr env f <> lintExpr env a
lintExpr env (FcTyApp e _ty) =
  lintExpr env e
lintExpr env (FcLam v body) =
  lintExpr (extendTermEnv v env) body
lintExpr env (FcTyLam tv body) =
  lintExpr (env {leTyVars = Set.insert tv (leTyVars env)}) body
lintExpr env (FcDictLam v body) =
  lintExpr (extendTermEnv v env) body
lintExpr env (FcDict fields) =
  concatMap (lintExpr env) fields
lintExpr env (FcDictSelect dict _index) =
  lintExpr env dict
lintExpr env (FcLet bind body) =
  let (errs, env') = lintBind env bind
   in errs <> lintExpr env' body
lintExpr env (FcCase scrut _binder alts) =
  lintExpr env scrut
    <> case alts of
      [] -> [LintFailure "case expression has no alternatives"]
      _ -> concatMap (lintAlt env) alts
lintExpr env (FcCast e _co) =
  lintExpr env e

-- | Lint a case alternative.
lintAlt :: LintEnv -> FcAlt -> [LintError]
lintAlt env (FcAlt _con binders rhs) =
  lintExpr (foldr extendTermEnv env binders) rhs

-- | Extend the term environment with a variable.
extendTermEnv :: Var -> LintEnv -> LintEnv
extendTermEnv v env =
  env {leTerms = Map.insert (varUnique v) (varType v) (leTerms env)}
