{-# LANGUAGE OverloadedStrings #-}

-- | The size of System FC code, as the optimizer measures it.
--
-- The size follows the lowered code, not the tree. A type, a coercion, and
-- a type application have no size. The GRIN CPS conversion copies the
-- continuation of a case in bind position into each of its alternatives,
-- so a case whose scrutinee has several tail leaves counts its
-- alternatives once for each leaf, and the body of a strict let counts
-- once for each leaf of its right-hand side. A case of a case is then not
-- free, and the inliner's growth is the growth of the object.
module Aihc.Fc.Size
  ( programSize,
    exprSize,
    tailLeaves,
    isStrictBinder,
    isLiftedBinder,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, extendBinder, reduceType, repOf, typeEnvFromProgram)
import Aihc.Fc.Wired (primPackageFromScopes)
import Aihc.Resolve (PackageId (..))
import Data.List qualified as List
import Data.Maybe (fromMaybe)

-- | The size of a program: the sum of the sizes of its value bodies, plus
-- one for each value.
programSize :: Program -> Int
programSize program =
  sum [1 + exprSize env (valBody declaration) | DeclVal declaration <- programDecls program]
  where
    env = typeEnvFromProgram (fromMaybe (PackageId "aihc-prim") (primPackageFromScopes (programScopes program))) program

-- | The number of nodes of an expression that reach the lowered code. A
-- type, a coercion, and a type application have no size.
--
-- The GRIN CPS conversion copies the continuation of a case in bind
-- position into each of its alternatives. A case whose scrutinee has
-- several tail leaves therefore counts its alternatives once for each
-- leaf, and the body of a strict let counts once for each leaf of its
-- right-hand side. The size then follows the lowered code, and a case of
-- a case is not free. The environment gives the kinds of the type
-- variables in scope, which decide whether a let is strict.
exprSize :: TypeEnv -> Expr -> Int
exprSize env expr =
  case expr of
    ExVar {} -> 1
    ExLit {} -> 1
    ExCoercion {} -> 1
    ExApp function argument -> 1 + exprSize env function + exprSize env argument
    ExTyApp function _ -> exprSize env function
    ExLam _ body -> 1 + exprSize env body
    ExTyLam binder body -> exprSize (extendBinder env binder) body
    ExLet bind body
      | isStrictBinder env (bindBinder bind) -> 1 + exprSize env (bindRhs bind) + tailLeaves env (bindRhs bind) * exprSize env body
      | otherwise -> 1 + exprSize env (bindRhs bind) + exprSize env body
    ExRec binds body -> 1 + sum (map (exprSize env . bindRhs) binds) + exprSize env body
    ExCase scrutinee _ _ alternatives ->
      exprSize env scrutinee + tailLeaves env scrutinee * sum [1 + altSize alternative | alternative <- alternatives]
    ExCast body _ -> exprSize env body
    ExForeignCall _ _ arguments -> 1 + sum (map (exprSize env) arguments)
  where
    altSize alternative = exprSize (List.foldl' extendBinder env (altTypeBinders alternative)) (altRhs alternative)

-- | The number of paths through the tail of an expression: one for a
-- value or a call, the sum over the alternatives of a case, and the
-- product of the right-hand side and the body of a strict let.
tailLeaves :: TypeEnv -> Expr -> Int
tailLeaves env expr =
  case expr of
    ExCase _ _ _ alternatives -> max 1 (sum [tailLeaves (List.foldl' extendBinder env (altTypeBinders alternative)) (altRhs alternative) | alternative <- alternatives])
    ExLet bind body
      | isStrictBinder env (bindBinder bind) -> tailLeaves env (bindRhs bind) * tailLeaves env body
      | otherwise -> tailLeaves env body
    ExRec _ body -> tailLeaves env body
    ExCast body _ -> tailLeaves env body
    ExTyLam binder body -> tailLeaves (extendBinder env binder) body
    _ -> 1

-- | A binder that a let evaluates before its body: one whose type is not
-- lifted.
isStrictBinder :: TypeEnv -> Binder -> Bool
isStrictBinder env = not . isLiftedBinder env

-- | A binder whose type is lifted: a let of such a binder allocates a
-- thunk and evaluates nothing before its body.
isLiftedBinder :: TypeEnv -> Binder -> Bool
isLiftedBinder env binder =
  case reduceType env <$> repOf env (binderType binder) of
    Just (TyCon name) -> nameText name == "LiftedRep"
    Just (TyApp (TyCon boxed) (TyCon levity)) ->
      nameText boxed == "BoxedRep" && nameText levity == "Lifted"
    _ -> False
