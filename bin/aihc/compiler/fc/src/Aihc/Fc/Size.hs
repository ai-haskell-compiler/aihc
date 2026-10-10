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
    exprSizeWith,
    Known (..),
    tailLeaves,
    tailLeavesWith,
    isStrictBinder,
    isLiftedBinder,
    isLiftedType,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, extendBinder, reduceType, repOf, typeEnvFromProgram)
import Aihc.Resolve (PackageId (..))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)

-- | The size of a program: the sum of the sizes of its value bodies, plus
-- one for each value.
programSize :: PackageId -> Program -> Int
programSize primPackage program =
  sum [1 + exprSize env (valBody declaration) | DeclVal declaration <- programDecls program]
  where
    env = typeEnvFromProgram primPackage program

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
exprSize env = exprSizeWith env Map.empty

-- | What a variable is known to hold: a constructor or a literal, and
-- for each field of the constructor what it holds in turn, when that is
-- known.
data Known = Known !AltCon [Maybe Known]

-- | The size of an expression in which some cases are already decided:
-- a case on a variable the map knows selects the alternative of its
-- constructor, or else the default one, and counts that alternative
-- alone, with no case and no other alternative. Inside the alternative
-- the case binder and the field binders are known as well, so a case on
-- a field is decided in turn. This is what a copy of a body costs at a
-- site that gives a known constructor to a parameter the body
-- scrutinises, without the copy.
exprSizeWith :: TypeEnv -> Map Name Known -> Expr -> Int
exprSizeWith = go
  where
    go env known expr =
      case expr of
        ExVar {} -> 1
        ExLit {} -> 1
        ExCoercion {} -> 1
        ExApp function argument -> 1 + go env known function + go env known argument
        ExTyApp function _ -> go env known function
        ExLam _ body -> 1 + go env known body
        ExTyLam binder body -> go (extendBinder env binder) known body
        ExLet bind body
          | isStrictBinder env (bindBinder bind) -> 1 + go env known (bindRhs bind) + tailLeavesWith env known (bindRhs bind) * go env known body
          | otherwise -> 1 + go env known (bindRhs bind) + go env known body
        ExRec binds body -> 1 + sum (map (go env known . bindRhs) binds) + go env known body
        ExCase scrutinee binder alternatives
          | Just (alternative, known') <- selected known scrutinee binder alternatives -> altSize env known' alternative
          | otherwise ->
              go env known scrutinee + tailLeavesWith env known scrutinee * sum [1 + altSize env known alternative | alternative <- NE.toList alternatives]
        ExAbsurd scrutinee _ -> 1 + go env known scrutinee
        ExCast body _ -> go env known body
        ExForeignCall _ _ arguments -> 1 + sum (map (go env known) arguments)
    altSize env known alternative = go (List.foldl' extendBinder env (altTypeBinders alternative)) known (altRhs alternative)

-- | The alternative a known scrutinee selects, when the map knows the
-- scrutinee: the alternative of its constructor, or else the default
-- one, with the map extended by what the case binder and the field
-- binders of that alternative hold.
selected :: Map Name Known -> Expr -> Maybe Binder -> NE.NonEmpty Alt -> Maybe (Alt, Map Name Known)
selected known scrutinee binder alternatives = do
  name <- scrutineeVariable scrutinee
  value@(Known con fields) <- Map.lookup name known
  alternative <-
    case [alternative | alternative <- NE.toList alternatives, altCon alternative == con] of
      alternative : _ -> Just alternative
      [] -> case [alternative | alternative <- NE.toList alternatives, altCon alternative == AltDefault] of
        alternative : _ -> Just alternative
        [] -> Nothing
  let withBinder = maybe known (\named -> Map.insert (binderName named) value known) binder
      withFields
        | altCon alternative == con =
            List.foldl' (\acc (field, held) -> maybe acc (\value' -> Map.insert (binderName field) value' acc) held) withBinder (zip (altBinders alternative) fields)
        | otherwise = withBinder
  pure (alternative, withFields)

-- | The variable a scrutinee reads, under its casts.
scrutineeVariable :: Expr -> Maybe Name
scrutineeVariable expr =
  case expr of
    ExVar name -> Just name
    ExCast body _ -> scrutineeVariable body
    _ -> Nothing

-- | The number of paths through the tail of an expression: one for a
-- value or a call, the sum over the alternatives of a case, and the
-- product of the right-hand side and the body of a strict let.
tailLeaves :: TypeEnv -> Expr -> Int
tailLeaves env = tailLeavesWith env Map.empty

-- | 'tailLeaves' with some cases decided, as in 'exprSizeWith'.
tailLeavesWith :: TypeEnv -> Map Name Known -> Expr -> Int
tailLeavesWith = go
  where
    go env known expr =
      case expr of
        ExCase scrutinee binder alternatives
          | Just (alternative, known') <- selected known scrutinee binder alternatives -> altLeaves env known' alternative
          | otherwise -> max 1 (sum [altLeaves env known alternative | alternative <- NE.toList alternatives])
        ExLet bind body
          | isStrictBinder env (bindBinder bind) -> go env known (bindRhs bind) * go env known body
          | otherwise -> go env known body
        ExRec _ body -> go env known body
        ExCast body _ -> go env known body
        ExTyLam binder body -> go (extendBinder env binder) known body
        _ -> 1
    altLeaves env known alternative = go (List.foldl' extendBinder env (altTypeBinders alternative)) known (altRhs alternative)

-- | A binder that a let evaluates before its body: one whose type is not
-- lifted.
-- | A binder whose type is known to be unlifted: a let of such a binder
-- evaluates its right-hand side before its body. A binder whose
-- representation is not known, such as one of a type variable, is not
-- strict: its value can be a thunk once the variable is instantiated.
isStrictBinder :: TypeEnv -> Binder -> Bool
isStrictBinder env binder = isJust (repOf env (binderType binder)) && not (isLiftedBinder env binder)

-- | A binder whose type is lifted: a let of such a binder allocates a
-- thunk and evaluates nothing before its body.
isLiftedBinder :: TypeEnv -> Binder -> Bool
isLiftedBinder env = isLiftedType env . binderType

-- | A type whose values are lifted: such a value can be a thunk.
isLiftedType :: TypeEnv -> Type -> Bool
isLiftedType env ty =
  case reduceType env <$> repOf env ty of
    Just (TyCon name) -> nameText name == "LiftedRep"
    Just (TyApp (TyCon boxed) (TyCon levity)) ->
      nameText boxed == "BoxedRep" && nameText levity == "Lifted"
    _ -> False
