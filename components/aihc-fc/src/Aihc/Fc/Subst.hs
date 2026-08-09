-- | Capture-avoiding substitution for System FC types.
--
-- Used by the lint pass when checking type application (@\forall a. \tau@)
-- instantiated with a concrete type.
module Aihc.Fc.Subst
  ( substType,
    OccurrenceCount (..),
    countExprVar,
    substExpr,
    substExprVar,
    maximumProgramUnique,
    programVars,
    freeRigidTyVars,
    freeRigidTyVarsOf,
  )
where

import Aihc.Fc.Syntax
import Aihc.Tc.Types (Pred (..), TcType (..), TyVarId (..), Unique (..))
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | The greatest term-variable unique used anywhere in a program.
maximumProgramUnique :: FcProgram -> Int
maximumProgramUnique = maximum . (0 :) . map uniqueInt . programVars
  where
    uniqueInt var = case varUnique var of Unique value -> value

-- | All term variables in a program, including nested binders and occurrences.
programVars :: FcProgram -> [Var]
programVars (FcProgram _ topBinds) = concatMap topBindVars topBinds
  where
    topBindVars topBind =
      case topBind of
        FcPrimitive var _ -> [var]
        FcTopBind bind -> bindVars bind
        _ -> []

    bindVars bind =
      case bind of
        FcNonRec var expression -> var : exprVars expression
        FcRec bindings -> concat [var : exprVars expression | (var, expression) <- bindings]

    exprVars expression =
      case expression of
        FcVar var -> [var]
        FcLit {} -> []
        FcApp function argument -> exprVars function <> exprVars argument
        FcTyApp inner _ -> exprVars inner
        FcLam var body -> var : exprVars body
        FcTyLam _ body -> exprVars body
        FcLet bind body -> bindVars bind <> exprVars body
        FcCase scrutinee binder alternatives -> exprVars scrutinee <> (binder : concatMap alternativeVars alternatives)
        FcCast inner _ -> exprVars inner
        FcCallForeign _ arguments -> concatMap exprVars arguments

    alternativeVars alternative = altBinders alternative <> exprVars (altRhs alternative)

-- | A deliberately capped occurrence count. Simplifications only need to
-- distinguish dead variables, single uses, and uses that may benefit from
-- sharing.
data OccurrenceCount
  = Dead
  | Once
  | Many
  deriving (Eq, Show)

instance Semigroup OccurrenceCount where
  Dead <> count = count
  Once <> Dead = Once
  Once <> _ = Many
  Many <> _ = Many

instance Monoid OccurrenceCount where
  mempty = Dead

-- | Free rigid type variables in stable left-to-right order.
freeRigidTyVars :: TcType -> [TyVarId]
freeRigidTyVars = freeRigidTyVarsOf . pure

-- | Free rigid type variables across several types, without duplicates.
freeRigidTyVarsOf :: [TcType] -> [TyVarId]
freeRigidTyVarsOf = uniqueTyVars . concatMap go
  where
    go ty =
      case ty of
        TcTyVar tyVar -> [tyVar]
        TcMetaTv {} -> []
        TcTyCon _ arguments -> concatMap go arguments
        TcFunTy argument result -> go argument <> go result
        TcForAllTy tyVar body -> filter (/= tyVar) (go body)
        TcQualTy predicates body -> concatMap goPredicate predicates <> go body
        TcAppTy function argument -> go function <> go argument

    goPredicate predicate =
      case predicate of
        ClassPred _ arguments -> concatMap go arguments
        EqPred left right -> go left <> go right

    uniqueTyVars = List.foldl' (\variables tyVar -> if tyVar `elem` variables then variables else variables <> [tyVar]) []

-- | Substitute type variables in a type according to the given mapping.
--
-- This is capture-avoiding: if a @forall@ binds a variable that shadows
-- one in the substitution, we stop substituting that variable inside.
substType :: Map TyVarId TcType -> TcType -> TcType
substType subst ty
  | Map.null subst = ty
  | otherwise = go subst ty
  where
    go s (TcTyVar tv) = case Map.lookup tv s of
      Just t -> t
      Nothing -> TcTyVar tv
    go _ t@(TcMetaTv _) = t
    go s (TcTyCon tc args) = TcTyCon tc (map (go s) args)
    go s (TcFunTy a b) = TcFunTy (go s a) (go s b)
    go s (TcForAllTy tv body) =
      -- Remove the bound variable from substitution to avoid capture.
      let s' = Map.delete tv s
       in TcForAllTy tv (go s' body)
    go s (TcQualTy preds body) = TcQualTy (map (goPred s) preds) (go s body)
    go s (TcAppTy f a) = TcAppTy (go s f) (go s a)

    goPred s (ClassPred cls args) = ClassPred cls (map (go s) args)
    goPred s (EqPred t1 t2) = EqPred (go s t1) (go s t2)

-- | Count the free occurrences of a System FC term variable, stopping once
-- more than one occurrence has been found.
countExprVar :: Var -> FcExpr -> OccurrenceCount
countExprVar target = go
  where
    go expression =
      case expression of
        FcVar var
          | var == target -> Once
          | otherwise -> Dead
        FcLit {} -> Dead
        FcApp function argument -> go function <> go argument
        FcTyApp function _ -> go function
        FcLam binder body
          | binder == target -> Dead
          | otherwise -> go body
        FcTyLam _ body -> go body
        FcLet bind body ->
          case bind of
            FcNonRec binder rhs ->
              go rhs <> if binder == target then Dead else go body
            FcRec bindings
              | target `elem` map fst bindings -> Dead
              | otherwise -> foldMap (go . snd) bindings <> go body
        FcCase scrutinee binder alternatives ->
          go scrutinee
            <> if binder == target
              then Dead
              else foldMap goAlternative alternatives
        FcCast inner _ -> go inner
        FcCallForeign _ arguments -> foldMap go arguments

    goAlternative alternative
      | target `elem` altBinders alternative = Dead
      | otherwise = go (altRhs alternative)

-- | Scope-aware substitution of one System FC term variable with an
-- expression. Free variables in the replacement must be fresh for the target
-- expression's nested scopes.
substExpr :: Var -> FcExpr -> FcExpr -> FcExpr
substExpr source replacement = go
  where
    go expression =
      case expression of
        FcVar var
          | var == source -> replacement
          | otherwise -> expression
        FcLit {} -> expression
        FcApp function argument -> FcApp (go function) (go argument)
        FcTyApp function ty -> FcTyApp (go function) ty
        FcLam binder body
          | binder == source -> expression
          | otherwise -> FcLam binder (go body)
        FcTyLam tyVar body -> FcTyLam tyVar (go body)
        FcLet bind body ->
          case bind of
            FcNonRec binder rhs ->
              FcLet
                (FcNonRec binder (go rhs))
                (if binder == source then body else go body)
            FcRec bindings
              | source `elem` map fst bindings -> expression
              | otherwise -> FcLet (FcRec [(binder, go rhs) | (binder, rhs) <- bindings]) (go body)
        FcCase scrutinee binder alternatives ->
          FcCase
            (go scrutinee)
            binder
            (if binder == source then alternatives else map goAlternative alternatives)
        FcCast inner coercion -> FcCast (go inner) coercion
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map go arguments)

    goAlternative alternative
      | source `elem` altBinders alternative = alternative
      | otherwise = alternative {altRhs = go (altRhs alternative)}

-- | Scope-aware substitution of one System FC term variable. The replacement
-- must be fresh for the expression's scope.
--
-- Case lowering uses this to make the evaluated case binder authoritative in
-- an alternative. In particular, primitives that consume an already-entered
-- value must not receive the original thunk merely because the source body
-- referred to the scrutinee by its old name.
substExprVar :: Var -> Var -> FcExpr -> FcExpr
substExprVar source replacement = substExpr source (FcVar replacement)
