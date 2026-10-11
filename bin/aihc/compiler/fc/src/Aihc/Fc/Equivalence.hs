-- | Alpha equivalence of System FC expressions.
--
-- Two expressions are alpha equivalent when they are the same up to the
-- names of the binders that they bind themselves. A free name must be the
-- same name on both sides, with one exception: the caller can give a set
-- of free names on each side that may correspond to each other. These are
-- the field binders of two case alternatives. The match then gives the
-- correspondence that it found, and each name corresponds to one name
-- only.
module Aihc.Fc.Equivalence
  ( alphaEquivalent,
    alphaCorrespondence,
  )
where

import Aihc.Fc.Name (Name)
import Aihc.Fc.Syntax
import Control.Monad (unless, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, put)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Set qualified as Set

-- | Whether two expressions are the same up to the names of their own
-- binders.
alphaEquivalent :: Expr -> Expr -> Bool
alphaEquivalent left right = isJust (alphaCorrespondence Set.empty Set.empty left right)

-- | Match two expressions up to the names of their own binders. A free
-- name of the first set can correspond to a free name of the second set.
-- The result gives each such correspondence that the match used, from the
-- left name to the right name.
alphaCorrespondence :: Set Name -> Set Name -> Expr -> Expr -> Maybe (Map Name Name)
alphaCorrespondence leftHoles rightHoles left right =
  fst <$> execStateT (expr (Scope Map.empty Map.empty) left right) (Map.empty, Map.empty)
  where
    -- The names that the two expressions bind, each mapped to its partner.
    -- The state holds the correspondence of the free names.
    name :: Scope -> Name -> Name -> Match ()
    name scope l r =
      case (Map.lookup l (scopeLeft scope), Map.lookup r (scopeRight scope)) of
        (Just partner, Just back) -> check (partner == r && back == l)
        (Just _, Nothing) -> lift Nothing
        (Nothing, Just _) -> lift Nothing
        (Nothing, Nothing)
          | Set.member l leftHoles && Set.member r rightHoles -> do
              (forward, backward) <- get
              case (Map.lookup l forward, Map.lookup r backward) of
                (Just partner, _) -> check (partner == r)
                (Nothing, Just _) -> lift Nothing
                (Nothing, Nothing) -> put (Map.insert l r forward, Map.insert r l backward)
          | otherwise -> check (l == r && not (Set.member l leftHoles) && not (Set.member r rightHoles))

    binder :: Scope -> Binder -> Binder -> Match Scope
    binder scope l r = do
      ty scope (binderType l) (binderType r)
      pure (bindNames scope (binderName l) (binderName r))

    binders :: Scope -> [Binder] -> [Binder] -> Match Scope
    binders scope ls rs = do
      check (length ls == length rs)
      go scope (zip ls rs)
      where
        go current pairs =
          case pairs of
            [] -> pure current
            (l, r) : rest -> binder current l r >>= (`go` rest)

    ty :: Scope -> Type -> Type -> Match ()
    ty scope l r =
      case (l, r) of
        (TyVar a, TyVar b) -> name scope a b
        (TyCon a, TyCon b) -> check (a == b)
        (TyLit a x, TyLit b y) -> check (a == b && x == y)
        (TyApp f a, TyApp g b) -> ty scope f g >> ty scope a b
        (TyFun a b c d, TyFun e f g h) -> ty scope a e >> ty scope b f >> ty scope c g >> ty scope d h
        (TyForAll a body, TyForAll b body') -> binder scope a b >>= \inner -> ty inner body body'
        (TyEq a b, TyEq c d) -> ty scope a c >> ty scope b d
        _ -> lift Nothing

    coercion :: Scope -> Coercion -> Coercion -> Match ()
    coercion scope l r =
      case (l, r) of
        (CoVar a, CoVar b) -> name scope a b
        (CoRefl a, CoRefl b) -> ty scope a b
        (CoSym a, CoSym b) -> coercion scope a b
        (CoTrans a b, CoTrans c d) -> coercion scope a c >> coercion scope b d
        (CoApp a b, CoApp c d) -> coercion scope a c >> coercion scope b d
        (CoFun a b, CoFun c d) -> coercion scope a c >> coercion scope b d
        (CoForAll a body, CoForAll b body') -> binder scope a b >>= \inner -> coercion inner body body'
        (CoNth i a, CoNth j b) -> check (i == j) >> coercion scope a b
        (CoTyConApp a as, CoTyConApp b bs) -> check (a == b && length as == length bs) >> zipWithM_ (coercion scope) as bs
        (CoAxiom a as, CoAxiom b bs) -> check (a == b && length as == length bs) >> zipWithM_ (ty scope) as bs
        _ -> lift Nothing

    literal :: Scope -> Literal -> Literal -> Match ()
    literal scope l r =
      case (l, r) of
        (LitInt a x, LitInt b y) -> check (x == y) >> ty scope a b
        (LitChar a x, LitChar b y) -> check (x == y) >> ty scope a b
        (LitAddr a x, LitAddr b y) -> check (x == y) >> ty scope a b
        _ -> lift Nothing

    expr :: Scope -> Expr -> Expr -> Match ()
    expr scope l r =
      case (l, r) of
        (ExVar a, ExVar b) -> name scope a b
        (ExLit a x, ExLit b y) -> literal scope a b >> ty scope x y
        (ExApp f a, ExApp g b) -> expr scope f g >> expr scope a b
        (ExTyApp f a, ExTyApp g b) -> expr scope f g >> ty scope a b
        (ExLam a body, ExLam b body') -> binder scope a b >>= \inner -> expr inner body body'
        (ExTyLam a body, ExTyLam b body') -> binder scope a b >>= \inner -> expr inner body body'
        (ExLet (Bind a rhs) body, ExLet (Bind b rhs') body') -> do
          expr scope rhs rhs'
          inner <- binder scope a b
          expr inner body body'
        (ExRec as body, ExRec bs body') -> do
          inner <- binders scope (map bindBinder as) (map bindBinder bs)
          zipWithM_ (\a b -> expr inner (bindRhs a) (bindRhs b)) as bs
          expr inner body body'
        (ExCase s a alts, ExCase s' b alts') -> do
          expr scope s s'
          inner <- case (a, b) of
            (Nothing, Nothing) -> pure scope
            (Just x, Just y) -> binder scope x y
            _ -> lift Nothing
          check (length alts == length alts')
          zipWithM_ (alternative inner) (NE.toList alts) (NE.toList alts')
        (ExAbsurd s a, ExAbsurd s' b) -> expr scope s s' >> ty scope a b
        (ExCast a x, ExCast b y) -> expr scope a b >> coercion scope x y
        (ExCoercion x, ExCoercion y) -> coercion scope x y
        (ExForeignCall a xs as, ExForeignCall b ys bs) -> do
          check (foreignCallName a == foreignCallName b && foreignCallConvention a == foreignCallConvention b && foreignCallDependencies a == foreignCallDependencies b)
          -- The foreign type is closed.
          ty (Scope Map.empty Map.empty) (foreignCallType a) (foreignCallType b)
          check (length xs == length ys && length as == length bs)
          zipWithM_ (ty scope) xs ys
          zipWithM_ (expr scope) as bs
        _ -> lift Nothing

    alternative :: Scope -> Alt -> Alt -> Match ()
    alternative scope l r = do
      case (altCon l, altCon r) of
        (AltData a, AltData b) -> check (a == b)
        (AltLit a, AltLit b) -> literal scope a b
        (AltDefault, AltDefault) -> pure ()
        _ -> lift Nothing
      typed <- binders scope (altTypeBinders l) (altTypeBinders r)
      inner <- binders typed (altBinders l) (altBinders r)
      expr inner (altRhs l) (altRhs r)

type Match = StateT (Map Name Name, Map Name Name) Maybe

-- | The names that the two expressions bind around the point of the
-- match: each left name with its right partner, and the reverse.
data Scope = Scope
  { scopeLeft :: !(Map Name Name),
    scopeRight :: !(Map Name Name)
  }

bindNames :: Scope -> Name -> Name -> Scope
bindNames scope l r = Scope (Map.insert l r (scopeLeft scope)) (Map.insert r l (scopeRight scope))

check :: Bool -> Match ()
check condition = unless condition (lift Nothing)
