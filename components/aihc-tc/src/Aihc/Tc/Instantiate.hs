-- | Scheme instantiation.
--
-- Instantiation replaces the quantified type variables in a 'TypeScheme'
-- with fresh meta-variables, and emits wanted constraints for the scheme's
-- predicates.
module Aihc.Tc.Instantiate
  ( instantiate,
    applySubst,
  )
where

import Aihc.Tc.Monad
import Aihc.Tc.Types
import Data.Map.Strict qualified as Map

-- | Instantiate a type scheme.
--
-- Returns the instantiated monotype and the wanted predicates
-- (constraints that must be satisfied at the use site).
instantiate :: TypeScheme -> TcM (TcType, [Pred])
instantiate (ForAll tvs preds body) = do
  -- Create a fresh meta-variable for each quantified type variable.
  subst <- Map.fromList <$> mapM mkSubst tvs
  let substTy = applySubst subst
  let body' = substTy body
  let preds' = map (substPred subst) preds
  pure (body', preds')
  where
    mkSubst tv = do
      meta <- freshMetaTv
      pure (tvUnique tv, meta)

-- | Apply a substitution (from TyVar uniques to types) to a type.
applySubst :: Map.Map Unique TcType -> TcType -> TcType
applySubst subst = go
  where
    go (TcTyVar tv) =
      case Map.lookup (tvUnique tv) subst of
        Just ty -> ty
        Nothing -> TcTyVar tv
    go (TcMetaTv u) = TcMetaTv u
    go (TcTyCon tc args) = TcTyCon tc (map go args)
    go (TcFunTy a b) = TcFunTy (go a) (go b)
    go (TcForAllTy tv body) =
      -- Do not substitute under the binder if it shadows.
      let subst' = Map.delete (tvUnique tv) subst
       in TcForAllTy tv (applySubst subst' body)
    go (TcQualTy preds body) =
      TcQualTy (map (substPred subst) preds) (go body)
    go (TcAppTy f a) = TcAppTy (go f) (go a)

-- | Apply a substitution to a predicate.
substPred :: Map.Map Unique TcType -> Pred -> Pred
substPred subst (ClassPred cls args) = ClassPred cls (map (applySubst subst) args)
substPred subst (EqPred a b) = EqPred (applySubst subst a) (applySubst subst b)
