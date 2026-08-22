-- | Scheme instantiation.
--
-- Instantiation replaces the quantified type variables in a 'TypeScheme'
-- with fresh meta-variables, and emits wanted constraints for the scheme's
-- predicates.
module Aihc.Tc.Instantiate
  ( Instantiation (..),
    instantiate,
    instantiateWithArgs,
    applySubst,
    substKind,
  )
where

import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (foldM)
import Data.Map.Strict qualified as Map

data Instantiation = Instantiation
  { instType :: !TcType,
    instTypeArgs :: ![TcType],
    instPreds :: ![Pred]
  }
  deriving (Eq, Show)

-- | Instantiate a type scheme.
--
-- Returns the instantiated monotype and the wanted predicates
-- (constraints that must be satisfied at the use site).
instantiate :: TypeScheme -> TcM (TcType, [Pred])
instantiate scheme = do
  inst <- instantiateWithArgs scheme
  pure (instType inst, instPreds inst)

instantiateWithArgs :: TypeScheme -> TcM Instantiation
instantiateWithArgs (ForAll tvs preds body) = do
  -- Allocate in binder order so later binder kinds can refer to earlier
  -- instantiations (for example @b :: TYPE r@).
  subst <- foldM extendSubst Map.empty tvs
  let substTy = applySubst subst
      body' = substTy body
      preds' = map (substPred subst) preds
      typeArgs = map (substTy . TcTyVar) tvs
  pure
    Instantiation
      { instType = body',
        instTypeArgs = typeArgs,
        instPreds = preds'
      }
  where
    extendSubst subst tv = do
      meta <- freshMetaTvOfKind (substKind subst (tvKind tv))
      pure (Map.insert (tvUnique tv) meta subst)

substKind :: Map.Map Unique TcType -> TcType -> TcType
substKind = applySubst

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
    go (TcAppTy f a) = applyType (go f) (go a)

    applyType (TcTyCon tc args) arg = TcTyCon tc (args <> [arg])
    applyType f arg = TcAppTy f arg

-- | Apply a substitution to a predicate.
substPred :: Map.Map Unique TcType -> Pred -> Pred
substPred subst (ClassPred cls args) = ClassPred cls (map (applySubst subst) args)
substPred subst (EqPred a b) = EqPred (applySubst subst a) (applySubst subst b)
