-- | Capture-avoiding substitution for System FC types.
--
-- Used by the lint pass when checking type application (@\forall a. \tau@)
-- instantiated with a concrete type.
module Aihc.Fc.Subst
  ( substType,
  )
where

import Aihc.Tc.Types (Pred (..), TcType (..), TyVarId (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

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
