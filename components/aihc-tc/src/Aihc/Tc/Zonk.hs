-- | Zonking: replace meta-variables with their solutions.
--
-- After solving, zonking replaces all meta-variables throughout the
-- type annotations. Any remaining unsolved meta-variables become
-- ambiguity errors or are defaulted.
module Aihc.Tc.Zonk
  ( zonkType,
    zonkPred,
  )
where

import Aihc.Tc.Monad (TcM, readMetaTv)
import Aihc.Tc.Types

-- | Zonk a type: chase meta-variable solutions to their final values.
zonkType :: TcType -> TcM TcType
zonkType ty = case ty of
  TcMetaTv u -> do
    mSol <- readMetaTv u
    case mSol of
      Nothing -> pure ty
      Just sol -> do
        sol' <- zonkType sol
        pure sol'
  TcTyVar _ -> pure ty
  TcTyCon tc args -> TcTyCon tc <$> mapM zonkType args
  TcFunTy a b -> TcFunTy <$> zonkType a <*> zonkType b
  TcForAllTy tv body -> TcForAllTy tv <$> zonkType body
  TcQualTy preds body -> TcQualTy <$> mapM zonkPred preds <*> zonkType body
  TcAppTy f a -> TcAppTy <$> zonkType f <*> zonkType a

-- | Zonk a predicate.
zonkPred :: Pred -> TcM Pred
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b
