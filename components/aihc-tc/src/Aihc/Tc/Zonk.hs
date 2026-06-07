-- | Zonking: replace meta-variables with their solutions.
--
-- After solving, zonking replaces all meta-variables throughout the
-- type annotations. Any remaining unsolved meta-variables become
-- ambiguity errors or are defaulted.
module Aihc.Tc.Zonk
  ( zonkType,
    zonkPred,
    zonkTypeWithReport,
    zonkPredWithReport,
  )
where

import Aihc.Tc.Monad (TcM, TcSolveReport (..), readMetaTv)
import Aihc.Tc.Types
import Data.Map.Strict qualified as Map

-- | Zonk a type: chase meta-variable solutions to their final values.
zonkType :: TcType -> TcM TcType
zonkType ty = case ty of
  TcMetaTv u -> do
    mSol <- readMetaTv u
    case mSol of
      Nothing -> pure ty
      Just sol -> zonkType sol
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

-- | Pure zonking against a completed solve report.
--
-- This is used by lazy syntax annotations that are created while constraints
-- are generated, but only inspected after solving has produced the report.
zonkTypeWithReport :: TcSolveReport -> TcType -> TcType
zonkTypeWithReport report ty = case ty of
  TcMetaTv u ->
    case Map.lookup u (tcSolveReportMetaSolutions report) of
      Nothing -> ty
      Just sol -> zonkTypeWithReport report sol
  TcTyVar _ -> ty
  TcTyCon tc args -> TcTyCon tc (map (zonkTypeWithReport report) args)
  TcFunTy a b -> TcFunTy (zonkTypeWithReport report a) (zonkTypeWithReport report b)
  TcForAllTy tv body -> TcForAllTy tv (zonkTypeWithReport report body)
  TcQualTy preds body -> TcQualTy (map (zonkPredWithReport report) preds) (zonkTypeWithReport report body)
  TcAppTy f a -> TcAppTy (zonkTypeWithReport report f) (zonkTypeWithReport report a)

zonkPredWithReport :: TcSolveReport -> Pred -> Pred
zonkPredWithReport report pred' =
  case pred' of
    ClassPred cls args -> ClassPred cls (map (zonkTypeWithReport report) args)
    EqPred a b -> EqPred (zonkTypeWithReport report a) (zonkTypeWithReport report b)
