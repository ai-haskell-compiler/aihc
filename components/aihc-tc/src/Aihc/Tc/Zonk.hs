-- | Zonking: replace meta-variables with their solutions.
--
-- After solving, zonking replaces all meta-variables throughout the
-- type annotations. Any remaining unsolved meta-variables become
-- ambiguity errors or are defaulted.
module Aihc.Tc.Zonk
  ( zonkType,
    zonkPred,
    defaultTypeKinds,
    defaultTypeSchemeKinds,
    defaultPredKinds,
    defaultTyVarKinds,
  )
where

import Aihc.Tc.Kind (defaultKindMetas, zonkKind)
import Aihc.Tc.Monad (TcM, readMetaTv)
import Aihc.Tc.Types

-- | Zonk a type: chase meta-variable solutions to their final values.
zonkType :: TcType -> TcM TcType
zonkType ty = case ty of
  TcMetaTv u -> do
    mSol <- readMetaTv u
    case mSol of
      Nothing -> pure ty
      Just sol -> zonkType sol
  TcTyVar tv -> TcTyVar <$> zonkTyVar tv
  TcTyCon tc args -> do
    kind <- zonkKind (tyConKind tc)
    TcTyCon (mkTyCon (tyConName tc) (tyConArity tc) kind) <$> mapM zonkType args
  TcFunTy a b -> TcFunTy <$> zonkType a <*> zonkType b
  TcForAllTy tv body -> TcForAllTy <$> zonkTyVar tv <*> zonkType body
  TcQualTy preds body -> TcQualTy <$> mapM zonkPred preds <*> zonkType body
  TcAppTy f a -> TcAppTy <$> zonkType f <*> zonkType a

-- | Zonk a predicate.
zonkPred :: Pred -> TcM Pred
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b

zonkTyVar :: TyVarId -> TcM TyVarId
zonkTyVar tv = do
  kind <- zonkKind (tvKind tv)
  pure (setTyVarKind kind tv)

-- | Finalize every kind embedded in a type. Unlike ordinary zonking, this
-- defaults unconstrained kind metavariables to 'Type', so it must only run at
-- a module/interface boundary after kind constraints have been solved.
defaultTypeKinds :: TcType -> TcM TcType
defaultTypeKinds ty =
  case ty of
    TcMetaTv {} -> pure ty
    TcTyVar tv -> TcTyVar <$> defaultTyVarKinds tv
    TcTyCon tyCon args -> do
      kind <- defaultKindMetas (tyConKind tyCon)
      let tyCon' = mkTyCon (tyConName tyCon) (tyConArity tyCon) kind
      TcTyCon tyCon' <$> mapM defaultTypeKinds args
    TcFunTy argument result -> TcFunTy <$> defaultTypeKinds argument <*> defaultTypeKinds result
    TcForAllTy tv body -> TcForAllTy <$> defaultTyVarKinds tv <*> defaultTypeKinds body
    TcQualTy predicates body -> TcQualTy <$> mapM defaultPredKinds predicates <*> defaultTypeKinds body
    TcAppTy function argument -> TcAppTy <$> defaultTypeKinds function <*> defaultTypeKinds argument

defaultTypeSchemeKinds :: TypeScheme -> TcM TypeScheme
defaultTypeSchemeKinds (ForAll tyVars predicates body) =
  ForAll
    <$> mapM defaultTyVarKinds tyVars
    <*> mapM defaultPredKinds predicates
    <*> defaultTypeKinds body

defaultPredKinds :: Pred -> TcM Pred
defaultPredKinds predicate =
  case predicate of
    ClassPred className args -> ClassPred className <$> mapM defaultTypeKinds args
    EqPred left right -> EqPred <$> defaultTypeKinds left <*> defaultTypeKinds right

defaultTyVarKinds :: TyVarId -> TcM TyVarId
defaultTyVarKinds tv = do
  kind <- defaultKindMetas (tvKind tv)
  pure (setTyVarKind kind tv)
