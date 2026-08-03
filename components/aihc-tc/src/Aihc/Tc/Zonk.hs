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

import Aihc.Tc.Kind (defaultKindMetas, runtimeRepToTcType, zonkKind)
import Aihc.Tc.Monad (TcM, readMetaTv, readRuntimeRepDependency, writeMetaTv)
import Aihc.Tc.Types

-- | Zonk a type: chase meta-variable solutions to their final values.
zonkType :: TcType -> TcM TcType
zonkType ty = case ty of
  TcMetaTv u -> do
    mSol <- readMetaTv u
    resolved <- case mSol of
      Nothing -> pure ty
      Just sol -> zonkType sol
    resolveRuntimeRepDependency u resolved
  TcTyVar tv -> TcTyVar <$> zonkTyVar tv
  TcTyCon tc args -> do
    kind <- zonkKind (tyConKind tc)
    TcTyCon (mkTyCon (tyConName tc) (tyConArity tc) kind) <$> mapM zonkType args
  TcFunTy a b -> TcFunTy <$> zonkType a <*> zonkType b
  TcForAllTy tv body -> TcForAllTy <$> zonkTyVar tv <*> zonkType body
  TcQualTy preds body -> TcQualTy <$> mapM zonkPred preds <*> zonkType body
  TcAppTy f a -> TcAppTy <$> zonkType f <*> zonkType a

resolveRuntimeRepDependency :: Unique -> TcType -> TcM TcType
resolveRuntimeRepDependency unique unresolved@(TcMetaTv unresolvedUnique)
  | unique == unresolvedUnique = do
      dependency <- readRuntimeRepDependency unique
      case dependency of
        Nothing -> pure unresolved
        Just representedType -> do
          representedType' <- zonkType representedType
          if containsMetaVariable representedType'
            then pure unresolved
            else case runtimeRepOfType representedType' of
              Left _ -> pure unresolved
              Right runtimeRep -> do
                let solution = runtimeRepToTcType runtimeRep
                writeMetaTv unique solution
                pure solution
resolveRuntimeRepDependency _ resolved = pure resolved

containsMetaVariable :: TcType -> Bool
containsMetaVariable ty =
  case ty of
    TcMetaTv {} -> True
    TcTyVar {} -> False
    TcTyCon _ arguments -> any containsMetaVariable arguments
    TcFunTy argument result -> containsMetaVariable argument || containsMetaVariable result
    TcForAllTy _ body -> containsMetaVariable body
    TcQualTy predicates body -> any containsMetaPred predicates || containsMetaVariable body
    TcAppTy function argument -> containsMetaVariable function || containsMetaVariable argument
  where
    containsMetaPred predicate =
      case predicate of
        ClassPred _ arguments -> any containsMetaVariable arguments
        EqPred left right -> containsMetaVariable left || containsMetaVariable right

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
