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
    defaultTyConKindScheme,
    defaultPredKinds,
    defaultTyVarKinds,
    zonkErrorKind,
    finalizeDiagnostics,
  )
where

import Aihc.Tc.Constraint (EqProvenance (..), TypeTrace (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..))
import Aihc.Tc.Kind (defaultKindMetas, kindNeedsZonkIn, zonkKind)
import Aihc.Tc.Monad (TcM, TcState (..), getKinds, readMetaTv, readMetaTvKind, writeMetaTv)
import Aihc.Tc.Tidy (tidyDiagnostic)
import Aihc.Tc.Types
import Control.Monad ((>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, gets, modify')
import Data.IntMap.Strict qualified as IntMap
import Data.Text qualified as T

-- | Zonk a type: chase meta-variable solutions to their final values.
--
-- A type with nothing to chase is given back as it is. The checker zonks
-- the same types over and over -- before every unification, every
-- generalization and every annotation -- and a rebuilt copy of a settled
-- type is only heap for the collector to move. The question is decided by
-- a pure walk that allocates nothing.
zonkType :: TcType -> TcM TcType
zonkType ty = do
  state <- lift get
  if typeNeedsZonkIn state ty then rebuildZonkedType ty else pure ty

-- | Whether 'rebuildZonkedType' would change anything in a type: a solved
-- meta-variable, a kind that 'zonkKind' rewrites, or an application that
-- 'mkAppTy' normalises.
typeNeedsZonkIn :: TcState -> TcType -> Bool
typeNeedsZonkIn state ty = case ty of
  TcArrowTy -> False
  TcTyLit {} -> False
  TcMetaTv (Unique key) -> IntMap.member key (tcsMetaSolutions state)
  TcTyVar tv -> kindNeedsZonkIn state (tvKind tv)
  TcTyCon _ args -> any (typeNeedsZonkIn state) args
  TcKindedTyCon _ kindArgs -> any (kindNeedsZonkIn state) kindArgs
  TcFunTy a b -> typeNeedsZonkIn state a || typeNeedsZonkIn state b
  TcForAllTy tv body -> kindNeedsZonkIn state (tvKind tv) || typeNeedsZonkIn state body
  TcQualTy preds body -> any (predNeedsZonkIn state) preds || typeNeedsZonkIn state body
  TcAppTy f a -> normalises f || typeNeedsZonkIn state f || typeNeedsZonkIn state a
  where
    -- The shapes 'mkAppTy' rewrites rather than rebuilds.
    normalises f = case f of
      TcTyCon {} -> True
      TcAppTy TcArrowTy _ -> True
      _ -> False

-- | Whether 'rebuildZonkedPred' would change anything in a predicate.
predNeedsZonkIn :: TcState -> Pred -> Bool
predNeedsZonkIn state predicate = case predicate of
  ClassPred _ args -> any (typeNeedsZonkIn state) args
  EqPred a b -> typeNeedsZonkIn state a || typeNeedsZonkIn state b
  IParamPred _ payload -> typeNeedsZonkIn state payload
  IrredPred constraint -> typeNeedsZonkIn state constraint
  QuantifiedPred variables antecedents consequent ->
    any (kindNeedsZonkIn state . tvKind) variables
      || any (predNeedsZonkIn state) antecedents
      || predNeedsZonkIn state consequent

rebuildZonkedType :: TcType -> TcM TcType
rebuildZonkedType ty = case ty of
  TcArrowTy -> pure ty
  TcTyLit {} -> pure ty
  TcMetaTv u -> do
    mSol <- readMetaTv u
    case mSol of
      Nothing -> pure ty
      Just sol -> do
        state <- lift get
        -- A settled solution needs neither a new type nor another store write.
        if typeNeedsZonkIn state sol
          then do
            zonked <- rebuildZonkedType sol
            writeMetaTv u zonked
            pure zonked
          else pure sol
  TcTyVar tv -> TcTyVar <$> zonkTyVar tv
  TcTyCon tc args -> TcTyCon tc <$> mapM rebuildZonkedType args
  TcKindedTyCon tc kindArgs -> TcKindedTyCon tc <$> mapM zonkKind kindArgs
  TcFunTy a b -> TcFunTy <$> rebuildZonkedType a <*> rebuildZonkedType b
  TcForAllTy tv body -> TcForAllTy <$> zonkTyVar tv <*> rebuildZonkedType body
  TcQualTy preds body -> TcQualTy <$> mapM rebuildZonkedPred preds <*> rebuildZonkedType body
  TcAppTy f a -> mkAppTy <$> rebuildZonkedType f <*> rebuildZonkedType a

-- | Zonk a predicate, giving it back as it is when nothing changes.
zonkPred :: Pred -> TcM Pred
zonkPred predicate = do
  state <- lift get
  if predNeedsZonkIn state predicate then rebuildZonkedPred predicate else pure predicate

rebuildZonkedPred :: Pred -> TcM Pred
rebuildZonkedPred (ClassPred cls args) = ClassPred cls <$> mapM rebuildZonkedType args
rebuildZonkedPred (EqPred a b) = EqPred <$> rebuildZonkedType a <*> rebuildZonkedType b
rebuildZonkedPred (IParamPred name payload) = IParamPred name <$> rebuildZonkedType payload
rebuildZonkedPred (IrredPred constraint) = IrredPred <$> rebuildZonkedType constraint
rebuildZonkedPred (QuantifiedPred variables antecedents consequent) =
  QuantifiedPred <$> mapM zonkTyVar variables <*> mapM rebuildZonkedPred antecedents <*> rebuildZonkedPred consequent

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
    TcArrowTy -> pure ty
    TcTyLit {} -> pure ty
    TcTyVar tv -> TcTyVar <$> defaultTyVarKinds tv
    TcTyCon tyCon args -> TcTyCon tyCon <$> mapM defaultTypeKinds args
    TcKindedTyCon tyCon kindArgs -> TcKindedTyCon tyCon <$> mapM defaultKindArgument kindArgs
    TcFunTy argument result -> TcFunTy <$> defaultTypeKinds argument <*> defaultTypeKinds result
    TcForAllTy tv body -> TcForAllTy <$> defaultTyVarKinds tv <*> defaultTypeKinds body
    TcQualTy predicates body -> TcQualTy <$> mapM defaultPredKinds predicates <*> defaultTypeKinds body
    TcAppTy function argument -> mkAppTy <$> defaultTypeKinds function <*> defaultTypeKinds argument

-- | Settle one kind argument of a bare type constructor. Nothing at the use
-- site can have fixed it: @Proxy Tagged@ alone does not tell the kind of
-- @Tagged@. An open kind argument defaults by its own kind: a
-- representation to the lifted one, a levity to @Lifted@, and a kind to
-- 'Type'. An argument of another kind stays open.
defaultKindArgument :: TcType -> TcM TcType
defaultKindArgument argument = do
  zonked <- zonkKind argument
  case zonked of
    TcMetaTv unique -> do
      kinds <- getKinds
      metaKind <- readMetaTvKind unique >>= zonkKind
      let settled =
            case metaKind of
              KRuntimeRep -> Just (liftedRep kinds)
              KLevity -> Just (TcTyCon (kindsDataCon kinds (T.pack "Lifted") 0) [])
              KType -> Just (typeKind kinds)
              TcMetaTv {} -> Just (typeKind kinds)
              _ -> Nothing
      case settled of
        Just value -> writeMetaTv unique value >> pure value
        Nothing -> pure zonked
    _ -> defaultKindMetas zonked >>= zonkKind

defaultTypeSchemeKinds :: TypeScheme -> TcM TypeScheme
defaultTypeSchemeKinds = traverseScheme defaultTyVarKinds defaultPredKinds defaultTypeKinds

defaultTyConKindScheme :: TypeScheme -> TcM TypeScheme
defaultTyConKindScheme = traverseScheme defaultTyVarKinds defaultPredKinds (defaultKindMetas >=> zonkKind)

defaultPredKinds :: Pred -> TcM Pred
defaultPredKinds predicate =
  case predicate of
    ClassPred className args -> ClassPred className <$> mapM defaultTypeKinds args
    EqPred left right -> EqPred <$> defaultTypeKinds left <*> defaultTypeKinds right
    IParamPred name payload -> IParamPred name <$> defaultTypeKinds payload
    IrredPred constraint -> IrredPred <$> defaultTypeKinds constraint
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred <$> mapM defaultTyVarKinds variables <*> mapM defaultPredKinds antecedents <*> defaultPredKinds consequent

defaultTyVarKinds :: TyVarId -> TcM TyVarId
defaultTyVarKinds tv = do
  kind <- defaultKindMetas (tvKind tv) >>= zonkKind
  pure (setTyVarKind kind tv)

-- | Zonk the types in one error kind.
zonkErrorKind :: TcErrorKind -> TcM TcErrorKind
zonkErrorKind kind =
  case kind of
    UnificationError left right origin provenance ->
      UnificationError <$> zonkType left <*> zonkType right <*> pure origin <*> traverse zonkProvenance provenance
    OccursCheckError variable ty ->
      OccursCheckError <$> zonkType variable <*> zonkType ty
    KindMismatch expected actual ->
      KindMismatch <$> zonkType expected <*> zonkType actual
    UnsolvedWanted predicate origin ->
      UnsolvedWanted <$> zonkPred predicate <*> pure origin
    TopLevelUnliftedBinding name ty ->
      TopLevelUnliftedBinding name <$> zonkType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      RepresentationPolymorphicFunctionArgument name <$> zonkType ty
    InstanceFunDepCoverage predicate determiners determined ->
      InstanceFunDepCoverage <$> zonkPred predicate <*> pure determiners <*> pure determined
    InstanceFunDepConflict predicate other determiners determined ->
      InstanceFunDepConflict <$> zonkPred predicate <*> zonkPred other <*> pure determiners <*> pure determined
    FunDepUnknownTyVar {} -> pure kind
    UnboundVariable {} -> pure kind
    OtherError {} -> pure kind

zonkProvenance :: EqProvenance -> TcM EqProvenance
zonkProvenance provenance = do
  actual <- zonkTrace (eqActualTrace provenance)
  expected <- zonkTrace (eqExpectedTrace provenance)
  pure provenance {eqActualTrace = actual, eqExpectedTrace = expected}
  where
    zonkTrace trace = do
      ty <- zonkType (typeTraceType trace)
      pure trace {typeTraceType = ty}

-- | Zonk and tidy the collected diagnostics.
--
-- Run this before the diagnostics leave the type checker.
-- Zonking shows the solutions that the solver found after the diagnostic.
-- Tidying replaces internal meta-variable numbers with stable display names.
finalizeDiagnostics :: TcM ()
finalizeDiagnostics = do
  kinds <- getKinds
  diagnostics <- lift (gets tcsDiagnostics)
  zonked <- mapM zonkDiagnostic diagnostics
  lift (modify' (\state -> state {tcsDiagnostics = map (tidyDiagnostic kinds) zonked}))
  where
    zonkDiagnostic diagnostic = do
      kind <- zonkErrorKind (diagKind diagnostic)
      pure diagnostic {diagKind = kind}
