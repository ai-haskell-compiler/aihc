-- | Tidy diagnostics for display.
--
-- Meta-variable uniques are internal and not stable.
-- Before a diagnostic leaves the type checker, this module renames each
-- unsolved meta-variable to a display name such as @t0@.
-- The names follow the order of first appearance in the diagnostic.
-- The pass skips names that rigid type variables in the same diagnostic use.
-- This mirrors the GHC @tidyType@ pass for error messages.
module Aihc.Tc.Tidy
  ( tidyDiagnostic,
    tidyErrorKind,
    tidyTypes,
    tidyType,
  )
where

import Aihc.Tc.Constraint (EqProvenance (..), TypeTrace (..))
import Aihc.Tc.Error
import Aihc.Tc.Types
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

type TidyEnv = Map Unique TyVarId

-- | Rename the meta-variables of one diagnostic to display names.
tidyDiagnostic :: TcDiagnostic -> TcDiagnostic
tidyDiagnostic diagnostic =
  diagnostic {diagKind = tidyErrorKind (diagKind diagnostic)}

-- | Rename the meta-variables of one error kind to display names.
tidyErrorKind :: TcErrorKind -> TcErrorKind
tidyErrorKind kind =
  case kind of
    UnificationError left right origin provenance ->
      let env = mkTidyEnv (provenanceTypes provenance ++ [left, right]) []
       in UnificationError (tidyTypeWith env left) (tidyTypeWith env right) origin (tidyProvenance env <$> provenance)
    OccursCheckError variable ty ->
      let env = mkTidyEnv [variable, ty] []
       in OccursCheckError (tidyTypeWith env variable) (tidyTypeWith env ty)
    KindMismatch expected actual ->
      let env = mkTidyEnv [expected, actual] []
       in KindMismatch (tidyTypeWith env expected) (tidyTypeWith env actual)
    UnsolvedWanted predicate origin ->
      let env = mkTidyEnv [] [predicate]
       in UnsolvedWanted (tidyPredWith env predicate) origin
    TopLevelUnliftedBinding name ty ->
      TopLevelUnliftedBinding name (tidyType ty)
    RepresentationPolymorphicFunctionArgument name ty ->
      RepresentationPolymorphicFunctionArgument name (tidyType ty)
    UnboundVariable {} -> kind
    OtherError {} -> kind

-- | Rename the meta-variables of some types with one shared name supply.
tidyTypes :: [TcType] -> [TcType]
tidyTypes types = map (tidyTypeWith (mkTidyEnv types [])) types

-- | Rename the meta-variables of one type.
tidyType :: TcType -> TcType
tidyType ty = tidyTypeWith (mkTidyEnv [ty] []) ty

provenanceTypes :: Maybe EqProvenance -> [TcType]
provenanceTypes provenance =
  case provenance of
    Nothing -> []
    Just eq -> [typeTraceType (eqActualTrace eq), typeTraceType (eqExpectedTrace eq)]

tidyProvenance :: TidyEnv -> EqProvenance -> EqProvenance
tidyProvenance env provenance =
  provenance
    { eqActualTrace = tidyTrace (eqActualTrace provenance),
      eqExpectedTrace = tidyTrace (eqExpectedTrace provenance)
    }
  where
    tidyTrace trace = trace {typeTraceType = tidyTypeWith env (typeTraceType trace)}

mkTidyEnv :: [TcType] -> [Pred] -> TidyEnv
mkTidyEnv types predicates =
  Map.fromList (zip metas (zipWith TyVarId freshNames metas))
  where
    metas = orderedNub (concatMap typeMetas types ++ concatMap predMetas predicates)
    usedNames = Set.fromList (concatMap typeNames types ++ concatMap predNames predicates)
    freshNames = filter (`Set.notMember` usedNames) [T.pack ('t' : show n) | n <- [0 :: Int ..]]

orderedNub :: [Unique] -> [Unique]
orderedNub = go Set.empty
  where
    go _ [] = []
    go seen (unique : rest)
      | Set.member unique seen = go seen rest
      | otherwise = unique : go (Set.insert unique seen) rest

typeMetas :: TcType -> [Unique]
typeMetas ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcTyVar {} -> []
    TcTyCon _ args -> concatMap typeMetas args
    TcFunTy argument result -> typeMetas argument ++ typeMetas result
    TcForAllTy _ body -> typeMetas body
    TcQualTy preds body -> concatMap predMetas preds ++ typeMetas body
    TcAppTy function argument -> typeMetas function ++ typeMetas argument

predMetas :: Pred -> [Unique]
predMetas predicate =
  case predicate of
    ClassPred _ args -> concatMap typeMetas args
    EqPred left right -> typeMetas left ++ typeMetas right
    IParamPred _ payload -> typeMetas payload
    QuantifiedPred _ antecedents consequent -> concatMap predMetas antecedents ++ predMetas consequent

typeNames :: TcType -> [Text]
typeNames ty =
  case ty of
    TcMetaTv {} -> []
    TcTyVar tv -> [tvName tv]
    TcTyCon _ args -> concatMap typeNames args
    TcFunTy argument result -> typeNames argument ++ typeNames result
    TcForAllTy tv body -> tvName tv : typeNames body
    TcQualTy preds body -> concatMap predNames preds ++ typeNames body
    TcAppTy function argument -> typeNames function ++ typeNames argument

predNames :: Pred -> [Text]
predNames predicate =
  case predicate of
    ClassPred _ args -> concatMap typeNames args
    EqPred left right -> typeNames left ++ typeNames right
    IParamPred _ payload -> typeNames payload
    QuantifiedPred variables antecedents consequent ->
      map tvName variables ++ concatMap predNames antecedents ++ predNames consequent

tidyTypeWith :: TidyEnv -> TcType -> TcType
tidyTypeWith env ty =
  case ty of
    TcMetaTv unique -> maybe ty TcTyVar (Map.lookup unique env)
    TcTyVar {} -> ty
    TcTyCon tyCon args -> TcTyCon tyCon (map (tidyTypeWith env) args)
    TcFunTy argument result -> TcFunTy (tidyTypeWith env argument) (tidyTypeWith env result)
    TcForAllTy tv body -> TcForAllTy tv (tidyTypeWith env body)
    TcQualTy preds body -> TcQualTy (map (tidyPredWith env) preds) (tidyTypeWith env body)
    TcAppTy function argument -> mkAppTy (tidyTypeWith env function) (tidyTypeWith env argument)

tidyPredWith :: TidyEnv -> Pred -> Pred
tidyPredWith env predicate =
  case predicate of
    ClassPred tyCon args -> ClassPred tyCon (map (tidyTypeWith env) args)
    EqPred left right -> EqPred (tidyTypeWith env left) (tidyTypeWith env right)
    IParamPred name payload -> IParamPred name (tidyTypeWith env payload)
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred variables (map (tidyPredWith env) antecedents) (tidyPredWith env consequent)
