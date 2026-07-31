{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize strategy-specific deriving contexts as a module batch.
--
-- AnyClass has no structural method obligations. Its inferred context comes
-- from instantiated superclasses and the constraints on default signatures.
-- The whole batch is visible while simplifying those predicates so sibling
-- AnyClass instances are independent of declaration order.
module Aihc.Tc.Deriving.Context
  ( finalizeDerivingModulesTc,
    derivingPlanInstanceInfo,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Module (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Tc.Annotations
  ( TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
  )
import Aihc.Tc.Constraint (Ct (..), CtOrigin (..), mkWantedCt)
import Aihc.Tc.Env (InstanceInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Instantiate (applySubst)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Dict (DictResult (..), constraintTypeToPred, matchTypes, solveDictWithGivens, substPred)
import Aihc.Tc.Types
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

finalizeDerivingModulesTc :: [Module] -> TcM [Module]
finalizeDerivingModulesTc modules = do
  existingInstances <- getInstances
  let originalPlans = concatMap moduleDerivingPlans modules
  contextPlans <- mapM (inferPlanContext existingInstances originalPlans) originalPlans
  let derivedInstances = mapMaybe derivingPlanInstanceInfo contextPlans
  mapM_ addInstance derivedInstances
  evidencePlans <- mapM attachDerivingEvidence contextPlans
  pure (map (replaceModulePlans evidencePlans) modules)

inferPlanContext :: [InstanceInfo] -> [TcDerivingPlan] -> TcDerivingPlan -> TcM TcDerivingPlan
inferPlanContext existingInstances plans plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (TcDerivingAnyclass, TcDerivingInferContext) ->
      case inferAnyClassContext existingInstances plans [] plan of
        Left predicate -> do
          emitError
            (tcDerivingSourceSpan plan)
            (UnsolvedWanted predicate (InstOrigin (tcDerivingClassName plan)))
          pure plan
        Right context ->
          pure plan {tcDerivingContext = TcDerivingExplicitContext context}
    _ -> pure plan

inferAnyClassContext :: [InstanceInfo] -> [TcDerivingPlan] -> [PlanKey] -> TcDerivingPlan -> Either Pred [Pred]
inferAnyClassContext existingInstances plans stack plan
  | key `elem` stack = Left (planPredicate plan)
  | otherwise =
      nub . concat
        <$> mapM
          (simplifyPredicate existingInstances plans (key : stack) plan)
          (anyClassObligations plan)
  where
    key = planKey plan

simplifyPredicate :: [InstanceInfo] -> [TcDerivingPlan] -> [PlanKey] -> TcDerivingPlan -> Pred -> Either Pred [Pred]
simplifyPredicate existingInstances plans stack owner predicate
  | isBareVariablePredicate (tcDerivingTyVars owner) predicate = Right [predicate]
  | Just arguments <- typeableArguments predicate =
      concat
        <$> mapM
          (simplifyPredicate existingInstances plans stack owner . ClassPred "Typeable" . (: []))
          arguments
  | otherwise =
      case firstSuccessful (map simplifyExisting matchingExisting <> map simplifyDerived matchingDerived) of
        Just context -> Right context
        Nothing
          | isAdmissibleContextPredicate owner predicate -> Right [predicate]
          | otherwise -> Left predicate
  where
    matchingExisting =
      [ (instanceInfo, substitution)
      | instanceInfo <- existingInstances,
        iiClassName instanceInfo == predClassName predicate,
        Just substitution <- [matchTypes (iiHead instanceInfo) (predArguments predicate)]
      ]
    matchingDerived =
      [ (candidate, substitution)
      | candidate <- plans,
        tcDerivingClassName candidate == predClassName predicate,
        Just substitution <- [matchTypes (tcDerivingHeadTypes candidate) (predArguments predicate)]
      ]
    simplifyExisting (instanceInfo, substitution) =
      concat
        <$> mapM
          (simplifyPredicate existingInstances plans stack owner . substPred substitution)
          (iiContext instanceInfo)
    simplifyDerived (candidate, substitution) = do
      context <- candidateContext candidate
      concat
        <$> mapM
          (simplifyPredicate existingInstances plans stack owner . substPred substitution)
          context
    candidateContext candidate =
      case tcDerivingContext candidate of
        TcDerivingExplicitContext context -> Right context
        TcDerivingInferContext
          | tcDerivingStrategy candidate == TcDerivingAnyclass ->
              inferAnyClassContext existingInstances plans stack candidate
          | otherwise -> Left predicate

firstSuccessful :: [Either error value] -> Maybe value
firstSuccessful results =
  case results of
    [] -> Nothing
    Left _ : rest -> firstSuccessful rest
    Right value : _ -> Just value

anyClassObligations :: TcDerivingPlan -> [Pred]
anyClassObligations plan =
  mapMaybe
    (constraintTypeToPred . applySubst substitution . tcDictBinderType)
    (tcDerivingClassSuperClasses plan)
    <> concatMap snd (instantiatedDefaultSignaturePredicates plan)
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]

attachDerivingEvidence :: TcDerivingPlan -> TcM TcDerivingPlan
attachDerivingEvidence plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (TcDerivingAnyclass, TcDerivingExplicitContext context) -> do
      superClassEvidence <- mapM (solveObligation context) superClasses
      defaultMethodEvidence <-
        mapM
          (traverse (mapM (solveObligation context)))
          (instantiatedDefaultSignaturePredicates plan)
      pure
        plan
          { tcDerivingSuperClasses = zip (map predDictBinder superClasses) superClassEvidence,
            tcDerivingDefaultMethodEvidence = defaultMethodEvidence
          }
    _ -> pure plan
  where
    superClasses =
      mapMaybe
        (constraintTypeToPred . applySubst substitution . tcDictBinderType)
        (tcDerivingClassSuperClasses plan)
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]
    solveObligation context predicate = do
      evidenceVariable <- freshEvVar
      let constraint = mkWantedCt predicate evidenceVariable (InstOrigin (tcDerivingClassName plan)) (tcDerivingSourceSpan plan)
      result <- solveDictWithGivens context constraint
      case result of
        DictSolved -> do
          evidence <- lookupEvidence evidenceVariable
          case evidence of
            Just term -> pure term
            Nothing -> pure (EvVarTerm evidenceVariable)
        DictStuck stuck -> do
          emitError (ctLoc stuck) (UnsolvedWanted (ctPred stuck) (ctOrigin stuck))
          pure (EvVarTerm evidenceVariable)

instantiatedDefaultSignaturePredicates :: TcDerivingPlan -> [(Text, [Pred])]
instantiatedDefaultSignaturePredicates plan =
  [ (methodName, map (substPred substitution) predicates)
  | (methodName, predicates) <- tcDerivingDefaultSignatures plan,
    methodName `elem` tcDerivingDefaultMethods plan
  ]
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]

derivingPlanInstanceInfo :: TcDerivingPlan -> Maybe InstanceInfo
derivingPlanInstanceInfo plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (TcDerivingAnyclass, TcDerivingExplicitContext context) ->
      Just
        InstanceInfo
          { iiClassName = tcDerivingClassName plan,
            iiDictName = tcDerivingDictName plan,
            iiDictType = foldr TcForAllTy (TcQualTy context (predType (planPredicate plan))) (tcDerivingTyVars plan),
            iiTyVars = tcDerivingTyVars plan,
            iiContext = context,
            iiHead = tcDerivingHeadTypes plan
          }
    _ -> Nothing

moduleDerivingPlans :: Module -> [TcDerivingPlan]
moduleDerivingPlans = concatMap declDerivingPlans . moduleDecls

declDerivingPlans :: Decl -> [TcDerivingPlan]
declDerivingPlans decl =
  case decl of
    DeclAnn annotation inner ->
      maybe [] tcDerivingPlans (fromAnnotation @TcDerivingAnnotation annotation)
        <> declDerivingPlans inner
    _ -> []

replaceModulePlans :: [TcDerivingPlan] -> Module -> Module
replaceModulePlans plans modu =
  modu {moduleDecls = map replaceDecl (moduleDecls modu)}
  where
    replaceDecl decl =
      case decl of
        DeclAnn annotation inner
          | Just derivingAnnotation <- fromAnnotation @TcDerivingAnnotation annotation ->
              DeclAnn
                (mkAnnotation (derivingAnnotation {tcDerivingPlans = map replacePlan (tcDerivingPlans derivingAnnotation)}))
                (replaceDecl inner)
          | otherwise -> DeclAnn annotation (replaceDecl inner)
        _ -> decl
    replacePlan original =
      fromMaybe original (find ((== planKey original) . planKey) plans)

type PlanKey = (Text, [TcType])

planKey :: TcDerivingPlan -> PlanKey
planKey plan = (tcDerivingClassName plan, tcDerivingHeadTypes plan)

planPredicate :: TcDerivingPlan -> Pred
planPredicate plan = ClassPred (tcDerivingClassName plan) (tcDerivingHeadTypes plan)

predClassName :: Pred -> Text
predClassName predicate =
  case predicate of
    ClassPred className _ -> className
    EqPred {} -> "~"

predArguments :: Pred -> [TcType]
predArguments predicate =
  case predicate of
    ClassPred _ arguments -> arguments
    EqPred left right -> [left, right]

typeableArguments :: Pred -> Maybe [TcType]
typeableArguments predicate =
  case predicate of
    ClassPred "Typeable" [ty] ->
      case ty of
        TcTyCon _ arguments -> Just arguments
        TcFunTy argument result -> Just [argument, result]
        TcTyVar {} -> Nothing
        TcMetaTv {} -> Nothing
        TcForAllTy {} -> Nothing
        TcQualTy {} -> Nothing
        TcAppTy {} -> Nothing
    _ -> Nothing

isBareVariablePredicate :: [TyVarId] -> Pred -> Bool
isBareVariablePredicate tyVars predicate =
  case predicate of
    ClassPred _ arguments ->
      not (null arguments)
        && all isPlanTyVar arguments
    EqPred {} -> False
  where
    isPlanTyVar (TcTyVar tyVar) = tyVar `elem` tyVars
    isPlanTyVar _ = False

isAdmissibleContextPredicate :: TcDerivingPlan -> Pred -> Bool
isAdmissibleContextPredicate plan predicate =
  not (null mentionedVariables)
    && all (`elem` tcDerivingTyVars plan) mentionedVariables
    && maybe True (not . predicateMentionsTyCon predicate) (derivedTargetTyCon plan)
  where
    mentionedVariables = predTyVars predicate

derivedTargetTyCon :: TcDerivingPlan -> Maybe Text
derivedTargetTyCon plan =
  case reverse (tcDerivingHeadTypes plan) of
    target : _ -> typeHeadTyCon target
    [] -> Nothing

typeHeadTyCon :: TcType -> Maybe Text
typeHeadTyCon ty =
  case ty of
    TcTyCon tyCon _ -> Just (tyConName tyCon)
    TcAppTy function _ -> typeHeadTyCon function
    _ -> Nothing

predicateMentionsTyCon :: Pred -> Text -> Bool
predicateMentionsTyCon predicate name =
  any (typeMentionsTyCon name) (predArguments predicate)

typeMentionsTyCon :: Text -> TcType -> Bool
typeMentionsTyCon name ty =
  case ty of
    TcTyVar {} -> False
    TcMetaTv {} -> False
    TcTyCon tyCon arguments -> tyConName tyCon == name || any (typeMentionsTyCon name) arguments
    TcFunTy argument result -> typeMentionsTyCon name argument || typeMentionsTyCon name result
    TcForAllTy _ body -> typeMentionsTyCon name body
    TcQualTy predicates body -> any (`predicateMentionsTyCon` name) predicates || typeMentionsTyCon name body
    TcAppTy function argument -> typeMentionsTyCon name function || typeMentionsTyCon name argument

predTyVars :: Pred -> [TyVarId]
predTyVars predicate = nub (concatMap typeTyVars (predArguments predicate))

typeTyVars :: TcType -> [TyVarId]
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> [tyVar]
    TcMetaTv {} -> []
    TcTyCon _ arguments -> concatMap typeTyVars arguments
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> filter (/= tyVar) (typeTyVars body)
    TcQualTy predicates body -> concatMap predTyVars predicates <> typeTyVars body
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

predDictBinder :: Pred -> TcDictBinderAnnotation
predDictBinder predicate =
  case predicate of
    ClassPred className arguments ->
      TcDictBinderAnnotation className arguments (predType predicate)
    EqPred {} -> TcDictBinderAnnotation "<constraint>" [] (predType predicate)

predType :: Pred -> TcType
predType predicate =
  case predicate of
    ClassPred className arguments -> TcTyCon (TyCon className (length arguments)) arguments
    EqPred left right -> TcTyCon (TyCon "~" 2) [left, right]
