{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize strategy-specific deriving contexts as a module batch.
--
-- AnyClass contexts come from instantiated superclasses and default
-- signatures. Stock Eq contexts come from checked constructor fields. The
-- whole batch is visible while simplifying those predicates so recursive and
-- mutually recursive derived dictionaries are independent of source order.
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
  ( TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
    TcStockDerivingPlan (..),
  )
import Aihc.Tc.Constraint (Ct (..), CtOrigin (..), mkWantedCt)
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..), InstanceInfo (..), TyConFlavor (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Dict (DictResult (..), matchTypes, solveDictWithGivens)
import Aihc.Tc.Types
import Data.List (find, nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)

finalizeDerivingModulesTc :: [(Text, Text)] -> [Module] -> TcM [Module]
finalizeDerivingModulesTc moduleOrigins modules = do
  existingInstances <- getInstances
  let originalPlans = concatMap moduleDerivingPlans modules
      originalOrigins = concat (zipWith (\origin modu -> replicate (length (moduleDerivingPlans modu)) origin) moduleOrigins modules)
  contextPlans <- mapM (inferPlanContext existingInstances originalPlans) originalPlans
  let derivedInstances = mapMaybe (uncurry derivingPlanInstanceInfo) (zip originalOrigins contextPlans)
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
    (TcDerivingStock, context)
      | tcDerivingClassName plan == "Eq" ->
          case stockEqObligations plan of
            Left message -> do
              emitError (tcDerivingSourceSpan plan) (OtherError message)
              pure plan
            Right obligations ->
              case context of
                TcDerivingExplicitContext {} -> pure plan
                TcDerivingInferContext ->
                  case inferStockEqContext existingInstances plans [] plan obligations of
                    Left predicate -> do
                      emitError
                        (tcDerivingSourceSpan plan)
                        (UnsolvedWanted predicate (InstOrigin "Eq"))
                      pure plan
                    Right inferred ->
                      pure plan {tcDerivingContext = TcDerivingExplicitContext inferred}
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

inferStockEqContext :: [InstanceInfo] -> [TcDerivingPlan] -> [PlanKey] -> TcDerivingPlan -> [[Pred]] -> Either Pred [Pred]
inferStockEqContext existingInstances plans stack plan obligations
  | key `elem` stack = Right []
  | otherwise =
      nub . concat
        <$> mapM
          (simplifyPredicate existingInstances plans (key : stack) plan)
          (concat obligations)
  where
    key = planKey plan

simplifyPredicate :: [InstanceInfo] -> [TcDerivingPlan] -> [PlanKey] -> TcDerivingPlan -> Pred -> Either Pred [Pred]
simplifyPredicate existingInstances plans stack owner predicate
  | isBareVariablePredicate (tcDerivingTyVars owner) predicate = Right [predicate]
  | Just key <- predicatePlanKey predicate,
    key `elem` stack =
      Right []
  | ClassPred typeableTyCon _ <- predicate,
    Just arguments <- typeableArguments predicate =
      concat
        <$> mapM
          (simplifyPredicate existingInstances plans stack owner . ClassPred typeableTyCon . (: []))
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
          (simplifyPredicate existingInstances plans stack owner . applySubstPred substitution)
          (iiContext instanceInfo)
    simplifyDerived (candidate, substitution) = do
      context <- candidateContext candidate
      concat
        <$> mapM
          (simplifyPredicate existingInstances plans stack owner . applySubstPred substitution)
          context
    candidateContext candidate =
      case tcDerivingContext candidate of
        TcDerivingExplicitContext context -> Right context
        TcDerivingInferContext
          | tcDerivingStrategy candidate == TcDerivingAnyclass ->
              inferAnyClassContext existingInstances plans stack candidate
          | tcDerivingStrategy candidate == TcDerivingStock,
            tcDerivingClassName candidate == "Eq",
            Right obligations <- stockEqObligations candidate ->
              inferStockEqContext existingInstances plans stack candidate obligations
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
      superClassBinders <- mapM predDictBinder superClasses
      defaultMethodEvidence <-
        mapM
          (traverse (mapM (solveObligation context)))
          (instantiatedDefaultSignaturePredicates plan)
      pure
        plan
          { tcDerivingSuperClasses = zip superClassBinders superClassEvidence,
            tcDerivingDefaultMethodEvidence = defaultMethodEvidence
          }
    (TcDerivingStock, TcDerivingExplicitContext context)
      | tcDerivingClassName plan == "Eq",
        Right obligations <- stockEqObligations plan -> do
          fieldEvidence <- mapM (mapM (solveObligation context)) obligations
          pure plan {tcDerivingStockPlan = Just (TcStockEqPlan fieldEvidence)}
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
  [ (methodName, map (applySubstPred substitution) predicates)
  | (methodName, predicates) <- tcDerivingDefaultSignatures plan,
    methodName `elem` tcDerivingDefaultMethods plan
  ]
  where
    substitution =
      Map.fromList
        [ (tvUnique tyVar, headType)
        | (tyVar, headType) <- zip (tcDerivingClassTyVars plan) (tcDerivingHeadTypes plan)
        ]

derivingPlanInstanceInfo :: (Text, Text) -> TcDerivingPlan -> Maybe InstanceInfo
derivingPlanInstanceInfo origin plan =
  case (tcDerivingStrategy plan, tcDerivingContext plan) of
    (strategy, TcDerivingExplicitContext context)
      | strategy == TcDerivingAnyclass || isValidStockEqPlan plan ->
          Just
            InstanceInfo
              { iiClassName = tcDerivingClassName plan,
                iiDictName = tcDerivingDictName plan,
                iiDictOrigin = origin,
                iiDictType = foldr TcForAllTy (TcQualTy context (planPredicateType plan)) (tcDerivingTyVars plan),
                iiTyVars = tcDerivingTyVars plan,
                iiContext = context,
                iiHead = tcDerivingHeadTypes plan
              }
    _ -> Nothing

isValidStockEqPlan :: TcDerivingPlan -> Bool
isValidStockEqPlan plan =
  tcDerivingStrategy plan == TcDerivingStock
    && tcDerivingClassName plan == "Eq"
    && case stockEqObligations plan of
      Right {} -> True
      Left {} -> False

stockEqObligations :: TcDerivingPlan -> Either String [[Pred]]
stockEqObligations plan = do
  dataType <-
    maybe
      (Left "stock Eq deriving requires checked datatype metadata")
      Right
      (tcDerivingDataType plan)
  targetArguments <- stockEqTargetArguments dataType plan
  validateStockEqClass plan
  validateStockEqDataType dataType
  let substitution =
        Map.fromList
          [ (tvUnique tyVar, argument)
          | (tyVar, argument) <- zip (dtiTyVars dataType) targetArguments
          ]
  pure
    [ [ClassPred (tcDerivingClassTyCon plan) [applySubst substitution (dcfiType field)] | field <- dciFields constructor]
    | constructor <- dtiConstructors dataType
    ]

stockEqTargetArguments :: DataTypeInfo -> TcDerivingPlan -> Either String [TcType]
stockEqTargetArguments dataType plan =
  case reverse (tcDerivingHeadTypes plan) of
    TcTyCon tyCon arguments : _
      | tyConName tyCon == dtiName dataType,
        length arguments == length (dtiTyVars dataType) ->
          Right arguments
    _ -> Left "stock Eq deriving target does not match its checked datatype metadata"

validateStockEqClass :: TcDerivingPlan -> Either String ()
validateStockEqClass plan
  | [_] <- tcDerivingClassTyVars plan,
    null (tcDerivingClassSuperClasses plan),
    map tcClassMethodName (tcDerivingClassMethods plan) == ["==", "/="],
    all validMethod (tcDerivingClassMethods plan) =
      Right ()
  | otherwise = Left "stock Eq deriving requires the standard Eq class layout"
  where
    validMethod method =
      tcClassMethodName method `elem` ["==", "/="]
        && case methodTypeParts (tcClassMethodType method) of
          ( [classVar],
            [ClassPred eqTyCon [TcTyVar predicateVar]],
            TcFunTy (TcTyVar left) (TcFunTy (TcTyVar right) (TcTyCon boolTyCon []))
            ) ->
              tyConName eqTyCon == "Eq"
                && [classVar] == tcDerivingClassTyVars plan
                && predicateVar == classVar
                && left == classVar
                && right == classVar
                && tyConName boolTyCon == "Bool"
          _ -> False

    methodTypeParts ty =
      let (tyVars, qualified) = peelMethodForAlls ty
       in case qualified of
            TcQualTy predicates body -> (tyVars, predicates, body)
            body -> (tyVars, [], body)

    peelMethodForAlls (TcForAllTy tyVar body) =
      let (tyVars, inner) = peelMethodForAlls body
       in (tyVar : tyVars, inner)
    peelMethodForAlls ty = ([], ty)

validateStockEqDataType :: DataTypeInfo -> Either String ()
validateStockEqDataType dataType
  | dtiFlavor dataType `notElem` [DataTyCon, NewtypeTyCon] =
      Left "stock Eq deriving requires a data or newtype declaration"
  | null constructors =
      Left "stock Eq deriving does not yet support empty data declarations"
  | not (all (null . dciExTyVars) constructors) =
      Left "stock Eq deriving does not yet support existential constructors"
  | not (all (null . dciTheta) constructors) =
      Left "stock Eq deriving does not yet support constrained constructors"
  | any ((/= expectedResult) . dciResTy) constructors =
      Left "stock Eq deriving does not yet support refined GADT result types"
  | otherwise = Right ()
  where
    constructors = dtiConstructors dataType
    expectedResult = TcTyCon (dtiTyCon dataType) (map TcTyVar (dtiTyVars dataType))

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

predicatePlanKey :: Pred -> Maybe PlanKey
predicatePlanKey predicate =
  case predicate of
    ClassPred className arguments -> Just (tyConName className, arguments)
    EqPred {} -> Nothing
    QuantifiedPred {} -> Nothing
    IParamPred {} -> Nothing

planPredicate :: TcDerivingPlan -> Pred
planPredicate plan = ClassPred (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan)

predClassName :: Pred -> Text
predClassName predicate =
  case predicate of
    ClassPred className _ -> tyConName className
    EqPred {} -> "~"
    QuantifiedPred {} -> "quantified"
    IParamPred name _ -> name

predArguments :: Pred -> [TcType]
predArguments predicate =
  case predicate of
    ClassPred _ arguments -> arguments
    EqPred left right -> [left, right]
    QuantifiedPred _ antecedents consequent -> concatMap predArguments antecedents <> predArguments consequent
    IParamPred _ payload -> [payload]

typeableArguments :: Pred -> Maybe [TcType]
typeableArguments predicate =
  case predicate of
    ClassPred classTyCon [ty]
      | tyConName classTyCon == "Typeable" ->
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
    QuantifiedPred {} -> False
    IParamPred {} -> False
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
predTyVars predicate =
  case predicate of
    QuantifiedPred variables antecedents consequent ->
      filter (`notElem` variables) (nub (concatMap predTyVars antecedents <> predTyVars consequent))
    _ -> nub (concatMap typeTyVars (predArguments predicate))

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

planPredicateType :: TcDerivingPlan -> TcType
planPredicateType plan =
  TcTyCon (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan)

predDictBinder :: Pred -> TcM TcDictBinderAnnotation
predDictBinder predicate =
  case predicate of
    ClassPred classTyCon arguments ->
      pure (TcDictBinderAnnotation (tyConName classTyCon) arguments (TcTyCon classTyCon arguments))
    EqPred left right -> do
      equalityTyCon <- mkKnownTyCon "GHC.Types" "~" 2 (KFun KType (KFun KType KConstraint))
      pure (TcDictBinderAnnotation "<constraint>" [] (TcTyCon equalityTyCon [left, right]))
    quantified@QuantifiedPred {} ->
      TcDictBinderAnnotation "<quantified>" [] <$> predicateType quantified
    implicit@(IParamPred name payload) ->
      TcDictBinderAnnotation name [payload] <$> predicateType implicit

predicateType :: Pred -> TcM TcType
predicateType predicate =
  case predicate of
    ClassPred classTyCon arguments -> pure (TcTyCon classTyCon arguments)
    EqPred left right -> do
      equalityTyCon <- mkKnownTyCon "GHC.Types" "~" 2 (KFun KType (KFun KType KConstraint))
      pure (TcTyCon equalityTyCon [left, right])
    IParamPred name payload -> implicitParamType name payload
    QuantifiedPred variables antecedents consequent -> do
      consequentType <- predicateType consequent
      let qualified
            | null antecedents = consequentType
            | otherwise = TcQualTy antecedents consequentType
      pure (foldr TcForAllTy qualified variables)
