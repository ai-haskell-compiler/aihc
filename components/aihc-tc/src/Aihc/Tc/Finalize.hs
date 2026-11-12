{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize type-checker annotations after constraint solving.
module Aihc.Tc.Finalize
  ( finalizeModuleTc,
  )
where

import Aihc.Parser.Syntax (Annotation, Module, fromAnnotation, mkAnnotation)
import Aihc.Resolve.Traverse (traverseAnnotations)
import Aihc.Tc.Annotations
  ( CastDirection (..),
    PendingTcAnnotation (..),
    PendingTcCastAnnotation (..),
    PendingTcMatchArityCast (..),
    TcAnnotation (..),
    TcCastAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcCoercedInstance (..),
    TcCoercedMethod (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    TcMatchArityCast (..),
    TcPatSynAnnotation (..),
    renderTcType,
  )
import Aihc.Tc.Env (AssociatedTypeInfo (..), DataConFieldInfo (..), DataConInfo (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), FieldRep (..), TyConInfo (..), TypeFamilyInstanceInfo (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar)
import Aihc.Tc.Kind (defaultKindMetas)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (reclassifyIrreduciblePred, reduceTypeFamilies)
import Aihc.Tc.Tidy (tidyType)
import Aihc.Tc.Types
  ( GlobalName,
    Pred (..),
    TcKinds,
    TcType (..),
    TcTypeApplicationKinds (..),
    TyCon,
    TyVarId,
    TypeScheme (..),
    Unique (..),
    constraintKind,
    isEqualityTyCon,
    tvKind,
    tvUnique,
    tyConKey,
    typeApplicationKinds,
    typeKind,
    typeKindInEnv,
    typeMentionsMeta,
    typeMentionsTyVar,
    pattern KFun,
  )
import Aihc.Tc.Zonk (defaultPredKinds, defaultTyVarKinds, defaultTypeKinds, zonkPred, zonkType)
import Control.Applicative ((<|>))
import Control.DeepSeq (rnf)
import Control.Monad (zipWithM, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, mapMaybe)

-- | Convert every pending type-checker annotation in a module to a final
-- annotation. The walk covers every syntax constructor, so a pending
-- annotation cannot escape.
finalizeModuleTc :: Module -> TcM Module
finalizeModuleTc = traverseAnnotations finalizeAnnotationTc

finalizeAnnotationTc :: Annotation -> TcM Annotation
finalizeAnnotationTc ann =
  case fromAnnotation @PendingTcCastAnnotation ann of
    Just (PendingTcCastAnnotation ty ev direction) -> do
      evidence <- evidenceForEvVar ty ev >>= zonkEvTerm
      case evidence of
        EvCoercion (Refl _) -> do
          target <- zonkType ty >>= recordKindArguments Nothing
          pure $ case firstMetaType target of
            Nothing -> mkAnnotation (TcCastAnnotation Nothing target)
            Just {} -> mkAnnotation ()
        EvCoercion proof -> do
          let oriented = case direction of
                CastToRight -> proof
                CastToLeft -> symmetric proof
          target <- zonkType ty >>= recordKindArguments Nothing
          rejectMeta "cast annotation" (firstMetaCoercion oriented <|> firstMetaType target)
          pure (mkAnnotation (TcCastAnnotation (Just oriented) target))
        EvVarTerm _ -> pure (mkAnnotation ())
        _ -> abortTc "a result cast requires equality evidence"
    Nothing -> finalizeMatchArityCastTc ann

finalizeMatchArityCastTc :: Annotation -> TcM Annotation
finalizeMatchArityCastTc ann =
  case fromAnnotation @PendingTcMatchArityCast ann of
    Just (PendingTcMatchArityCast ty ev) -> do
      evidence <- evidenceForEvVar ty ev >>= zonkEvTerm
      arrow <- zonkType ty
      case evidence of
        EvCoercion (Refl _) -> do
          rejectMeta "match arity cast annotation" (firstMetaType arrow)
          pure (mkAnnotation (TcMatchArityCast Nothing arrow))
        EvCoercion proof -> do
          rejectMeta "match arity cast annotation" (firstMetaCoercion proof <|> firstMetaType arrow)
          pure (mkAnnotation (TcMatchArityCast (Just proof) arrow))
        EvVarTerm _ -> pure (mkAnnotation ())
        _ -> abortTc "a match arity cast requires equality evidence"
    Nothing -> finalizeOtherAnnotationTc ann

finalizeOtherAnnotationTc :: Annotation -> TcM Annotation
finalizeOtherAnnotationTc ann =
  case fromAnnotation @PendingTcAnnotation ann of
    Just pending -> mkAnnotation <$> annotationForPendingTc pending
    Nothing ->
      case fromAnnotation @TcInstanceMethodAnnotation ann of
        Just (TcInstanceMethodAnnotation name ty) -> do
          finalized <- finalizeType ty
          rejectMeta "instance method annotation" (firstMetaType finalized)
          pure (mkAnnotation (TcInstanceMethodAnnotation name finalized))
        Nothing ->
          case fromAnnotation @TcInstanceAnnotation ann of
            Just instanceAnnotation -> mkAnnotation <$> finalizeInstanceAnnotation instanceAnnotation
            Nothing -> finalizeOtherCheckedAnnotationTc ann

-- | Finalize the type and kind copies that an instance header retains.
finalizeInstanceAnnotation :: TcInstanceAnnotation -> TcM TcInstanceAnnotation
finalizeInstanceAnnotation annotation = do
  dictionaryType <- finalizeType (tcInstanceDictType annotation)
  variables <- mapM defaultTyVarKinds (tcInstanceTyVars annotation)
  headTypes <- mapM finalizeType (tcInstanceHeadTypes annotation)
  classVariables <- mapM defaultTyVarKinds (tcInstanceClassTyVars annotation)
  methods <- mapM finalizeMethod (tcInstanceClassMethods annotation)
  -- An instance variable that the head does not bind, as @v'@ in
  -- @instance (v ~~ v', Zero v') => Zero (Codensity v)@, is a meta
  -- variable in the evidence of a dictionary of that instance. The solver
  -- binds it after the evidence is built.
  superClasses <- mapM (traverse zonkEvTerm) (tcInstanceSuperClasses annotation)
  defaultMethodEvidence <- mapM (traverse (mapM zonkEvTerm)) (tcInstanceDefaultMethodEvidence annotation)
  let finalized =
        annotation
          { tcInstanceDictType = dictionaryType,
            tcInstanceTyVars = variables,
            tcInstanceHeadTypes = headTypes,
            tcInstanceClassTyVars = classVariables,
            tcInstanceClassMethods = methods,
            tcInstanceSuperClasses = superClasses,
            tcInstanceDefaultMethodEvidence = defaultMethodEvidence
          }
  rejectMetaFinalAnnotation (mkAnnotation finalized)
  pure finalized
  where
    finalizeMethod method = do
      ty <- finalizeType (tcClassMethodType method)
      variables <- mapM defaultTyVarKinds (tcClassMethodTyVars method)
      dictionaryType <- finalizeType (tcClassMethodDictType method)
      pure
        method
          { tcClassMethodType = ty,
            tcClassMethodTyVars = variables,
            tcClassMethodDictType = dictionaryType
          }

finalizeOtherCheckedAnnotationTc :: Annotation -> TcM Annotation
finalizeOtherCheckedAnnotationTc ann =
  case fromAnnotation @TcPatSynAnnotation ann of
    Just patSyn -> do
      -- The matcher, builder, and selector equations live inside the
      -- annotation. The walk does not enter an annotation payload.
      matcher <- traverseAnnotations finalizeAnnotationTc (tcPatSynMatcher patSyn)
      builder <- traverse (traverseAnnotations finalizeAnnotationTc) (tcPatSynBuilder patSyn)
      selectors <- traverse (traverse (traverseAnnotations finalizeAnnotationTc)) (tcPatSynSelectors patSyn)
      pure (mkAnnotation (TcPatSynAnnotation matcher builder selectors))
    Nothing -> do
      rejectMetaFinalAnnotation ann
      -- The checker writes the types of declarations as final annotations.
      case fromAnnotation @TcAnnotation ann of
        Just annotation -> mkAnnotation <$> recordAnnotationKindArguments annotation
        Nothing -> pure ann

annotationForPendingTc :: PendingTcAnnotation -> TcM TcAnnotation
annotationForPendingTc pending = do
  ty <- zonkType (pendingTcAnnType pending)
  typeBinders <- mapM zonkTypeBinder (pendingTcAnnTypeBinders pending)
  typeArgs <- mapM zonkType (pendingTcAnnTypeArgs pending)
  evidenceTerms <- mapM (evidenceForEvVar ty >=> zonkEvTerm) (pendingTcAnnEvidenceVars pending)
  evidenceBinders <- mapM (evidenceForEvVar ty >=> zonkEvTerm) (pendingTcAnnEvidenceBinders pending)
  termArgTypes <- mapM zonkType (pendingTcAnnTermArgTypes pending)
  let ann = TcAnnotation ty typeBinders typeArgs evidenceTerms evidenceBinders termArgTypes
  defaulted <- defaultUnsolvedAnnotationMetas ann
  rejectMetaTcAnnotation defaulted
  recordAnnotationKindArguments defaulted

-- | Record the kind arguments that no visible argument gives in the types
-- of an annotation. See 'recordKindArguments'.
recordAnnotationKindArguments :: TcAnnotation -> TcM TcAnnotation
recordAnnotationKindArguments annotation = do
  ty <- recordKindArguments Nothing (tcAnnType annotation)
  typeArgs <- mapM (recordKindArguments Nothing) (tcAnnTypeArgs annotation)
  termArgTypes <- mapM (recordKindArguments Nothing) (tcAnnTermArgTypes annotation)
  pure annotation {tcAnnType = ty, tcAnnTypeArgs = typeArgs, tcAnnTermArgTypes = termArgTypes}

defaultUnsolvedAnnotationMetas :: TcAnnotation -> TcM TcAnnotation
defaultUnsolvedAnnotationMetas annotation =
  case firstMetaTcAnnotation annotation of
    Nothing -> pure annotation
    Just meta -> do
      _ <- defaultKindMetas (TcMetaTv meta)
      solution <- readMetaTv meta
      case solution of
        Just {} -> pure ()
        Nothing -> do
          kinds <- getKinds
          rawKind <- defaultKindMetas =<< readMetaTvKind meta
          kind <- case rawKind of
            TcMetaTv kindMeta -> do
              writeMetaTv kindMeta (typeKind kinds)
              pure (typeKind kinds)
            _ -> pure rawKind
          writeMetaTv meta =<< undeterminedTypeOfKind kind
      annotation' <- zonkTcAnnotation annotation
      defaultUnsolvedAnnotationMetas annotation'

zonkTcAnnotation :: TcAnnotation -> TcM TcAnnotation
zonkTcAnnotation annotation =
  TcAnnotation
    <$> zonkType (tcAnnType annotation)
    <*> mapM zonkTypeBinder (tcAnnTypeBinders annotation)
    <*> mapM zonkType (tcAnnTypeArgs annotation)
    <*> mapM zonkEvTerm (tcAnnEvidenceTerms annotation)
    <*> mapM zonkEvTerm (tcAnnEvidenceBinders annotation)
    <*> mapM zonkType (tcAnnTermArgTypes annotation)

zonkTypeBinder :: TyVarId -> TcM TyVarId
zonkTypeBinder binder = do
  binderType <- zonkType (TcTyVar binder)
  case binderType of
    TcTyVar binder' -> pure binder'
    _ -> abortTc "internal type annotation error: type binder zonked to a non-variable"

-- | Reverse a proof, cancelling a symmetry rather than stacking one.
symmetric :: Coercion -> Coercion
symmetric proof =
  case proof of
    Sym inner -> inner
    _ -> Sym proof

evidenceForEvVar :: TcType -> EvVar -> TcM EvTerm
evidenceForEvVar contextType ev = do
  maybeEvidence <- lookupEvidence ev
  case maybeEvidence of
    Just evidence -> pure evidence
    Nothing -> do
      -- A reported type error can leave a wanted constraint without
      -- evidence. Keep the placeholder so the error reaches the user.
      kinds <- getKinds
      errorCount <- currentErrorCount
      if errorCount > 0
        then pure (EvVarTerm ev)
        else abortTc ("internal type annotation error: missing evidence for " <> show ev <> " while finalizing " <> renderTcType (tidyType kinds contextType))

zonkEvTerm :: EvTerm -> TcM EvTerm
zonkEvTerm evTerm =
  case evTerm of
    EvVarTerm ev -> do
      -- Follow a solved evidence variable to its term.
      maybeEvidence <- lookupEvidence ev
      case maybeEvidence of
        Just evidence -> zonkEvTerm evidence
        Nothing -> pure (EvVarTerm ev)
    EvGiven pred' ->
      EvGiven <$> finalizePred pred'
    EvDict origin name typeArgs evidence ->
      EvDict origin name <$> mapM finalizeType typeArgs <*> mapM zonkEvTerm evidence
    EvCoercible constructor left right ->
      EvCoercible constructor <$> finalizeType left <*> finalizeType right
    EvWithDict adapter arguments fieldType target targetArguments proof ->
      EvWithDict adapter <$> mapM finalizeType arguments <*> finalizeType fieldType <*> pure target <*> mapM finalizeType targetArguments <*> zonkCoercion proof
    EvEqualityDict constructor left right proof ->
      EvEqualityDict constructor <$> finalizeType left <*> finalizeType right <*> zonkCoercion proof
    EvCoercion coercion ->
      EvCoercion <$> zonkCoercion coercion
    EvSuperClass evidence sourceOrigin sourcePredicate fieldTypes index ->
      EvSuperClass <$> zonkEvTerm evidence <*> pure sourceOrigin <*> finalizePred sourcePredicate <*> mapM finalizeType fieldTypes <*> pure index
    EvCast evidence coercion ->
      EvCast <$> zonkEvTerm evidence <*> zonkCoercion coercion
    EvTypeable origin ty constructor kindArguments arguments ->
      EvTypeable origin <$> finalizeType ty <*> pure constructor <*> mapM (\(kind, evidence) -> (,) <$> finalizeType kind <*> zonkEvTerm evidence) kindArguments <*> mapM zonkEvTerm arguments
    EvTypeableLiteral origin ty helper evidence ->
      EvTypeableLiteral origin <$> finalizeType ty <*> pure helper <*> zonkEvTerm evidence
    EvTypeableApp origin function argument functionEvidence argumentEvidence ->
      EvTypeableApp origin <$> finalizeType function <*> finalizeType argument <*> zonkEvTerm functionEvidence <*> zonkEvTerm argumentEvidence
    EvTypeLit origin ty literal ->
      EvTypeLit origin <$> finalizeType ty <*> pure literal
    EvTypeLam variable body ->
      EvTypeLam <$> defaultTyVarKinds variable <*> zonkEvTerm body
    EvDictLam predicate binderType body ->
      EvDictLam <$> finalizePred predicate <*> finalizeType binderType <*> zonkEvTerm body
    EvTypeApp function argument ->
      EvTypeApp <$> zonkEvTerm function <*> finalizeType argument
    EvDictApp function argument ->
      EvDictApp <$> zonkEvTerm function <*> zonkEvTerm argument
    EvRecursive predicate dictionaryType body ->
      EvRecursive <$> finalizePred predicate <*> finalizeType dictionaryType <*> zonkEvTerm body
    EvCallStackPush origin function site parent ->
      EvCallStackPush origin function site <$> zonkEvTerm parent
    EvCallStackEmpty origin ->
      pure (EvCallStackEmpty origin)
    EvExceptionContextEmpty origin ->
      pure (EvExceptionContextEmpty origin)

finalizeType :: TcType -> TcM TcType
finalizeType = zonkType >=> defaultTypeKinds >=> recordKindArguments Nothing

-- | Record the kind arguments of a saturated type constructor application
-- when its visible arguments do not give them.
--
-- The kind of a type constructor can have a variable that only its result
-- kind mentions, as @k'@ in @Fst :: forall k k'. k -> k'@. 'mkAppTy'
-- drops the kind arguments of a saturated application, because the
-- visible arguments usually give them. For @Fst (T, T)@ they do not give
-- @k'@, and the desugarer cannot find it. Then this function writes the
-- application with its kind arguments, as @Fst \@Type \@Type (T, T)@.
--
-- The reduct of the application gives the kind when the application
-- reduces: @Fst (T, T)@ is @T@, of kind 'Type'. Otherwise the place of the
-- type gives the kind: a side of an arrow, a side of an equality, or a
-- constraint. If neither gives the kind, the application stays as it is.
--
-- The type must be zonked and its kinds defaulted. A type with no such
-- application is given back as it is.
recordKindArguments :: Maybe TcType -> TcType -> TcM TcType
recordKindArguments expected ty = do
  globals <- lift (gets tcsGlobalTyCons)
  if typeHasOpenResultKind globals ty
    then do
      kinds <- getKinds
      recordKindArgumentsIn kinds globals expected ty
    else pure ty

recordKindArgumentsIn :: TcKinds -> Map.Map GlobalName TyConInfo -> Maybe TcType -> TcType -> TcM TcType
recordKindArgumentsIn kinds globals = go
  where
    kindEnv = Map.map tciKindScheme globals
    go expected ty =
      case ty of
        TcTyCon tyCon arguments -> do
          -- The two sides of an equality have one kind, and each side can
          -- give it.
          let argumentKinds
                | isEqualityTyCon kinds tyCon,
                  [left, right] <- arguments =
                    [kindOf right, kindOf left]
                | otherwise = []
          arguments' <- zipWithM go (argumentKinds <> repeat Nothing) arguments
          let application = TcTyCon tyCon arguments'
          case openResultKindScheme globals tyCon (length arguments') of
            Just variables
              | not (determined tyCon arguments' variables) -> do
                  reduced <- reduceTypeFamilies application
                  -- Only a reduct with a different head gives the kind. If
                  -- only an argument reduces, the application stays stuck,
                  -- and its kind is the free result kind variable of the
                  -- scheme: @TypeError ('ShowType (F a))@ has the kind @b@.
                  let reducedKind =
                        case reduced of
                          TcTyCon reducedTyCon _ | reducedTyCon == tyCon -> Nothing
                          _ | reduced /= application -> kindOf reduced
                          _ -> Nothing
                  -- Calculate the result now. A lazy result keeps the kind
                  -- environment in memory until a consumer reads the type.
                  case mapMaybe (kindArguments tyCon arguments' variables) [reducedKind, expected] of
                    kindArguments' : _ -> rnf kindArguments' `seq` pure (foldl TcAppTy (TcKindedTyCon tyCon kindArguments') arguments')
                    [] -> pure application
            _ -> pure application
        -- A side of an arrow has a kind @TYPE r@. The type checker
        -- defaults an open representation to the lifted one.
        TcFunTy argument result -> TcFunTy <$> go (Just (typeKind kinds)) argument <*> go (Just (typeKind kinds)) result
        TcForAllTy tyVar body -> TcForAllTy tyVar <$> go expected body
        TcQualTy predicates body -> TcQualTy <$> mapM (recordPredKindArguments go anyKindOf kinds) predicates <*> go expected body
        TcAppTy function argument -> TcAppTy <$> go Nothing function <*> go Nothing argument
        _ -> pure ty
    kindOf argument =
      case anyKindOf argument of
        Just kind | not (typeMentionsMeta kind) -> Just kind
        _ -> Nothing
    anyKindOf argument = either (const Nothing) Just (typeKindInEnv kinds kindEnv argument)
    determined tyCon arguments variables =
      case typeApplicationKinds kinds kindEnv tyCon arguments Nothing of
        Right checked -> all (\variable -> Map.member (tvUnique variable) (tcInvisibleKindSubstitution checked)) variables
        Left _ -> False
    kindArguments tyCon arguments variables maybeKind = do
      kind <- maybeKind
      checked <- either (const Nothing) Just (typeApplicationKinds kinds kindEnv tyCon arguments (Just kind))
      mapM (\variable -> Map.lookup (tvUnique variable) (tcInvisibleKindSubstitution checked)) variables

recordPredKindArguments :: (Maybe TcType -> TcType -> TcM TcType) -> (TcType -> Maybe TcType) -> TcKinds -> Pred -> TcM Pred
recordPredKindArguments record kindOf kinds predicate =
  case predicate of
    EqPred left right ->
      EqPred <$> record (kindOf right) left <*> record (kindOf left) right
    IrredPred constraint -> IrredPred <$> record (Just (constraintKind kinds)) constraint
    _ -> pure predicate

-- | The kind variables of a type constructor, when its kind has a variable
-- that only the result kind mentions, for an application to this number
-- of visible arguments.
openResultKindScheme :: Map.Map GlobalName TyConInfo -> TyCon -> Int -> Maybe [TyVarId]
openResultKindScheme globals tyCon arity = do
  info <- Map.lookup (tyConKey tyCon) globals
  let ForAll variables _ resultKind = tciKindScheme info
      formals = take arity (formalKinds resultKind)
  if any (\variable -> not (any (typeMentionsTyVar variable) formals)) variables
    then Just variables
    else Nothing
  where
    formalKinds kind =
      case kind of
        KFun argument result -> argument : formalKinds result
        _ -> []

-- | Whether a type has an application that 'recordKindArguments' can
-- change. The walk allocates nothing.
typeHasOpenResultKind :: Map.Map GlobalName TyConInfo -> TcType -> Bool
typeHasOpenResultKind globals = go
  where
    go ty =
      case ty of
        TcTyCon tyCon arguments -> isJust (openResultKindScheme globals tyCon (length arguments)) || any go arguments
        TcFunTy argument result -> go argument || go result
        TcForAllTy _ body -> go body
        TcQualTy predicates body -> any (predHasOpenResultKind globals) predicates || go body
        TcAppTy function argument -> go function || go argument
        _ -> False

predHasOpenResultKind :: Map.Map GlobalName TyConInfo -> Pred -> Bool
predHasOpenResultKind globals predicate =
  case predicate of
    EqPred left right -> typeHasOpenResultKind globals left || typeHasOpenResultKind globals right
    IrredPred constraint -> typeHasOpenResultKind globals constraint
    _ -> False

finalizePred :: Pred -> TcM Pred
finalizePred = zonkPred >=> reclassifyIrreduciblePred >=> defaultPredKinds

zonkCoercion :: Coercion -> TcM Coercion
zonkCoercion coercion =
  case coercion of
    CoVar ev ->
      pure (CoVar ev)
    GivenCo predicate -> GivenCo <$> finalizePred predicate
    Refl ty ->
      Refl <$> (zonkType ty >>= recordKindArguments Nothing)
    Sym inner ->
      Sym <$> zonkCoercion inner
    Trans left right ->
      Trans <$> zonkCoercion left <*> zonkCoercion right
    NthCo index proof -> NthCo index <$> zonkCoercion proof
    LeftCo proof -> LeftCo <$> zonkCoercion proof
    RightCo proof -> RightCo <$> zonkCoercion proof
    EvidenceCo predicate evidence -> EvidenceCo <$> finalizePred predicate <*> zonkEvTerm evidence
    AppCo function argument -> AppCo <$> zonkCoercion function <*> zonkCoercion argument
    FunCo domain range -> FunCo <$> zonkCoercion domain <*> zonkCoercion range
    ForAllCo tyVar body -> ForAllCo tyVar <$> zonkCoercion body
    TyConAppCo tyCon arguments coercions kindCoercions ->
      TyConAppCo tyCon <$> mapM zonkType arguments <*> mapM zonkCoercion coercions <*> mapM zonkCoercion kindCoercions
    AxiomInstCo name typeArgs ->
      AxiomInstCo name <$> mapM zonkType typeArgs

rejectMetaTcAnnotation :: TcAnnotation -> TcM ()
rejectMetaTcAnnotation ann =
  case firstMetaTcAnnotation ann of
    Nothing -> pure ()
    Just {} -> do
      kinds <- getKinds
      abortTc
        ( "internal type annotation error: unzonked meta-variable in finalized annotation with type "
            <> renderTcType (tidyType kinds (tcAnnType ann))
        )

-- | An annotation carries one value, so the first cast that matches is the
-- only one that can. This runs over every annotation of every finalized
-- module, and trying all seven casts each time was most of its cost.
rejectMetaFinalAnnotation :: Annotation -> TcM ()
rejectMetaFinalAnnotation ann =
  case firstJusts
    [ reject "type annotation" firstMetaTcAnnotation (fromAnnotation @TcAnnotation ann),
      reject "class annotation" firstMetaClassAnnotation (fromAnnotation @TcClassAnnotation ann),
      reject "deriving annotation" firstMetaDerivingAnnotation (fromAnnotation @TcDerivingAnnotation ann),
      reject "instance annotation" firstMetaInstanceAnnotation (fromAnnotation @TcInstanceAnnotation ann),
      reject "instance method annotation" firstMetaInstanceMethodAnnotation (fromAnnotation @TcInstanceMethodAnnotation ann),
      reject "data-family instance annotation" firstMetaDataFamilyInstance (fromAnnotation @DataFamilyInstanceInfo ann),
      reject "type-family instance annotation" firstMetaTypeFamilyInstance (fromAnnotation @TypeFamilyInstanceInfo ann)
    ] of
    Nothing -> pure ()
    Just action -> action
  where
    reject context firstMeta = fmap (rejectMeta ("finalized " <> context) . firstMeta)

rejectMeta :: String -> Maybe Unique -> TcM ()
rejectMeta context maybeMeta =
  case maybeMeta of
    Nothing ->
      pure ()
    Just {} ->
      abortTc ("internal type annotation error: unzonked meta-variable in " <> context)

firstMetaTcAnnotation :: TcAnnotation -> Maybe Unique
firstMetaTcAnnotation ann =
  firstJusts
    ( firstMetaType (tcAnnType ann)
        : map firstMetaType (tcAnnTypeArgs ann)
        ++ map firstMetaEvTerm (tcAnnEvidenceTerms ann)
        ++ map firstMetaEvTerm (tcAnnEvidenceBinders ann)
        ++ map firstMetaType (tcAnnTermArgTypes ann)
    )

firstMetaClassAnnotation :: TcClassAnnotation -> Maybe Unique
firstMetaClassAnnotation classAnnotation =
  firstJusts (map (firstMetaType . TcTyVar) (tcClassKindTyVars classAnnotation))
    <|> firstJusts (map (firstMetaType . tcDictBinderType) (tcClassSuperClasses classAnnotation))
    <|> firstJusts (map firstMetaClassMethodAnnotation (tcClassMethods classAnnotation))
    <|> firstJusts (map (firstMetaType . snd) (tcClassDefaultSignatures classAnnotation))
    <|> firstJusts (map firstMetaTypeFamilyInstance (mapMaybe atiDefault (tcClassAssociatedTypes classAnnotation)))

firstMetaClassMethodAnnotation :: TcClassMethodAnnotation -> Maybe Unique
firstMetaClassMethodAnnotation method =
  firstMetaType (tcClassMethodType method) <|> firstMetaType (tcClassMethodDictType method)

firstMetaDerivingAnnotation :: TcDerivingAnnotation -> Maybe Unique
firstMetaDerivingAnnotation annotation =
  firstJusts (map firstMetaDerivingPlan (tcDerivingPlans annotation))

firstMetaDerivingPlan :: TcDerivingPlan -> Maybe Unique
firstMetaDerivingPlan plan =
  firstJusts
    ( map firstMetaType (tcDerivingHeadTypes plan)
        ++ maybe [] (map firstMetaDataConInfo . dtiConstructors) (tcDerivingDataType plan)
        ++ map firstMetaClassMethodAnnotation (tcDerivingClassMethods plan)
        ++ map firstMetaDictBinderAnnotation (tcDerivingClassSuperClasses plan)
        ++ concatMap (map firstMetaPred . snd) (tcDerivingDefaultSignatures plan)
        ++ [firstMetaDerivingStrategy (tcDerivingStrategy plan), firstMetaDerivingContext (tcDerivingContext plan)]
    )

firstMetaDerivingStrategy :: TcDerivingStrategy -> Maybe Unique
firstMetaDerivingStrategy strategy =
  case strategy of
    TcDerivingVia viaType -> firstMetaType viaType
    _ -> Nothing

firstMetaDerivingContext :: TcDerivingContext -> Maybe Unique
firstMetaDerivingContext context =
  case context of
    TcDerivingInferContext -> Nothing
    TcDerivingExplicitContext predicates -> firstJusts (map firstMetaPred predicates)

firstMetaInstanceAnnotation :: TcInstanceAnnotation -> Maybe Unique
firstMetaInstanceAnnotation ann =
  firstJusts
    ( firstMetaType (tcInstanceDictType ann)
        : map firstMetaType (tcInstanceHeadTypes ann)
        ++ map firstMetaDictBinderAnnotation (tcInstanceClassSuperClasses ann)
        ++ map (firstMetaType . tcClassMethodType) (tcInstanceClassMethods ann)
        ++ map firstMetaDictBinderAnnotation (tcInstanceContextDicts ann)
        ++ [ firstMetaDictBinderAnnotation superClass <|> firstMetaEvTerm evidence
           | (superClass, evidence) <- tcInstanceSuperClasses ann
           ]
        ++ concatMap (map firstMetaEvTerm . snd) (tcInstanceDefaultMethodEvidence ann)
        ++ concatMap (map firstMetaType . snd) (tcInstanceDefaultMethodTypes ann)
        ++ map firstMetaTypeFamilyInstance (tcInstanceAssociatedTypes ann)
        ++ [firstMetaCoerced body | Just body <- [tcInstanceCoerced ann]]
    )

firstMetaCoerced :: TcCoercedInstance -> Maybe Unique
firstMetaCoerced body =
  firstJusts
    ( map firstMetaType (tcCoercedHeadTypes body <> tcCoercedFieldTypes body)
        ++ [firstMetaEvTerm evidence | Just evidence <- [tcCoercedEvidence body]]
        ++ [firstMetaCoercion proof | Just proof <- [tcCoercedDictionaryCast body]]
        ++ [ firstMetaCoercion (tcCoercedMethodCoercion method)
               <|> firstJusts (map (firstMetaType . tvKind) (tcCoercedMethodTyVars method))
               <|> firstJusts (map firstMetaPred (tcCoercedMethodPredicates method))
               <|> firstJusts [firstMetaCoercion proof | Just proof <- tcCoercedMethodPredicateCasts method]
           | method <- tcCoercedMethods body
           ]
    )

firstMetaDictBinderAnnotation :: TcDictBinderAnnotation -> Maybe Unique
firstMetaDictBinderAnnotation ann =
  firstJusts (map firstMetaType (tcDictBinderArgs ann)) <|> firstMetaType (tcDictBinderType ann)

firstMetaInstanceMethodAnnotation :: TcInstanceMethodAnnotation -> Maybe Unique
firstMetaInstanceMethodAnnotation ann =
  firstMetaType (tcInstanceMethodType ann)

firstMetaDataFamilyInstance :: DataFamilyInstanceInfo -> Maybe Unique
firstMetaDataFamilyInstance info =
  firstMetaType (dfiiFamilyType info) <|> firstJusts (map firstMetaDataConInfo (dfiiConstructors info))

firstMetaTypeFamilyInstance :: TypeFamilyInstanceInfo -> Maybe Unique
firstMetaTypeFamilyInstance info =
  firstMetaType (tfiiLeft info) <|> firstMetaType (tfiiRight info)

firstMetaDataConInfo :: DataConInfo -> Maybe Unique
firstMetaDataConInfo info =
  firstJusts
    ( map firstMetaPred (dciTheta info)
        ++ map firstMetaField (dciFields info)
        ++ [firstMetaType (dciResTy info)]
    )

firstMetaField :: DataConFieldInfo -> Maybe Unique
firstMetaField field =
  firstMetaType (dcfiType field) <|> firstMetaFieldRep (dcfiRep field)

firstMetaFieldRep :: FieldRep -> Maybe Unique
firstMetaFieldRep rep =
  case rep of
    RepStored ty _ -> firstMetaType ty
    RepUnpack _ leaves -> firstJusts (map firstMetaFieldRep leaves)
    RepCast _ arguments inner ->
      firstJusts (map firstMetaType arguments) <|> firstMetaFieldRep inner

firstMetaEvTerm :: EvTerm -> Maybe Unique
firstMetaEvTerm evTerm =
  case evTerm of
    EvVarTerm {} ->
      Nothing
    EvGiven pred' ->
      firstMetaPred pred'
    EvDict _ _ typeArgs evidence ->
      firstJusts (map firstMetaType typeArgs ++ map firstMetaEvTerm evidence)
    EvCoercible _ left right ->
      firstMetaType left <|> firstMetaType right
    EvWithDict _ arguments fieldType _ targetArguments proof ->
      firstJusts (map firstMetaType (fieldType : arguments ++ targetArguments)) <|> firstMetaCoercion proof
    EvEqualityDict _ left right proof ->
      firstMetaType left <|> firstMetaType right <|> firstMetaCoercion proof
    EvCoercion coercion ->
      firstMetaCoercion coercion
    EvSuperClass evidence _ sourcePredicate fieldTypes _ ->
      firstMetaEvTerm evidence <|> firstMetaPred sourcePredicate <|> firstJusts (map firstMetaType fieldTypes)
    EvCast evidence coercion ->
      firstMetaEvTerm evidence <|> firstMetaCoercion coercion
    EvTypeable _ ty _ kindArguments arguments ->
      firstMetaType ty <|> firstJusts [firstMetaType kind <|> firstMetaEvTerm evidence | (kind, evidence) <- kindArguments] <|> firstJusts (map firstMetaEvTerm arguments)
    EvTypeableLiteral _ ty _ evidence ->
      firstMetaType ty <|> firstMetaEvTerm evidence
    EvTypeableApp _ function argument functionEvidence argumentEvidence ->
      firstMetaType function <|> firstMetaType argument <|> firstMetaEvTerm functionEvidence <|> firstMetaEvTerm argumentEvidence
    EvTypeLit _ ty _ ->
      firstMetaType ty
    EvTypeLam variable body ->
      firstMetaType (tvKind variable) <|> firstMetaEvTerm body
    EvDictLam predicate binderType body ->
      firstMetaPred predicate <|> firstMetaType binderType <|> firstMetaEvTerm body
    EvTypeApp function argument ->
      firstMetaEvTerm function <|> firstMetaType argument
    EvDictApp function argument ->
      firstMetaEvTerm function <|> firstMetaEvTerm argument
    EvRecursive predicate dictionaryType body ->
      firstMetaPred predicate <|> firstMetaType dictionaryType <|> firstMetaEvTerm body
    EvCallStackPush _ _ _ parent ->
      firstMetaEvTerm parent
    EvCallStackEmpty {} ->
      Nothing
    EvExceptionContextEmpty {} ->
      Nothing

firstMetaCoercion :: Coercion -> Maybe Unique
firstMetaCoercion coercion =
  case coercion of
    CoVar {} ->
      Nothing
    GivenCo predicate -> firstMetaPred predicate
    Refl ty ->
      firstMetaType ty
    Sym inner ->
      firstMetaCoercion inner
    Trans left right ->
      firstMetaCoercion left <|> firstMetaCoercion right
    NthCo _ proof -> firstMetaCoercion proof
    LeftCo proof -> firstMetaCoercion proof
    RightCo proof -> firstMetaCoercion proof
    EvidenceCo predicate evidence -> firstMetaPred predicate <|> firstMetaEvTerm evidence
    AppCo function argument -> firstMetaCoercion function <|> firstMetaCoercion argument
    FunCo domain range -> firstMetaCoercion domain <|> firstMetaCoercion range
    ForAllCo _ body -> firstMetaCoercion body
    TyConAppCo _ arguments coercions kindCoercions ->
      firstJusts (map firstMetaType arguments <> map firstMetaCoercion (coercions <> kindCoercions))
    AxiomInstCo _ typeArgs ->
      firstJusts (map firstMetaType typeArgs)

firstMetaPred :: Pred -> Maybe Unique
firstMetaPred pred' =
  case pred' of
    ClassPred _ args ->
      firstJusts (map firstMetaType args)
    EqPred left right ->
      firstMetaType left <|> firstMetaType right
    IParamPred _ payload ->
      firstMetaType payload
    IrredPred constraint ->
      firstMetaType constraint
    QuantifiedPred variables antecedents consequent ->
      firstJusts (map (firstMetaType . tvKind) variables)
        <|> firstJusts (map firstMetaPred antecedents)
        <|> firstMetaPred consequent

firstMetaType :: TcType -> Maybe Unique
firstMetaType ty =
  case ty of
    TcMetaTv meta ->
      Just meta
    TcArrowTy ->
      Nothing
    TcTyLit {} ->
      Nothing
    TcTyVar {} ->
      Nothing
    TcTyCon _ args ->
      firstJusts (map firstMetaType args)
    TcKindedTyCon _ kindArgs ->
      firstJusts (map firstMetaType kindArgs)
    TcFunTy left right ->
      firstMetaType left <|> firstMetaType right
    TcForAllTy _ body ->
      firstMetaType body
    TcQualTy preds body ->
      firstJusts (map firstMetaPred preds) <|> firstMetaType body
    TcAppTy fun arg ->
      firstMetaType fun <|> firstMetaType arg

firstJusts :: [Maybe a] -> Maybe a
firstJusts [] = Nothing
firstJusts (Nothing : rest) = firstJusts rest
firstJusts (Just value : _) = Just value
