{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Normalize deriving syntax into typed plans for later strategy-specific
-- checking and System FC lowering. This module checks the shared structure;
-- individual deriving mechanisms remain responsible for inferring attached
-- contexts and validating their class-specific rules.
module Aihc.Tc.Deriving
  ( annotateAttachedDerivingTc,
    annotateDataFamilyInstDerivingTc,
    annotateStandaloneDerivingTc,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BinderHead,
    Decl (..),
    DerivingClause (..),
    DerivingStrategy (..),
    Extension,
    Name (..),
    SourceSpan,
    StandaloneDerivingDecl (..),
    Type (..),
    UnqualifiedName,
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    instanceHeadName,
    mkAnnotation,
    nameText,
    tyVarBinderName,
    unqualifiedNameText,
  )
import Aihc.Resolve (binderResolution)
import Aihc.Tc.Annotations
  ( TcClassMethodAnnotation (..),
    TcDerivingAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcDictBinderAnnotation (..),
  )
import Aihc.Tc.Deriving.Strategy (checkDerivingStrategy, defaultStockFallback, isAutomaticTypeableClass)
import Aihc.Tc.Env (ClassInfo (..), DataConFieldInfo (..), DataConInfo (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), TyConFlavor (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (ParamInfo (..), TvKindEnv, checkSurfaceType, defaultKindMetas, freeTypeVars, freshKindMeta, instanceHeadArguments, makeParamEnv, surfaceContextToPreds, takeVisibleArgumentKinds, tcTypeKind, unifyKinds, zonkKind)
import Aihc.Tc.Monad
import Aihc.Tc.TypeScheme (schemeToType)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (defaultPredKinds, defaultTyVarKinds, defaultTypeKinds)
import Control.Applicative ((<|>))
import Control.Monad (filterM, zipWithM, zipWithM_, (<=<))
import Data.List (nub, (\\))
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T

annotateAttachedDerivingTc :: [Extension] -> TyConFlavor -> BinderHead UnqualifiedName -> [DerivingClause] -> Decl -> TcM Decl
annotateAttachedDerivingTc extensions targetFlavor targetHead clauses decl = do
  plans <- checkAttachedDerivingPlans extensions targetFlavor targetHead clauses
  pure (annotateDerivingPlans plans decl)

annotateDerivingPlans :: [TcDerivingPlan] -> Decl -> Decl
annotateDerivingPlans [] decl = decl
annotateDerivingPlans plans decl =
  DeclAnn (mkAnnotation (TcDerivingAnnotation plans)) decl

checkAttachedDerivingPlans :: [Extension] -> TyConFlavor -> BinderHead UnqualifiedName -> [DerivingClause] -> TcM [TcDerivingPlan]
checkAttachedDerivingPlans _ _ targetHead [] = do
  mapM_ (freshSkolemTv . tyVarBinderName) (binderHeadParams targetHead)
  pure []
checkAttachedDerivingPlans extensions targetFlavor targetHead clauses = do
  rawParams <- makeParamEnv (binderHeadParams targetHead)
  let targetName = unqualifiedNameText (binderHeadName targetHead)
  targetInfo <-
    case binderResolution (binderHeadName targetHead) of
      Just resolution -> lookupResolvedTypeSyntax resolution
      Nothing -> lookupTyCon targetName
  case targetInfo of
    Nothing -> missingTypeInfo ("deriving target " <> T.unpack targetName)
    Just info -> do
      -- The parameters take the kinds that the declaration inferred, so a
      -- higher-kinded parameter is not defaulted to 'Type' here.
      let argumentKinds = takeVisibleArgumentKinds maxBound (typeSchemeBody (tciKindScheme info))
      zipWithM_ (unifyKinds . paramKind) rawParams argumentKinds
      -- A declaration with a kind signature can have fewer binders than
      -- visible arguments, as @data FieldInfo :: Type -> Type where@ of
      -- generics-sop. The other arguments get a parameter of their own,
      -- so the target of a class of kind @Type -> Constraint@ is
      -- @FieldInfo p1@ and not the bare constructor.
      signatureParams <- zipWithM signatureParam [length rawParams + 1 ..] (drop (length rawParams) argumentKinds)
      params <- mapM defaultParam (rawParams <> signatureParams)
      dataType <- traverse derivingDataType =<< lookupDataType (tciTyCon info)
      concat <$> mapM (checkClause info dataType params) clauses
  where
    defaultParam param = do
      tyVar <- defaultTyVarKinds (paramTyVar param)
      pure param {paramTyVar = tyVar, paramKind = tvKind tyVar}

    signatureParam :: Int -> TcType -> TcM ParamInfo
    signatureParam index kind = do
      let name = T.pack ("p" <> show index)
      tyVar <- freshSkolemTvOfKind name kind
      pure ParamInfo {paramName = name, paramTyVar = tyVar, paramKind = kind}

    checkClause targetInfo dataType params clause = do
      classHeads <- attachedDerivingClassHeads clause
      catMaybes <$> mapM (checkOne targetInfo dataType params (derivingStrategy clause)) classHeads

    checkOne targetInfo dataType params strategy classHead = do
      let ForAll kindVariables _ _ = tciKindScheme targetInfo
      kindArguments <- mapM (const freshKindMeta) kindVariables
      let substitution = Map.fromList (zip (map tvUnique kindVariables) kindArguments)
          specialize param =
            let kind = applySubst substitution (paramKind param)
             in param {paramKind = kind, paramTyVar = setTyVarKind kind (paramTyVar param)}
          specializedParams = map specialize params
          tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- specializedParams]
      (plan, hadErrors) <-
        withErrorTracking (checkDerivingPlanFor extensions targetFlavor (\classSpan -> attachedTargetType classSpan targetInfo specializedParams) dataType specializedParams tvEnv strategy classHead)
      pure (if hadErrors then Nothing else plan)

-- | Plan the deriving clauses of a data family instance. The target is the
-- family applied to the instance arguments, as in GHC. The planner takes
-- the constructors of the instance through a datatype record whose
-- parameters stand for the instance arguments ('familyInstanceDataType').
annotateDataFamilyInstDerivingTc :: [Extension] -> DataFamilyInstanceInfo -> [DerivingClause] -> Decl -> TcM Decl
annotateDataFamilyInstDerivingTc extensions info clauses decl = do
  dataType <- familyInstanceDataType info
  let targetFlavor = if dfiiIsNewtype info then NewtypeTyCon else DataTyCon
      params = [ParamInfo {paramName = tvName tyVar, paramTyVar = tyVar, paramKind = tvKind tyVar} | tyVar <- dfiiTyVars info]
      tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params]
  plans <- concat <$> mapM (checkClause dataType targetFlavor params tvEnv) clauses
  pure (annotateDerivingPlans plans decl)
  where
    checkClause dataType targetFlavor params tvEnv clause = do
      classHeads <- attachedDerivingClassHeads clause
      catMaybes <$> mapM (checkOne dataType targetFlavor params tvEnv (derivingStrategy clause)) classHeads
    checkOne dataType targetFlavor params tvEnv strategy classHead = do
      (plan, hadErrors) <-
        withErrorTracking (checkDerivingPlanFor extensions targetFlavor (familyInstanceTargetType info) (Just dataType) params tvEnv strategy classHead)
      pure (if hadErrors then Nothing else plan)

-- | The target type of a family instance plan: the family applied to the
-- instance arguments. A class of a higher kind, as @Functor@, takes the
-- application without its last arguments. Each argument it leaves out must
-- be a type variable that no other argument mentions, as in GHC.
familyInstanceTargetType :: DataFamilyInstanceInfo -> Maybe SourceSpan -> TcType -> TcM TcType
familyInstanceTargetType info sourceSpan expectedKind =
  case dfiiFamilyType info of
    TcTyCon family arguments -> do
      let candidates =
            [ TcTyCon family kept
            | count <- [length arguments, length arguments - 1 .. 0],
              let (kept, dropped) = splitAt count arguments,
              all (droppable kept dropped) dropped
            ]
      matching <- filterM (fmap (kindAccepts expectedKind) . tcTypeKind) candidates
      case matching of
        target : _ -> do
          actualKind <- tcTypeKind target
          case expectedKind of
            KTYPE (TcTyVar _) -> pure ()
            _ -> unifyKinds expectedKind actualKind
          pure target
        [] -> do
          actualKind <- tcTypeKind (dfiiFamilyType info)
          emitError sourceSpan (KindMismatch expectedKind actualKind)
          pure (dfiiFamilyType info)
    other -> pure other
  where
    droppable kept dropped argument =
      case argument of
        TcTyVar tyVar ->
          not (any (typeMentionsTyVar tyVar) kept)
            && length (filter (== argument) dropped) == 1
        _ -> False

-- | A datatype record for the constructors of a data family instance. Each
-- instance argument that is a distinct type variable is a parameter. Each
-- other argument gets a fresh parameter that no field mentions. Thus the
-- substitution of the parameters by the instance arguments leaves the
-- fields as they are.
familyInstanceDataType :: DataFamilyInstanceInfo -> TcM DataTypeInfo
familyInstanceDataType info = do
  resultKind <- tcTypeKind (dfiiFamilyType info)
  case dfiiFamilyType info of
    TcTyCon family arguments -> do
      parameters <- zipWithM (parameterFor arguments) [0 :: Int ..] arguments
      let resultType = TcTyCon family (map TcTyVar parameters)
      pure
        DataTypeInfo
          { dtiName = dfiiFamilyName info,
            dtiTyCon = family,
            dtiTyVars = parameters,
            dtiResultKind = resultKind,
            dtiFlavor = if dfiiIsNewtype info then NewtypeTyCon else DataTyCon,
            dtiConstructors = [constructor {dciResTy = resultType} | constructor <- dfiiConstructors info],
            dtiNominalRoles = [],
            dtiCType = Nothing
          }
    _ -> missingTypeInfo ("data family instance head of " <> T.unpack (dfiiFamilyName info))
  where
    parameterFor arguments index argument =
      case argument of
        TcTyVar tyVar
          | length (filter (typeMentionsTyVar tyVar) arguments) == 1 -> pure tyVar
        _ -> do
          kind <- tcTypeKind argument
          freshSkolemTvOfKind (T.pack ("p" <> show index)) kind

data AttachedDerivingClassHead = AttachedDerivingClassHead
  { attachedClassName :: !Name,
    attachedClassArguments :: ![Type],
    attachedClassSpan :: !(Maybe SourceSpan)
  }

attachedDerivingClassHeads :: DerivingClause -> TcM [AttachedDerivingClassHead]
attachedDerivingClassHeads clause =
  case derivingClasses clause of
    Left className ->
      pure [AttachedDerivingClassHead className [] (nameSourceSpan className)]
    Right classTypes -> catMaybes <$> mapM classTypeHead classTypes
  where
    classTypeHead classType =
      case instanceHeadName classType of
        Just className ->
          pure
            ( Just
                AttachedDerivingClassHead
                  { attachedClassName = className,
                    attachedClassArguments = instanceHeadArguments classType,
                    attachedClassSpan = nameSourceSpan className <|> typeSpan classType
                  }
            )
        Nothing -> do
          emitError (typeSpan classType) (OtherError "invalid class in deriving clause")
          pure Nothing

-- | Check one class of a deriving clause. The function gives the target
-- type for the span of the class and the kind that the class expects.
checkDerivingPlanFor :: [Extension] -> TyConFlavor -> (Maybe SourceSpan -> TcType -> TcM TcType) -> Maybe DataTypeInfo -> [ParamInfo] -> TvKindEnv -> Maybe DerivingStrategy -> AttachedDerivingClassHead -> TcM (Maybe TcDerivingPlan)
checkDerivingPlanFor extensions targetFlavor targetTypeFor dataType params tvEnv strategy classHead = do
  let className = nameText (attachedClassName classHead)
      classSpan = attachedClassSpan classHead
      suppliedArguments = attachedClassArguments classHead
  maybeClassInfo <- lookupClassNamed (attachedClassName classHead)
  case maybeClassInfo of
    Nothing -> do
      emitError classSpan (OtherError ("deriving target " <> T.unpack className <> " is not a type class"))
      pure Nothing
    Just classInfo -> do
      automaticTypeable <- isAutomaticTypeableClass className (ciOrigin classInfo)
      if automaticTypeable
        then pure Nothing
        else case unsnoc (ciTyVars classInfo) of
          Nothing -> do
            emitError classSpan (OtherError ("deriving class " <> T.unpack className <> " has no target parameter"))
            pure Nothing
          Just (prefixClassVars, targetClassVar)
            | length suppliedArguments /= length prefixClassVars -> do
                emitError classSpan (derivingArityError className (length prefixClassVars) (length suppliedArguments))
                pure Nothing
            | not (null (unboundViaTyVars tvEnv strategy)) -> do
                mapM_ (emitError classSpan . OtherError . unboundViaTyVarError) (unboundViaTyVars tvEnv strategy)
                pure Nothing
            | otherwise -> do
                -- A variable that only the class arguments mention, as @s@
                -- in @deriving newtype (MonadState s)@, is a parameter of
                -- the derived instance, as in GHC.
                extraParams <- mapM argumentParam (nub (concatMap freeTypeVars suppliedArguments) \\ Map.keys tvEnv)
                let argumentEnv = tvEnv <> Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- extraParams]
                -- Each derived instance instantiates the kind variables of
                -- the class afresh: @deriving Generic1@ on @Lift f a@
                -- gives @Generic1 \@Type (Lift f)@ for
                -- @class Generic1 (f :: k -> Type)@.
                classKindSubst <- Map.fromList <$> mapM (\tyVar -> (,) (tvUnique tyVar) <$> freshKindMeta) (ciKindTyVars classInfo)
                let instantiateClassKind = applySubst classKindSubst
                checkedArguments <- mapM defaultTypeKinds =<< zipWithM (checkSurfaceType argumentEnv) suppliedArguments (map (instantiateClassKind . tvKind) prefixClassVars)
                argumentParams <- mapM defaultArgumentParam extraParams
                declaredTargetKind <- instantiateClassKind <$> defaultKindMetas (tvKind targetClassVar)
                targetType <- targetTypeFor classSpan declaredTargetKind
                targetKind <- defaultKindMetas =<< zonkKind declaredTargetKind
                -- A poly-kinded via type, as @Wrap (Modify s)@ for
                -- @newtype Wrap (a :: k -> Type) (es :: k)@, leaves kind
                -- metas that the target kind has fixed. Settle them, as a
                -- standalone declaration does.
                checkedStrategy <- defaultDerivingStrategyKinds =<< checkDerivingStrategy extensions targetFlavor className (ciOrigin classInfo) tvEnv targetKind classSpan strategy
                fallback <- defaultStockFallback className (ciOrigin classInfo) strategy checkedStrategy
                kinds <- getKinds
                methods <- derivingClassMethods classInfo
                let headTypes = checkedArguments <> [targetType]
                    strategyTypes = case checkedStrategy of TcDerivingVia viaType -> [viaType]; _ -> []
                    quantified = filter (\param -> any (typeMentionsTyVar (paramTyVar param)) (headTypes <> strategyTypes)) (params <> argumentParams)
                pure (Just ((mkDerivingPlan kinds classSpan checkedStrategy classInfo (map paramTyVar quantified) headTypes dataType TcDerivingInferContext methods) {tcDerivingStockFallback = fallback}))
  where
    argumentParam name = do
      rawTyVar <- freshSkolemTv name
      kind <- freshKindMeta
      let tyVar = setTyVarKind kind rawTyVar
      pure ParamInfo {paramName = name, paramTyVar = tyVar, paramKind = kind}
    defaultArgumentParam param = do
      tyVar <- defaultTyVarKinds (paramTyVar param)
      pure param {paramTyVar = tyVar, paramKind = tvKind tyVar}

-- | The variables a via type mentions that the datatype head does not bind.
-- A standalone declaration binds its own, but an attached clause has only
-- the parameters of the declaration it hangs off, and kind-checking the via
-- type against anything else leaves a meta-variable that no later pass can
-- solve.
unboundViaTyVars :: TvKindEnv -> Maybe DerivingStrategy -> [Text]
unboundViaTyVars tvEnv strategy =
  case strategy of
    Just (DerivingVia viaType) -> nub (freeTypeVars viaType) \\ Map.keys tvEnv
    _ -> []

unboundViaTyVarError :: Text -> String
unboundViaTyVarError name =
  "the via type mentions "
    <> T.unpack name
    <> ", which the datatype head does not bind"

attachedTargetType :: Maybe SourceSpan -> TyConInfo -> [ParamInfo] -> TcType -> TcM TcType
attachedTargetType sourceSpan targetInfo params expectedKind = do
  let tyCon = tciTyCon targetInfo
      arguments = map (TcTyVar . paramTyVar) params
      candidates =
        [ TcTyCon tyCon (take argumentCount arguments)
        | argumentCount <- [length arguments, length arguments - 1 .. 0]
        ]
  matching <- filterM (fmap (kindAccepts expectedKind) . tcTypeKind) candidates
  case matching of
    target : _ -> do
      actualKind <- tcTypeKind target
      case expectedKind of
        KTYPE (TcTyVar _) -> pure ()
        _ -> unifyKinds expectedKind actualKind
      pure target
    [] -> do
      emitError sourceSpan (KindMismatch expectedKind (typeSchemeBody (tciKindScheme targetInfo)))
      pure (TcTyCon tyCon arguments)

-- | Whether a target kind fits the kind that a class needs. A class whose
-- parameter has kind @TYPE r@ with a variable representation accepts a
-- type of any representation.
kindAccepts :: TcType -> TcType -> Bool
kindAccepts expected actual =
  expected == actual
    || case (expected, actual) of
      (_, TcMetaTv _) -> True
      (TcMetaTv _, _) -> True
      (TcFunTy left right, TcFunTy left' right') -> kindAccepts left left' && kindAccepts right right'
      (KTYPE (TcTyVar _), KTYPE _) -> True
      (KTYPE (TcMetaTv _), KTYPE _) -> True
      _ -> False

annotateStandaloneDerivingTc :: [Extension] -> StandaloneDerivingDecl -> TcM Decl
annotateStandaloneDerivingTc extensions derivingDecl = do
  (maybePlan, hadErrors) <- withErrorTracking (checkStandaloneDerivingPlan extensions derivingDecl)
  let plans = if hadErrors then [] else maybeToList maybePlan
  pure (annotateDerivingPlans plans (DeclStandaloneDeriving derivingDecl))

checkStandaloneDerivingPlan :: [Extension] -> StandaloneDerivingDecl -> TcM (Maybe TcDerivingPlan)
checkStandaloneDerivingPlan extensions derivingDecl =
  case instanceHeadName (standaloneDerivingHead derivingDecl) of
    Nothing -> do
      emitError (typeSpan (standaloneDerivingHead derivingDecl)) (OtherError "invalid standalone deriving instance head")
      pure Nothing
    Just classNameSyntax -> do
      let className = nameText classNameSyntax
          classSpan = nameSourceSpan classNameSyntax <|> typeSpan (standaloneDerivingHead derivingDecl)
          headArguments = instanceHeadArguments (standaloneDerivingHead derivingDecl)
          surfaceTypes = standaloneDerivingContext derivingDecl <> headArguments <> derivingStrategyTypes (standaloneDerivingStrategy derivingDecl)
          explicitNames = map tyVarBinderName (standaloneDerivingForall derivingDecl)
          implicitNames = nub (concatMap freeTypeVars surfaceTypes) \\ explicitNames
      explicitParams <- makeParamEnv (standaloneDerivingForall derivingDecl)
      implicitParams <- mapM implicitParam implicitNames
      let params = explicitParams <> implicitParams
          tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params]
      maybeClassInfo <- lookupClassNamed classNameSyntax
      automaticTypeable <- maybe (pure False) (isAutomaticTypeableClass className . ciOrigin) maybeClassInfo
      case maybeClassInfo of
        Nothing -> do
          emitError classSpan (OtherError ("deriving target " <> T.unpack className <> " is not a type class"))
          pure Nothing
        Just _
          | automaticTypeable -> pure Nothing
        Just classInfo
          | length headArguments /= length (ciTyVars classInfo) -> do
              emitError classSpan (standaloneDerivingArityError className (length (ciTyVars classInfo)) (length headArguments))
              pure Nothing
          | otherwise -> do
              kinds <- getKinds
              -- The instance instantiates the kind variables of the class
              -- afresh, as in 'checkAttachedDerivingPlan'.
              classKindSubst <- Map.fromList <$> mapM (\tyVar -> (,) (tvUnique tyVar) <$> freshKindMeta) (ciKindTyVars classInfo)
              let classParamKinds = map (applySubst classKindSubst . tvKind) (ciTyVars classInfo)
              checkedHead <- zipWithM (checkSurfaceType tvEnv) headArguments classParamKinds
              checkedContext <- surfaceContextToPreds tvEnv (standaloneDerivingContext derivingDecl)
              targetKind <- zonkKind (maybe (typeKind kinds) snd (unsnoc classParamKinds))
              targetFlavor <- standaloneTargetFlavor checkedHead
              checkedStrategy <- checkDerivingStrategy extensions targetFlavor className (ciOrigin classInfo) tvEnv targetKind classSpan (standaloneDerivingStrategy derivingDecl)
              tyVars <- mapM (defaultTyVarKinds . paramTyVar) params
              headTypes <- mapM defaultTypeKinds checkedHead
              context <- mapM defaultPredKinds checkedContext
              fallback <- defaultStockFallback className (ciOrigin classInfo) (standaloneDerivingStrategy derivingDecl) checkedStrategy
              strategy <- defaultDerivingStrategyKinds checkedStrategy
              methods <- derivingClassMethods classInfo
              dataType <- standaloneTargetDataType headTypes
              pure (Just ((mkDerivingPlan kinds classSpan strategy classInfo tyVars headTypes dataType (TcDerivingExplicitContext context) methods) {tcDerivingStockFallback = fallback, tcDerivingStandalone = True}))
  where
    implicitParam name = do
      rawTyVar <- freshSkolemTv name
      kind <- freshKindMeta
      let tyVar = setTyVarKind kind rawTyVar
      pure ParamInfo {paramName = name, paramTyVar = tyVar, paramKind = kind}

mkDerivingPlan :: TcKinds -> Maybe SourceSpan -> TcDerivingStrategy -> ClassInfo -> [TyVarId] -> [TcType] -> Maybe DataTypeInfo -> TcDerivingContext -> [TcClassMethodAnnotation] -> TcDerivingPlan
mkDerivingPlan kinds sourceSpan strategy classInfo tyVars headTypes dataType context methods =
  TcDerivingPlan
    { tcDerivingSourceSpan = sourceSpan,
      tcDerivingStrategy = strategy,
      tcDerivingStockFallback = False,
      tcDerivingStandalone = False,
      tcDerivingClassName = className,
      tcDerivingClassTyCon = ciTyCon classInfo,
      tcDerivingClassOrigin = ciOrigin classInfo,
      tcDerivingTyVars = tyVars,
      tcDerivingHeadTypes = headTypes,
      tcDerivingDataType = dataType,
      tcDerivingContext = context,
      tcDerivingClassTyVars = ciTyVars classInfo,
      tcDerivingClassSuperClasses = map (constraintTypeDictBinder kinds) (ciSuperClassTypes classInfo),
      tcDerivingClassMethods = methods,
      tcDerivingDefaultMethods = ciDefaultMethods classInfo,
      tcDerivingDefaultSignatures = [(methodName, predicates) | (methodName, ForAll _ predicates _) <- ciDefaultSignatures classInfo]
    }
  where
    className = ciName classInfo

derivingClassMethods :: ClassInfo -> TcM [TcClassMethodAnnotation]
derivingClassMethods classInfo =
  zipWithM method [0 :: Int ..] (ciMethods classInfo)
  where
    method index (methodName, scheme) = do
      let methodType = schemeToType scheme
          dictType = classDictionaryType classInfo
      pure
        TcClassMethodAnnotation
          { tcClassMethodName = methodName,
            tcClassMethodType = methodType,
            tcClassMethodTyVars = fst (peelForAlls methodType),
            tcClassMethodDictType = dictType,
            tcClassMethodIndex = index
          }

classDictionaryType :: ClassInfo -> TcType
classDictionaryType classInfo =
  TcTyCon (ciTyCon classInfo) (map TcTyVar (ciTyVars classInfo))

derivingStrategyTypes :: Maybe DerivingStrategy -> [Type]
derivingStrategyTypes (Just (DerivingVia viaType)) = [viaType]
derivingStrategyTypes _ = []

-- | The flavor of the type that a standalone deriving declaration targets.
-- The lookup goes by the identity of the checked type constructor, not by
-- its name: a name lookup finds whichever type of that name the environment
-- holds first, and a newtype such as @Lit@ then loses its flavor to an
-- unrelated @Lit@ from another package.
standaloneTargetFlavor :: [TcType] -> TcM TyConFlavor
standaloneTargetFlavor headTypes =
  case reverse headTypes of
    targetType : _
      | Just targetTyCon <- tcTypeConstructor targetType -> do
          maybeInfo <- lookupTyConByIdentity targetTyCon
          pure (maybe DataTyCon tciFlavor maybeInfo)
    _ -> pure DataTyCon

standaloneTargetDataType :: [TcType] -> TcM (Maybe DataTypeInfo)
standaloneTargetDataType headTypes =
  case reverse headTypes of
    targetType : _ -> maybe (pure Nothing) (traverse derivingDataType <=< lookupDataType) (tcTypeConstructor targetType)
    _ -> pure Nothing

-- | The datatype record that a deriving plan reads, with one parameter for
-- each visible argument and with each constructor in terms of these
-- parameters.
--
-- A declaration with a kind signature, as @data V :: Type -> Type where@,
-- binds fewer parameters than the type has visible arguments. Each other
-- argument gets a parameter here.
--
-- A constructor in GADT syntax has its own type variables, as @b@ in
-- @V :: b -> b -> V b@. The field analysis substitutes the instance head
-- for the datatype parameters, so a field type that names @b@ would not be
-- a use of the last parameter. When the result type of a constructor is the
-- type applied to distinct variables, these variables become the datatype
-- parameters. A constructor with another result type, as @IntV :: Int -> V
-- Int@, does not change.
derivingDataType :: DataTypeInfo -> TcM DataTypeInfo
derivingDataType dataType = do
  let signatureKinds = takeVisibleArgumentKinds maxBound (dtiResultKind dataType)
  signatureTyVars <- zipWithM signatureTyVar [length (dtiTyVars dataType) + 1 ..] signatureKinds
  let tyVars = dtiTyVars dataType <> signatureTyVars
      resultKind = dropVisibleArguments (length signatureKinds) (dtiResultKind dataType)
  pure
    dataType
      { dtiTyVars = tyVars,
        dtiResultKind = resultKind,
        dtiConstructors = map (renameConstructor tyVars) (dtiConstructors dataType)
      }
  where
    signatureTyVar :: Int -> TcType -> TcM TyVarId
    signatureTyVar index = freshSkolemTvOfKind (T.pack ("p" <> show index))
    dropVisibleArguments count kind =
      case kind of
        TcFunTy _ result | count > 0 -> dropVisibleArguments (count - 1) result
        _ -> kind
    renameConstructor tyVars constructor =
      case resultVariables (dciResTy constructor) of
        Just variables
          | length variables == length tyVars,
            length (nub variables) == length variables,
            all (`elem` (dciUnivTyVars constructor <> dciExTyVars constructor)) variables ->
              let substitution = Map.fromList (zip (map tvUnique variables) (map TcTyVar tyVars))
               in constructor
                    { dciUnivTyVars = tyVars,
                      dciExTyVars = filter (`notElem` variables) (dciExTyVars constructor),
                      dciTheta = map (applySubstPred substitution) (dciTheta constructor),
                      dciFields = [field {dcfiType = applySubst substitution (dcfiType field)} | field <- dciFields constructor],
                      dciResTy = applySubst substitution (dciResTy constructor)
                    }
        _ -> constructor
    resultVariables resultType =
      case collectTypeApplications resultType of
        (TcTyCon _ arguments, applied) -> traverse variable (arguments <> applied)
        (TcKindedTyCon _ _, applied) -> traverse variable applied
        _ -> Nothing
    variable ty =
      case ty of
        TcTyVar tyVar -> Just tyVar
        _ -> Nothing

tcTypeConstructor :: TcType -> Maybe TyCon
tcTypeConstructor ty =
  case ty of
    TcTyCon tyCon _ -> Just tyCon
    TcKindedTyCon tyCon _ -> Just tyCon
    TcAppTy function _ -> tcTypeConstructor function
    _ -> Nothing

defaultDerivingStrategyKinds :: TcDerivingStrategy -> TcM TcDerivingStrategy
defaultDerivingStrategyKinds strategy =
  case strategy of
    TcDerivingVia viaType -> TcDerivingVia <$> defaultTypeKinds viaType
    _ -> pure strategy

constraintTypeDictBinder :: TcKinds -> TcType -> TcDictBinderAnnotation
constraintTypeDictBinder kinds ty =
  case constraintTypeToPred kinds ty of
    Just predicate@(ClassPred classTyCon arguments) -> TcDictBinderAnnotation (tyConName classTyCon) arguments ty predicate
    Just predicate -> TcDictBinderAnnotation "<constraint>" [] ty predicate
    Nothing -> TcDictBinderAnnotation "<constraint>" [] ty (IrredPred ty)

peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tyVar body) =
  let (tyVars, inner) = peelForAlls body
   in (tyVar : tyVars, inner)
peelForAlls ty = ([], ty)

derivingArityError :: Text -> Int -> Int -> TcErrorKind
derivingArityError className expected supplied =
  OtherError
    ( "deriving class "
        <> T.unpack className
        <> " expects "
        <> show expected
        <> " argument(s) before the instance target, but got "
        <> show supplied
    )

standaloneDerivingArityError :: Text -> Int -> Int -> TcErrorKind
standaloneDerivingArityError className expected supplied =
  OtherError
    ( "standalone deriving class "
        <> T.unpack className
        <> " expects "
        <> show expected
        <> " instance argument(s), but got "
        <> show supplied
    )

nameSourceSpan :: Name -> Maybe SourceSpan
nameSourceSpan = sourceSpanFromAnns . nameAnns

sourceSpanFromAnns :: [Annotation] -> Maybe SourceSpan
sourceSpanFromAnns annotations =
  listToMaybe [sourceSpan | annotation <- annotations, Just sourceSpan <- [fromAnnotation @SourceSpan annotation]]

typeSpan :: Type -> Maybe SourceSpan
typeSpan ty =
  case ty of
    TAnn annotation inner -> fromAnnotation @SourceSpan annotation <|> typeSpan inner
    TParen inner -> typeSpan inner
    TForall _ inner -> typeSpan inner
    TContext _ inner -> typeSpan inner
    TKindSig inner _ -> typeSpan inner
    _ -> Nothing

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc values = Just (init values, last values)

missingTypeInfo :: String -> TcM a
missingTypeInfo message =
  abortTc ("internal type annotation error: missing " <> message)
