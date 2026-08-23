{-# LANGUAGE OverloadedStrings #-}

module Aihc.Tc.Kind
  ( TvKindEnv,
    ParamInfo (..),
    checkSurfaceType,
    checkRuntimeType,
    convertSurfaceType,
    convertSurfaceTypeWithKinds,
    defaultKindMetas,
    freeTypeVars,
    freshKindMeta,
    classPredicateArgKinds,
    makeParamEnv,
    makeParamEnvWith,
    sigToScheme,
    standaloneKindSigToScheme,
    surfacePredToPred,
    tyConKindFromParams,
    tyConKindFromParamsWith,
    tcTypeKind,
    unifyKinds,
    zonkKind,
  )
where

import Aihc.Parser.Syntax
  ( Name (..),
    SourceSpan (..),
    TupleFlavor (..),
    TyVarBinder (..),
    Type (..),
    TypeBuiltinCon (..),
    TypePromotion (..),
    UnqualifiedName (..),
    forallTelescopeBinders,
    instanceHeadName,
    instanceHeadTypes,
    nameText,
    peelTypeHead,
    tyVarBinderKind,
    tyVarBinderName,
    unqualifiedNameText,
  )
import Aihc.Tc.Env (TyConInfo (..), TypeSynonymInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Instantiate (instantiate)
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (foldM, zipWithM, zipWithM_)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

type TvKindEnv = Map Text (TyVarId, TcType)

data ParamInfo = ParamInfo
  { paramName :: !Text,
    paramTyVar :: !TyVarId,
    paramKind :: !TcType
  }
  deriving (Show)

sigToScheme :: Type -> TcM TypeScheme
sigToScheme ty = do
  let (explicitBinders, qualifiedBody) = splitForalls ty
      (context, body) = splitContext qualifiedBody
      freeVars = freeTypeVars ty
  rawTvs <- mapM freshSkolemTv freeVars
  kinds <- mapM (const freshKindMeta) freeVars
  let implicitTvs = zipWith setTyVarKind kinds rawTvs
  let implicitEnv = Map.fromList (zip freeVars (zip implicitTvs kinds))
  explicitParams <- makeParamEnvWith implicitEnv explicitBinders
  let explicitTvs = map paramTyVar explicitParams
      tvEnv =
        implicitEnv
          <> Map.fromList
            [ (paramName param, (paramTyVar param, paramKind param))
            | param <- explicitParams
            ]
  tcTy <- checkRuntimeType tvEnv body
  preds <- mapM (surfacePredToPred tvEnv) context
  pure (ForAll (implicitTvs <> explicitTvs) preds tcTy)

standaloneKindSigToScheme :: Type -> TcM TypeScheme
standaloneKindSigToScheme ty = do
  let (explicitBinders, bodyType) = splitForalls ty
      freeVars = freeTypeVars ty
  rawTyVars <- mapM freshSkolemTv freeVars
  implicitKinds <- mapM (const freshKindMeta) freeVars
  let implicitTyVars = zipWith setTyVarKind implicitKinds rawTyVars
      implicitEnv = Map.fromList [(tvName tyVar, (tyVar, tvKind tyVar)) | tyVar <- implicitTyVars]
  explicitParams <- makeParamEnvWith implicitEnv explicitBinders
  let explicitTyVars = map paramTyVar explicitParams
      tyVarEnv =
        implicitEnv
          <> Map.fromList
            [ (paramName param, (paramTyVar param, paramKind param))
            | param <- explicitParams
            ]
  body <- kindFromSurfaceType tyVarEnv bodyType
  pure (ForAll (implicitTyVars <> explicitTyVars) [] body)

convertSurfaceType :: Map Text TyVarId -> Type -> TcM TcType
convertSurfaceType tvMap ty = do
  let tvEnv = Map.map (\tv -> (tv, tvKind tv)) tvMap
  checkRuntimeType tvEnv ty

checkSurfaceType :: TvKindEnv -> Type -> TcType -> TcM TcType
checkSurfaceType tvEnv ty expected = do
  (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
  unifyKinds expected actual
  pure tcTy

-- | Check that a surface type is a value-bearing type of kind @TYPE rep@.
-- Unconstrained kind metas default to lifted representation; explicitly
-- unlifted types retain their fixed representation.
checkRuntimeType :: TvKindEnv -> Type -> TcM TcType
checkRuntimeType tvEnv ty = do
  (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
  actual' <- zonkKind actual
  case actual' of
    KTYPE {} -> pure tcTy
    KMeta unique -> bindKindMeta unique KType >> pure tcTy
    _ -> emitError NoSourceSpan (KindMismatch KType actual') >> pure tcTy

convertSurfaceTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertSurfaceTypeWithKinds tvEnv ty = do
  expanded <- expandTypeSynonym tvEnv (peelTypeHead ty)
  case expanded of
    Just result -> pure result
    Nothing -> convertNonSynonymTypeWithKinds tvEnv (peelTypeHead ty)

convertNonSynonymTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertNonSynonymTypeWithKinds tvEnv ty =
  case ty of
    TAnn _ inner ->
      convertSurfaceTypeWithKinds tvEnv inner
    TVar name ->
      inferTypeVariable tvEnv name
    TCon name promoted ->
      inferTypeConstructor promoted name
    TBuiltinCon builtin ->
      inferBuiltinTypeConstructor builtin
    TStar {} ->
      knownType "GHC.Types" "Type" KType
    TApp f a -> do
      (fTy, fKind) <- convertSurfaceTypeWithKinds tvEnv f
      (aTy, aKind) <- convertSurfaceTypeWithKinds tvEnv a
      resultKind <- freshKindMeta
      unifyKinds fKind (KFun aKind resultKind)
      resultKind' <- zonkKind resultKind
      pure (applyType fTy aTy, resultKind')
    TTypeApp f a -> do
      (fTy, fKind) <- convertSurfaceTypeWithKinds tvEnv f
      (aTy, aKind) <- convertSurfaceTypeWithKinds tvEnv a
      resultKind <- freshKindMeta
      unifyKinds fKind (KFun aKind resultKind)
      resultKind' <- zonkKind resultKind
      pure (applyType fTy aTy, resultKind')
    TInfix lhs name promoted rhs ->
      convertSurfaceTypeWithKinds tvEnv (TApp (TApp (TCon name promoted) lhs) rhs)
    TFun _ a b -> do
      aTy <- checkRuntimeType tvEnv a
      bTy <- checkRuntimeType tvEnv b
      pure (TcFunTy aTy bTy, KType)
    TTuple flavor _ args -> do
      tys <-
        case flavor of
          Boxed -> mapM (\arg -> checkSurfaceType tvEnv arg KType) args
          Unboxed -> mapM (checkRuntimeType tvEnv) args
      argumentKinds <- mapM tcTypeKind tys
      let argumentReps = map runtimeRepOrLifted argumentKinds
      let arity = length tys
          fallbackResultKind =
            case flavor of
              Boxed -> KType
              Unboxed -> KTYPE (TupleRep argumentReps)
          fallbackKind = foldr KFun fallbackResultKind argumentKinds
          typeName = tupleTyConText flavor arity
      maybeTyCon <- lookupTyCon typeName
      tyCon <-
        case maybeTyCon of
          Just info -> pure (tciTyCon info)
          Nothing -> mkKnownTyCon (tupleTyConModule flavor) typeName arity fallbackKind
      let tupleType = TcTyCon tyCon tys
      tupleKind <- tcTypeKind tupleType
      pure (tupleType, tupleKind)
    TUnboxedSum args -> do
      tys <- mapM (checkRuntimeType tvEnv) args
      argumentKinds <- mapM tcTypeKind tys
      let arity = length tys
          resultKind = KTYPE (SumRep (map runtimeRepOrLifted argumentKinds))
          tyConKind' = foldr KFun resultKind argumentKinds
          name = "(#" <> bars (arity - 1) <> "#)"
      tyCon <- mkKnownTyCon "GHC.Types" name arity tyConKind'
      pure (TcTyCon tyCon tys, resultKind)
    TList Unpromoted [arg] -> do
      argTy <- checkSurfaceType tvEnv arg KType
      listTy <- listType argTy
      pure (listTy, KType)
    TList Promoted args -> do
      elemKind <- freshKindMeta
      args' <- mapM (\arg -> checkSurfaceType tvEnv arg elemKind) args
      promotedListKind <- listType elemKind
      maybeNilInfo <- lookupTyCon "'[]"
      nilTyCon <- maybe (mkKnownTyCon "GHC.Types" "'[]" 0 promotedListKind) (pure . tciTyCon) maybeNilInfo
      maybeConsInfo <- lookupTyCon "':"
      let consKind = TcFunTy elemKind (TcFunTy promotedListKind promotedListKind)
      consTyCon <- maybe (mkKnownTyCon "GHC.Types" "':" 2 consKind) (pure . tciTyCon) maybeConsInfo
      let nil = TcTyCon nilTyCon []
          cons field rest = TcTyCon consTyCon [field, rest]
      pure (foldr cons nil args', promotedListKind)
    TKindSig inner kindTy -> do
      expected <- kindFromSurfaceType tvEnv kindTy
      checkSurfaceType tvEnv inner expected >>= \innerTy -> pure (innerTy, expected)
    TContext preds inner -> do
      mapM_ (\predTy -> checkSurfaceType tvEnv predTy KConstraint) preds
      convertSurfaceTypeWithKinds tvEnv inner
    TForall telescope inner -> do
      params <- makeParamEnv (forallTelescopeBinders telescope)
      let tvEnv' = tvEnv <> Map.fromList [(paramName p, (paramTyVar p, paramKind p)) | p <- params]
      (innerTy, innerKind) <- convertSurfaceTypeWithKinds tvEnv' inner
      pure (foldr (TcForAllTy . paramTyVar) innerTy params, innerKind)
    _ -> do
      emitError NoSourceSpan (OtherError ("unsupported surface type in kind checker: " <> take 80 (show ty)))
      meta <- freshMetaTv
      pure (meta, KType)

expandTypeSynonym :: TvKindEnv -> Type -> TcM (Maybe (TcType, TcType))
expandTypeSynonym tvEnv ty =
  case typeApplicationSpine ty of
    (TCon name Unpromoted, arguments) -> do
      maybeInfo <- lookupResolvedTyCon name
      case maybeInfo >>= tciTypeSynonym of
        Just synonym
          | Just {} <- tsiBody synonym -> Just <$> instantiateTypeSynonym tvEnv (nameText name) synonym arguments
        _ -> pure Nothing
    _ -> pure Nothing

instantiateTypeSynonym :: TvKindEnv -> Text -> TypeSynonymInfo -> [Type] -> TcM (TcType, TcType)
instantiateTypeSynonym tvEnv synonymName synonym arguments =
  case tsiBody synonym of
    Nothing -> do
      emitError NoSourceSpan (OtherError ("recursive or incomplete type synonym: " <> T.unpack synonymName))
      meta <- freshMetaTv
      pure (meta, KType)
    Just body -> do
      let params = tsiParams synonym
          arity = length params
          (synonymArguments, remainingArguments) = splitAt arity arguments
      if length synonymArguments /= arity
        then do
          emitError NoSourceSpan (OtherError ("type synonym " <> T.unpack synonymName <> " is not fully applied"))
          meta <- freshMetaTv
          pure (meta, KType)
        else do
          checkedArguments <- zipWithM checkArgument params synonymArguments
          let substitution = Map.fromList (zip (map tvUnique params) checkedArguments)
          expandedBody <- expandTcTypeSynonyms Set.empty (applySubst substitution body)
          expandedKind <- tcTypeKind expandedBody
          applyRemainingArguments (expandedBody, expandedKind) remainingArguments
  where
    checkArgument param argument = checkSurfaceType tvEnv argument (tvKind param)

    applyRemainingArguments result [] = pure result
    applyRemainingArguments (functionType, functionKind) (argument : rest) = do
      (argumentType, argumentKind) <- convertSurfaceTypeWithKinds tvEnv argument
      resultKind <- freshKindMeta
      unifyKinds functionKind (KFun argumentKind resultKind)
      zonkedResultKind <- zonkKind resultKind
      applyRemainingArguments (applyType functionType argumentType, zonkedResultKind) rest

typeApplicationSpine :: Type -> (Type, [Type])
typeApplicationSpine = go []
  where
    go arguments (TAnn _ inner) = go arguments inner
    go arguments (TApp function argument) = go (argument : arguments) function
    go arguments (TTypeApp function argument) = go (argument : arguments) function
    go arguments headType = (headType, arguments)

expandTcTypeSynonyms :: Set Text -> TcType -> TcM TcType
expandTcTypeSynonyms expanding ty =
  case ty of
    TcTyVar {} -> pure ty
    TcMetaTv {} -> pure ty
    TcTyCon tyCon arguments -> do
      expandedArguments <- mapM (expandTcTypeSynonyms expanding) arguments
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo >>= tciTypeSynonym of
        Just synonym
          | Just body <- tsiBody synonym,
            let params = tsiParams synonym,
            length expandedArguments >= length params ->
              if tyConName tyCon `Set.member` expanding
                then do
                  emitError NoSourceSpan (OtherError ("recursive type synonym: " <> T.unpack (tyConName tyCon)))
                  pure (TcTyCon tyCon expandedArguments)
                else do
                  let (synonymArguments, remainingArguments) = splitAt (length params) expandedArguments
                      substitution = Map.fromList (zip (map tvUnique params) synonymArguments)
                      expandedBody = applySubst substitution body
                  normalizedBody <- expandTcTypeSynonyms (Set.insert (tyConName tyCon) expanding) expandedBody
                  expandTcTypeSynonyms expanding (foldl applyType normalizedBody remainingArguments)
        _ -> pure (TcTyCon tyCon expandedArguments)
    TcFunTy argument result -> TcFunTy <$> expandTcTypeSynonyms expanding argument <*> expandTcTypeSynonyms expanding result
    TcForAllTy tyVar body -> TcForAllTy tyVar <$> expandTcTypeSynonyms expanding body
    TcQualTy predicates body -> TcQualTy <$> mapM expandPredicate predicates <*> expandTcTypeSynonyms expanding body
    TcAppTy function argument -> applyType <$> expandTcTypeSynonyms expanding function <*> expandTcTypeSynonyms expanding argument
  where
    expandPredicate predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM (expandTcTypeSynonyms expanding) arguments
        EqPred left right -> EqPred <$> expandTcTypeSynonyms expanding left <*> expandTcTypeSynonyms expanding right

inferTypeVariable :: TvKindEnv -> UnqualifiedName -> TcM (TcType, TcType)
inferTypeVariable tvEnv name =
  let n = unqualifiedNameText name
   in case Map.lookup n tvEnv of
        Just (tv, kind) -> pure (TcTyVar tv, kind)
        Nothing -> inferUnknownType

inferTypeConstructor :: TypePromotion -> Name -> TcM (TcType, TcType)
inferTypeConstructor promoted name =
  case promoted of
    Promoted -> do
      maybeInfo <- lookupResolvedPromotedTyCon name
      case maybeInfo of
        Just info -> do
          kind <- instantiateTyConKind info
          pure (TcTyCon (tciTyCon info) [], kind)
        Nothing -> inferPromotedTypeConstructor (nameText name)
    Unpromoted ->
      case nameText name of
        "Type" -> knownType "GHC.Types" "Type" KType
        "Constraint" -> knownType "GHC.Types" "Constraint" KType
        _ -> do
          mInfo <- lookupResolvedTyCon name
          case mInfo of
            Just info -> do
              kind <- instantiateTyConKind info
              pure (TcTyCon (tciTyCon info) [], kind)
            Nothing -> inferUnknownType

instantiateTyConKind :: TyConInfo -> TcM TcType
instantiateTyConKind info = do
  (kindType, _) <- instantiate (tciKindScheme info)
  pure kindType

inferBuiltinTypeConstructor :: TypeBuiltinCon -> TcM (TcType, TcType)
inferBuiltinTypeConstructor builtin =
  case builtin of
    TBuiltinList ->
      do
        maybeInfo <- lookupTyCon "[]"
        tyCon <- maybe (mkKnownTyCon "GHC.Types" "[]" 1 (KFun KType KType)) (pure . tciTyCon) maybeInfo
        pure (TcTyCon tyCon [], KFun KType KType)
    TBuiltinCons -> do
      let kind = KFun KType (KFun (listTypeKind KType) (listTypeKind KType))
      tyCon <- mkKnownTyCon "GHC.Types" "':" 2 kind
      pure (TcTyCon tyCon [], kind)
    TBuiltinTuple arity ->
      let argKinds = replicate arity KType
          kind = foldr KFun KType argKinds
       in knownTypeWithArity "GHC.Tuple" (tupleTyConText Boxed arity) arity kind
    TBuiltinArrow -> do
      let kind = KFun KType (KFun KType KType)
      tyCon <- mkKnownTyCon "GHC.Types" "(->)" 2 kind
      pure (TcTyCon tyCon [], kind)

knownType :: Text -> Text -> TcType -> TcM (TcType, TcType)
knownType moduleName name = knownTypeWithArity moduleName name 0

knownTypeWithArity :: Text -> Text -> Int -> TcType -> TcM (TcType, TcType)
knownTypeWithArity moduleName name arity kind = do
  maybeInfo <- lookupTyCon name
  tyCon <- maybe (mkKnownTyCon moduleName name arity kind) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [], kind)

inferPromotedTypeConstructor :: Text -> TcM (TcType, TcType)
inferPromotedTypeConstructor name
  | name == "[]" = do
      elementKind <- freshKindMeta
      resultKind <- listType elementKind
      tyCon <- mkKnownTyCon "GHC.Types" "'[]" 0 resultKind
      pure (TcTyCon tyCon [], resultKind)
  | name == ":" = do
      elementKind <- freshKindMeta
      resultKind <- listType elementKind
      let kind = TcFunTy elementKind (TcFunTy resultKind resultKind)
      tyCon <- mkKnownTyCon "GHC.Types" "':" 2 kind
      pure (TcTyCon tyCon [], kind)
  | otherwise = inferUnknownType

inferUnknownType :: TcM (TcType, TcType)
inferUnknownType = do
  kind <- freshKindMeta
  ty <- freshMetaTvOfKind kind
  pure (ty, kind)

makeParamEnv :: [TyVarBinder] -> TcM [ParamInfo]
makeParamEnv = makeParamEnvWith Map.empty

makeParamEnvWith :: TvKindEnv -> [TyVarBinder] -> TcM [ParamInfo]
makeParamEnvWith = go
  where
    go _ [] = pure []
    go tvEnv (binder : rest) = do
      rawTv <- freshSkolemTv (tyVarBinderName binder)
      kind <- maybe freshKindMeta (kindFromSurfaceType tvEnv) (tyVarBinderKind binder)
      let tv = setTyVarKind kind rawTv
          param =
            ParamInfo
              { paramName = tyVarBinderName binder,
                paramTyVar = tv,
                paramKind = kind
              }
          tvEnv' = Map.insert (paramName param) (tv, kind) tvEnv
      (param :) <$> go tvEnv' rest

tyConKindFromParams :: [ParamInfo] -> Maybe Type -> TcM TcType
tyConKindFromParams = tyConKindFromParamsWith Map.empty

tyConKindFromParamsWith :: TvKindEnv -> [ParamInfo] -> Maybe Type -> TcM TcType
tyConKindFromParamsWith outerEnv params maybeResultKind = do
  let tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params] <> outerEnv
  resultKind <- maybe (pure KType) (kindFromSurfaceType tvEnv) maybeResultKind
  pure (foldr (KFun . paramKind) resultKind params)

kindFromSurfaceType :: TvKindEnv -> Type -> TcM TcType
kindFromSurfaceType tvEnv ty =
  case peelTypeHead ty of
    TStar {} -> pure KType
    other -> do
      (tcType, kind) <- convertSurfaceTypeWithKinds tvEnv other
      unifyKinds kind KType
      pure tcType

unifyKinds :: TcType -> TcType -> TcM ()
unifyKinds expected actual = do
  expected' <- zonkKind expected
  actual' <- zonkKind actual
  case (expected', actual') of
    (TcMetaTv unique, kind) -> bindKindMeta unique kind
    (kind, TcMetaTv unique) -> bindKindMeta unique kind
    (TcTyVar left, TcTyVar right)
      | left == right -> pure ()
    (TcTyCon left leftArguments, TcTyCon right rightArguments)
      | left == right,
        length leftArguments == length rightArguments ->
          zipWithM_ unifyKinds leftArguments rightArguments
    (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
      unifyKinds leftArgument rightArgument >> unifyKinds leftResult rightResult
    (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) ->
      unifyKinds leftFunction rightFunction >> unifyKinds leftArgument rightArgument
    (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody)
      | leftVar == rightVar -> unifyKinds leftBody rightBody
    (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody)
      | leftPredicates == rightPredicates -> unifyKinds leftBody rightBody
    _ -> emitError NoSourceSpan (KindMismatch expected' actual')

bindKindMeta :: Unique -> TcType -> TcM ()
bindKindMeta u kind
  | kind == TcMetaTv u = pure ()
  | occursInKind u kind = emitError NoSourceSpan (KindMismatch (KMeta u) kind)
  | otherwise = writeMetaTv u kind

zonkKind :: TcType -> TcM TcType
zonkKind kind =
  case kind of
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      case solution of
        Nothing -> pure kind
        Just solved -> zonkKind solved
    TcTyVar tyVar -> do
      kind' <- zonkKind (tvKind tyVar)
      pure (TcTyVar (setTyVarKind kind' tyVar))
    TcTyCon tyCon arguments -> do
      tyCon' <- configuredTyCon tyCon
      let original = TcTyCon tyCon' arguments
      expanded <- expandTcTypeSynonyms Set.empty original
      if expanded == original
        then TcTyCon tyCon' <$> mapM zonkKind arguments
        else zonkKind expanded
    TcFunTy argument result -> TcFunTy <$> zonkKind argument <*> zonkKind result
    TcForAllTy tyVar body -> do
      kind' <- zonkKind (tvKind tyVar)
      TcForAllTy (setTyVarKind kind' tyVar) <$> zonkKind body
    TcQualTy predicates body -> TcQualTy <$> mapM zonkKindPred predicates <*> zonkKind body
    TcAppTy function argument -> TcAppTy <$> zonkKind function <*> zonkKind argument
  where
    zonkKindPred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM zonkKind arguments
        EqPred left right -> EqPred <$> zonkKind left <*> zonkKind right

defaultKindMetas :: TcType -> TcM TcType
defaultKindMetas kind =
  case kind of
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      case solution of
        Just solved -> do
          solved' <- zonkKind solved
          tracked <- isTrackedKindMeta unique
          incomplete <- containsUnsolvedMeta solved'
          if tracked && incomplete
            then unifyKinds solved' KType >> pure KType
            else defaultKindMetas solved'
        Nothing -> do
          tracked <- isTrackedKindMeta unique
          if tracked
            then writeMetaTv unique KType >> pure KType
            else pure kind
    TcTyVar tyVar -> do
      kind' <- defaultKindMetas (tvKind tyVar)
      pure (TcTyVar (setTyVarKind kind' tyVar))
    TcTyCon tyCon arguments -> TcTyCon tyCon <$> mapM defaultKindMetas arguments
    TcFunTy argument result -> TcFunTy <$> defaultKindMetas argument <*> defaultKindMetas result
    TcForAllTy tyVar body -> do
      kind' <- defaultKindMetas (tvKind tyVar)
      TcForAllTy (setTyVarKind kind' tyVar) <$> defaultKindMetas body
    TcQualTy predicates body -> TcQualTy <$> mapM defaultKindPred predicates <*> defaultKindMetas body
    TcAppTy function argument -> TcAppTy <$> defaultKindMetas function <*> defaultKindMetas argument
  where
    defaultKindPred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM defaultKindMetas arguments
        EqPred left right -> EqPred <$> defaultKindMetas left <*> defaultKindMetas right

containsUnsolvedMeta :: TcType -> TcM Bool
containsUnsolvedMeta ty =
  case ty of
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      maybe (pure True) containsUnsolvedMeta solution
    TcTyVar tyVar -> containsUnsolvedMeta (tvKind tyVar)
    TcTyCon _ arguments -> or <$> mapM containsUnsolvedMeta arguments
    TcFunTy argument result -> (||) <$> containsUnsolvedMeta argument <*> containsUnsolvedMeta result
    TcForAllTy tyVar body -> (||) <$> containsUnsolvedMeta (tvKind tyVar) <*> containsUnsolvedMeta body
    TcQualTy predicates body -> do
      predicateResults <- mapM containsUnsolvedPred predicates
      bodyResult <- containsUnsolvedMeta body
      pure (or predicateResults || bodyResult)
    TcAppTy function argument -> (||) <$> containsUnsolvedMeta function <*> containsUnsolvedMeta argument
  where
    containsUnsolvedPred predicate =
      case predicate of
        ClassPred _ arguments -> or <$> mapM containsUnsolvedMeta arguments
        EqPred left right -> (||) <$> containsUnsolvedMeta left <*> containsUnsolvedMeta right

freshKindMeta :: TcM TcType
freshKindMeta = do
  unique <- freshUnique
  trackKindMeta unique
  pure (TcMetaTv unique)

occursInKind :: Unique -> TcType -> Bool
occursInKind needle kind =
  case kind of
    TcMetaTv unique -> unique == needle
    TcTyVar tyVar -> occursInKind needle (tvKind tyVar)
    TcTyCon _ arguments -> any (occursInKind needle) arguments
    TcFunTy argument result -> occursInKind needle argument || occursInKind needle result
    TcForAllTy tyVar body -> occursInKind needle (tvKind tyVar) || occursInKind needle body
    TcQualTy predicates body -> any occursInPred predicates || occursInKind needle body
    TcAppTy function argument -> occursInKind needle function || occursInKind needle argument
  where
    occursInPred predicate =
      case predicate of
        ClassPred _ arguments -> any (occursInKind needle) arguments
        EqPred left right -> occursInKind needle left || occursInKind needle right

tcTypeKind :: TcType -> TcM TcType
tcTypeKind ty =
  case ty of
    TcTyVar tyVar -> zonkKind (tvKind tyVar)
    TcMetaTv unique -> readMetaTvKind unique >>= zonkKind
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      initialKind <-
        case maybeInfo of
          Just info -> instantiateTyConKind info
          Nothing -> do
            emitError NoSourceSpan (OtherError ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
            pure (foldr KFun KType (replicate (tyConArity tyCon) KType))
      foldM applyArgument initialKind arguments
    TcFunTy {} -> pure KType
    TcForAllTy _ body -> tcTypeKind body
    TcQualTy _ _ -> pure KType
    TcAppTy function argument -> tcTypeKind function >>= (`applyArgument` argument)
  where
    applyArgument functionKind argument = do
      functionKind' <- zonkKind functionKind
      case functionKind' of
        TcFunTy argumentKind resultKind -> do
          actualKind <- tcTypeKind argument
          unifyKinds argumentKind actualKind
          zonkKind resultKind
        TcMetaTv {} -> do
          argumentKind <- tcTypeKind argument
          resultKind <- freshKindMeta
          unifyKinds functionKind' (TcFunTy argumentKind resultKind)
          zonkKind resultKind
        _ -> do
          emitError NoSourceSpan (KindMismatch (TcFunTy KType KType) functionKind')
          pure KType

applyType :: TcType -> TcType -> TcType
applyType (TcTyCon tc args) arg = TcTyCon tc (args ++ [arg])
applyType f arg = TcAppTy f arg

listType :: TcType -> TcM TcType
listType ty = do
  maybeInfo <- lookupTyCon "[]"
  tyCon <- maybe (mkKnownTyCon "GHC.Types" "[]" 1 (KFun KType KType)) (pure . tciTyCon) maybeInfo
  pure (TcTyCon tyCon [ty])

listTypeKind :: TcType -> TcType
listTypeKind kind = KFun kind kind

runtimeRepOrLifted :: TcType -> TcType
runtimeRepOrLifted kind =
  case runtimeRepFromKind kind of
    Right runtimeRep -> runtimeRep
    Left _ -> liftedRep

freeTypeVars :: Type -> [Text]
freeTypeVars = nub . go
  where
    go (TVar name) = [unqualifiedNameText name]
    go (TApp f a) = go f ++ go a
    go (TTypeApp f a) = go f ++ go a
    go (TInfix lhs _ _ rhs) = go lhs ++ go rhs
    go (TFun _ a b) = go a ++ go b
    go (TTuple _ _ args) = concatMap go args
    go (TUnboxedSum args) = concatMap go args
    go (TList _ args) = concatMap go args
    go (TParen inner) = go inner
    go (TAnn _ inner) = go inner
    go (TKindSig inner kindTy) = go inner ++ go kindTy
    go (TContext preds inner) = concatMap go preds ++ go inner
    go (TForall telescope inner) =
      filter
        (`Set.notMember` boundNames)
        (concatMap binderKindVars binders ++ go inner)
      where
        binders = forallTelescopeBinders telescope
        boundNames = Set.fromList (map tyVarBinderName binders)
    go _ = []
    binderKindVars binder = maybe [] go (tyVarBinderKind binder)

splitContext :: Type -> ([Type], Type)
splitContext (TAnn _ inner) = splitContext inner
splitContext (TContext preds inner) = (preds, inner)
splitContext ty = ([], ty)

splitForalls :: Type -> ([TyVarBinder], Type)
splitForalls ty =
  case ty of
    TAnn _ inner -> splitForalls inner
    TParen inner -> splitForalls inner
    TForall telescope inner ->
      let (binders, body) = splitForalls inner
       in (forallTelescopeBinders telescope <> binders, body)
    _ -> ([], ty)

surfacePredToPred :: TvKindEnv -> Type -> TcM Pred
surfacePredToPred tvEnv ty =
  case instanceHeadName ty of
    Just className -> do
      let classNameText = nameText className
          headArgs = instanceHeadTypes ty
      maybeClassInfo <- lookupTyCon classNameText
      case maybeClassInfo of
        Just classInfo -> do
          argKinds <- takeClassArgKinds (length headArgs) <$> defaultKindMetas (typeSchemeBody (tciKindScheme classInfo))
          args <- zipWithM (checkSurfaceType tvEnv) headArgs argKinds
          pure (ClassPred (tciTyCon classInfo) args)
        Nothing -> do
          emitError NoSourceSpan (OtherError ("unknown class predicate: " <> T.unpack classNameText))
          abortTc ("missing checked type constructor for class predicate " <> T.unpack classNameText)
    Nothing -> do
      emitError NoSourceSpan (OtherError ("invalid class predicate: " <> show ty))
      abortTc "invalid checked class predicate"

classPredicateArgKinds :: Text -> Int -> TcM [TcType]
classPredicateArgKinds className argCount = do
  mInfo <- lookupTyCon className
  case mInfo of
    Just info -> takeClassArgKinds argCount <$> defaultKindMetas (typeSchemeBody (tciKindScheme info))
    Nothing -> mapM (const freshKindMeta) [1 .. argCount]

takeClassArgKinds :: Int -> TcType -> [TcType]
takeClassArgKinds n kind
  | n <= 0 = []
  | otherwise =
      case kind of
        KFun arg rest -> arg : takeClassArgKinds (n - 1) rest
        _ -> replicate n KType

tupleTyConText :: TupleFlavor -> Int -> Text
tupleTyConText flavor arity =
  case flavor of
    Boxed -> boxedTupleTyConName arity
    Unboxed -> unboxedTupleTyConName arity

tupleTyConModule :: TupleFlavor -> Text
tupleTyConModule flavor =
  case flavor of
    Boxed -> "GHC.Tuple"
    Unboxed -> "GHC.Types"

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = mconcat (replicate n "|")
