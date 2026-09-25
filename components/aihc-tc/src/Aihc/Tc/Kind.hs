{-# LANGUAGE OverloadedStrings #-}

module Aihc.Tc.Kind
  ( TvKindEnv,
    ParamInfo (..),
    checkSurfaceType,
    checkRuntimeType,
    unboxedSumType,
    convertSurfaceTypeWithKinds,
    defaultKindMetas,
    deferKindMetas,
    freeTypeVars,
    freshKindMeta,
    bindKindMeta,
    classPredicateArgKinds,
    makeParamEnv,
    makeParamEnvWith,
    sigToScheme,
    patSynSigToScheme,
    hasWildcardType,
    flattenSurfaceContext,
    isUnitConstraintType,
    splitSigma,
    explicitForallNames,
    scopedSigTyVars,
    standaloneKindSigToScheme,
    surfaceContextToPreds,
    takeVisibleArgumentKinds,
    tyConKindFromParams,
    tyConKindFromParamsWith,
    runtimeRepOrLifted,
    tcTypeKind,
    kindedTyConAt,
    kindedTyConUse,
    refineGivenTyVarKinds,
    unifyKinds,
    unifyKindsAt,
    surfaceTypeSpan,
    zonkKind,
    kindNeedsZonkIn,
  )
where

import Aihc.Parser.Syntax
  ( BuiltinCon (..),
    Name (..),
    SourceSpan,
    TupleFlavor (..),
    TyVarBinder (..),
    Type (..),
    TypeLiteral (..),
    TypePromotion (..),
    UnqualifiedName (..),
    forallTelescopeBinders,
    fromAnnotation,
    instanceHeadName,
    instanceHeadTypes,
    nameText,
    peelTypeHead,
    tyVarBinderKind,
    tyVarBinderName,
    unqualifiedNameText,
  )
import Aihc.Resolve (ResolutionAnnotation (..), ResolutionNamespace (..))
import Aihc.Tc.Env (TyConFlavor (..), TyConInfo (..), TypeSynonymInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Instantiate (Instantiation (..), instantiate, instantiateWithArgs)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (normalizeFamilyPred)
import Aihc.Tc.Types
import Control.Applicative ((<|>))
import Control.Monad (foldM, replicateM, when, zipWithM, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get)
import Data.IntMap.Strict qualified as IntMap
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

-- | Convert a signature to a type scheme. A free type variable that is a
-- lexically scoped type variable refers to that variable and is not
-- quantified again.
sigToScheme :: Type -> TcM TypeScheme
sigToScheme = signatureScheme True

-- | The scheme of a pattern synonym signature. Its provided context follows
-- the required context, thus it stays in the body.
patSynSigToScheme :: Type -> TcM TypeScheme
patSynSigToScheme = signatureScheme False

signatureScheme :: Bool -> Type -> TcM TypeScheme
signatureScheme floatResult ty = do
  scoped <- getScopedTyVars
  let (explicitBinders, context, body) = splitSigma ty
      freeVars = filter (`Map.notMember` scoped) (freeTypeVars ty)
  rawTvs <- mapM freshSkolemTv freeVars
  kinds <- mapM (const freshKindMeta) freeVars
  let implicitTvs = zipWith setTyVarKind kinds rawTvs
  let implicitEnv = scoped <> Map.fromList (zip freeVars (zip implicitTvs kinds))
  explicitParams <- makeParamEnvWith implicitEnv explicitBinders
  let explicitTvs = map paramTyVar explicitParams
      tvEnv =
        implicitEnv
          <> Map.fromList
            [ (paramName param, (paramTyVar param, paramKind param))
            | param <- explicitParams
            ]
  checkedBody <- checkRuntimeType tvEnv body
  preds <- surfaceContextToPreds tvEnv context
  let (floatedTvs, floatedPreds, tcTy)
        | floatResult = floatResultQuantifiers checkedBody
        | otherwise = ([], [], checkedBody)
  pure (specifiedScheme (implicitTvs <> explicitTvs <> floatedTvs) (preds <> floatedPreds) tcTy)

-- | Move the quantifiers and contexts of a function result to the front of
-- the type. A type synonym can put them there: with @type Action a =
-- forall m. Monad m => m a@, the signature @Bool -> Action Bool@ is
-- @forall m. Monad m => Bool -> m Bool@. The binding is then checked like
-- a signature that has the quantifiers at the front.
floatResultQuantifiers :: TcType -> ([TyVarId], [Pred], TcType)
floatResultQuantifiers ty =
  case ty of
    TcForAllTy tyVar inner ->
      let (tyVars, predicates, body) = floatResultQuantifiers inner
       in (tyVar : tyVars, predicates, body)
    TcQualTy predicates inner ->
      let (tyVars, morePredicates, body) = floatResultQuantifiers inner
       in (tyVars, predicates <> morePredicates, body)
    TcFunTy argument result ->
      let (tyVars, predicates, body) = floatResultQuantifiers result
       in (tyVars, predicates, TcFunTy argument body)
    _ -> ([], [], ty)

-- | Whether a signature contains a partial-signature wildcard.
hasWildcardType :: Type -> Bool
hasWildcardType ty =
  case ty of
    TWildcard -> True
    TApp f a -> hasWildcardType f || hasWildcardType a
    TFun _ a b -> hasWildcardType a || hasWildcardType b
    TParen inner -> hasWildcardType inner
    TAnn _ inner -> hasWildcardType inner
    TContext preds inner -> any hasWildcardType preds || hasWildcardType inner
    TForall _ inner -> hasWildcardType inner
    TTuple _ _ args -> any hasWildcardType args
    TList _ args -> any hasWildcardType args
    _ -> False

-- | The names of the variables of the explicit outer @forall@ of a
-- signature.
explicitForallNames :: Type -> [Text]
explicitForallNames ty = map tyVarBinderName (fst (splitForalls ty))

-- | The type variables that a signature scopes over its binding. Only the
-- variables of an explicit outer @forall@ scope, as in GHC. The given
-- variables are the opened variables of the checked scheme; they keep the
-- source names.
scopedSigTyVars :: [Text] -> [TyVarId] -> Map Text (TyVarId, TcType)
scopedSigTyVars explicitNames tyVars =
  Map.fromList
    [ (tvName tyVar, (tyVar, tvKind tyVar))
    | tyVar <- tyVars,
      tvName tyVar `elem` explicitNames
    ]

-- | The items of a written context, without the empty constraint @()@.
-- @()@ is the empty constraint tuple and gives no predicate. The parser
-- can give it as a unit tuple item or as the unit type constructor, for
-- example in @() =>@, @(()) =>@, and in the pattern synonym form
-- @() => provided =>@. A constraint tuple item, as in @((), Eq a) =>@,
-- gives its components.
flattenSurfaceContext :: [Type] -> [Type]
flattenSurfaceContext = concatMap flattenItem
  where
    flattenItem ty =
      case ty of
        TAnn _ inner -> flattenItem inner
        TParen inner -> flattenItem inner
        TTuple Boxed Unpromoted items -> concatMap flattenItem items
        TCon name _ | nameText name == "()" -> []
        _ -> [ty]

-- | Whether a checked constraint is the empty constraint tuple @()@.
-- A constraint synonym such as @type NoC = (() :: Constraint)@ expands to it.
isUnitConstraintType :: TcType -> TcM Bool
isUnitConstraintType ty = do
  wiring <- getWiring
  pure $ case ty of
    TcTyCon tyCon [] -> tyCon == tcWiringConstraintTupleTyCon wiring
    _ -> False

-- | The predicates of a written context. The empty constraint @()@ gives
-- no predicate, also when a constraint synonym expands to it.
surfaceContextToPreds :: TvKindEnv -> [Type] -> TcM [Pred]
surfaceContextToPreds tvEnv context =
  concat <$> mapM (surfacePredToPreds tvEnv) (flattenSurfaceContext context)

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
  let (nestedTyVars, body') = prenexKindForalls body
  pure (specifiedScheme (implicitTyVars <> explicitTyVars <> nestedTyVars) [] body')

prenexKindForalls :: TcType -> ([TyVarId], TcType)
prenexKindForalls kind =
  case kind of
    TcForAllTy tyVar body ->
      let (tyVars, body') = prenexKindForalls body
       in (tyVar : tyVars, body')
    TcFunTy argument result ->
      let (tyVars, result') = prenexKindForalls result
       in (tyVars, TcFunTy argument result')
    _ -> ([], kind)

checkSurfaceType :: TvKindEnv -> Type -> TcType -> TcM TcType
checkSurfaceType tvEnv ty expected = do
  -- @()@ is the unit type at kind 'Type' and the empty constraint tuple at
  -- kind 'Constraint'. Only the expected kind tells them apart, so a boxed
  -- tuple checked against 'Constraint' is converted here rather than in
  -- 'convertTupleType', which has no expectation to consult.
  kinds <- getKinds
  expected' <- zonkKind expected
  case peelTypeHead ty of
    TTuple Boxed _ [] | expected' == constraintKind kinds -> do
      wiring <- getWiring
      tyCon <- mkWiredTyCon (tcWiringConstraintTupleTyCon wiring) (constraintKind kinds)
      pure (TcTyCon tyCon [])
    -- A wildcard stands for a type of whatever kind is expected. Converting
    -- it without the expectation would give it a fresh @TYPE rep@ instead,
    -- which is wrong wherever the expected kind is not a kind of values:
    -- @Assert 'True _ = ()@ has a wildcard at kind 'Constraint'.
    TWildcard -> freshMetaTvOfKind expected'
    _ -> do
      (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
      unifyKindsAt (surfaceTypeSpan ty) expected actual
      pure tcTy

-- | The source span of a surface type, when its annotations give one.
surfaceTypeSpan :: Type -> Maybe SourceSpan
surfaceTypeSpan ty =
  case ty of
    TAnn ann inner -> fromAnnotation ann <|> surfaceTypeSpan inner
    TParen inner -> surfaceTypeSpan inner
    _ -> Nothing

-- | Check that a surface type is a value-bearing type of kind @TYPE rep@.
-- Unconstrained kind metas default to lifted representation; explicitly
-- unlifted types retain their fixed representation.
checkRuntimeType :: TvKindEnv -> Type -> TcM TcType
checkRuntimeType tvEnv ty = do
  kinds <- getKinds
  (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
  actual' <- zonkKind actual
  case actual' of
    KTYPE {} -> pure tcTy
    KMeta unique -> bindKindMetaAt (surfaceTypeSpan ty) unique (typeKind kinds) >> pure tcTy
    _ -> emitError (surfaceTypeSpan ty) (KindMismatch (typeKind kinds) actual') >> pure tcTy

convertSurfaceTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertSurfaceTypeWithKinds tvEnv ty =
  case ty of
    TAnn ann _
      | Just resolution <- (fromAnnotation ann :: Maybe ResolutionAnnotation),
        resolutionNamespace resolution == ResolutionNamespaceTerm ->
          convertNonSynonymTypeWithKinds tvEnv ty
    _ -> do
      expanded <- expandTypeSynonym tvEnv (peelTypeHead ty)
      case expanded of
        Just result -> pure result
        Nothing -> convertNonSynonymTypeWithKinds tvEnv (peelTypeHead ty)

convertNonSynonymTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertNonSynonymTypeWithKinds tvEnv ty = do
  kinds <- getKinds
  case ty of
    TAnn ann inner ->
      case fromAnnotation ann of
        Just resolution
          | resolutionNamespace resolution == ResolutionNamespaceTerm ->
              convertPromotedSyntaxType tvEnv resolution inner
        _ -> convertSurfaceTypeWithKinds tvEnv inner
    TVar name ->
      inferTypeVariable tvEnv name
    TCon name _ ->
      inferTypeConstructor name
    TBuiltinCon builtin promotion ->
      inferBuiltinTypeConstructor builtin promotion
    TStar {} ->
      knownType tcWiringTypeTyCon
    TApp f a -> do
      function <- convertSurfaceTypeWithKinds tvEnv f
      applyOneArgument tvEnv function a
    TTypeApp f a -> do
      function <- convertSurfaceTypeWithKinds tvEnv f
      applyOneArgument tvEnv function a
    TInfix lhs name _ rhs -> do
      constructor <- inferTypeConstructor name
      applySurfaceTypeArguments tvEnv constructor [lhs, rhs]
    TFun _ a b -> do
      aTy <- checkRuntimeType tvEnv a
      bTy <- checkRuntimeType tvEnv b
      pure (TcFunTy aTy bTy, typeKind kinds)
    TTuple flavor _ args ->
      convertTupleType tvEnv flavor args
    TUnboxedSum args -> do
      tys <- mapM (checkRuntimeType tvEnv) args
      argumentKinds <- mapM tcTypeKind tys
      let arity = length tys
          resultKind = mkTYPEKind kinds (sumRep kinds (map (runtimeRepOrLifted kinds) argumentKinds))
          tyConKind' = foldr KFun resultKind argumentKinds
      wiring <- getWiring
      tyCon <- mkWiredTyCon (tcWiringUnboxedSumTyCon wiring arity) tyConKind'
      pure (TcTyCon tyCon tys, resultKind)
    TList _ [arg] ->
      convertListType tvEnv arg
    TKindSig inner kindTy -> do
      expected <- kindFromSurfaceType tvEnv kindTy
      checkSurfaceType tvEnv inner expected >>= \innerTy -> pure (innerTy, expected)
    TContext preds inner -> do
      predicates <- surfaceContextToPreds tvEnv preds
      (innerType, innerKind) <- convertSurfaceTypeWithKinds tvEnv inner
      pure (TcQualTy predicates innerType, innerKind)
    TWildcard -> do
      -- A partial-signature wildcard stands for a type the checked body
      -- determines. Its representation stays open so an unlifted body
      -- (text's @Char# -> _@ case mappings) can fill it in.
      representation <- freshMetaTvOfKind (runtimeRepKind kinds)
      let kind = mkTYPEKind kinds representation
      meta <- freshMetaTvOfKind kind
      pure (meta, kind)
    TImplicitParam name payload -> do
      payloadType <- checkSurfaceType tvEnv payload (typeKind kinds)
      constraintType <- implicitParamType name payloadType
      pure (constraintType, constraintKind kinds)
    TTypeLit literal -> do
      -- A type-level literal is a type of its own: @3@, @"abc"@ or @'x'@.
      -- Its kind is its sort, and it is equal to no type but an equal
      -- literal of the same sort.
      let converted = convertTypeLiteral literal
      pure (TcTyLit converted, tyLitKind kinds converted)
    TForall telescope inner -> do
      params <- makeParamEnvWith tvEnv (forallTelescopeBinders telescope)
      let tvEnv' = tvEnv <> Map.fromList [(paramName p, (paramTyVar p, paramKind p)) | p <- params]
      (innerTy, innerKind) <- convertSurfaceTypeWithKinds tvEnv' inner
      pure (foldr (TcForAllTy . paramTyVar) innerTy params, innerKind)
    _ -> do
      emitError Nothing (OtherError ("unsupported surface type in kind checker: " <> take 80 (show ty)))
      meta <- freshMetaTv
      pure (meta, typeKind kinds)

-- | The type checker's form of a surface type-level literal. The surface
-- form keeps the source spelling beside the value so that a diagnostic can
-- echo it back; the checker keeps only the value, because two spellings of
-- one value are one type.
convertTypeLiteral :: TypeLiteral -> TyLit
convertTypeLiteral literal =
  case literal of
    TypeLitInteger value _ -> TyLitNat value
    TypeLitSymbol value _ -> TyLitSymbol value
    TypeLitChar value _ -> TyLitChar value

convertPromotedSyntaxType :: TvKindEnv -> ResolutionAnnotation -> Type -> TcM (TcType, TcType)
convertPromotedSyntaxType tvEnv resolution syntax =
  case peelTypeHead syntax of
    TList _ arguments ->
      convertDataConstructorList tvEnv arguments
    TTuple _ _ arguments ->
      convertResolvedConstructorApplication tvEnv resolution arguments
    _ -> convertSurfaceTypeWithKinds tvEnv syntax

convertListType :: TvKindEnv -> Type -> TcM (TcType, TcType)
convertListType tvEnv argument = do
  kinds <- getKinds
  argumentType <- checkSurfaceType tvEnv argument (typeKind kinds)
  result <- listType argumentType
  pure (result, typeKind kinds)

convertDataConstructorList :: TvKindEnv -> [Type] -> TcM (TcType, TcType)
convertDataConstructorList tvEnv arguments = do
  elementKind <- freshKindMeta
  argumentTypes <- mapM (\argument -> checkSurfaceType tvEnv argument elementKind) arguments
  resultKind <- listType elementKind
  let consKind = TcFunTy elementKind (TcFunTy resultKind resultKind)
  nilTyCon <- wiredTyCon tcWiringNilDataCon resultKind
  consTyCon <- wiredTyCon tcWiringConsDataCon consKind
  let nil = TcTyCon nilTyCon []
      cons field rest = TcTyCon consTyCon [field, rest]
  pure (foldr cons nil argumentTypes, resultKind)

-- | Use the wired sum identity and preserve every alternative representation.
unboxedSumType :: [TcType] -> TcM TcType
unboxedSumType types = do
  kinds <- getKinds
  argumentKinds <- mapM tcTypeKind types
  let resultKind = mkTYPEKind kinds (sumRep kinds (map (runtimeRepOrLifted kinds) argumentKinds))
      fallbackKind = foldr KFun resultKind argumentKinds
  constructor <- wiredTyCon (\wiring -> tcWiringUnboxedSumTyCon wiring (length types)) fallbackKind
  pure (TcTyCon constructor types)

convertTupleType :: TvKindEnv -> TupleFlavor -> [Type] -> TcM (TcType, TcType)
convertTupleType tvEnv flavor arguments = do
  kinds <- getKinds
  argumentTypes <-
    case flavor of
      Boxed -> mapM (\argument -> checkSurfaceType tvEnv argument (typeKind kinds)) arguments
      Unboxed -> mapM (checkRuntimeType tvEnv) arguments
  argumentKinds <- mapM tcTypeKind argumentTypes
  let argumentReps = map (runtimeRepOrLifted kinds) argumentKinds
      arity = length argumentTypes
      fallbackResultKind =
        case flavor of
          Boxed -> typeKind kinds
          Unboxed -> mkTYPEKind kinds (tupleRep kinds argumentReps)
      fallbackKind = foldr KFun fallbackResultKind argumentKinds
  wired <- wiredTupleTyCon flavor arity
  -- The wiring gives the full identity of the tuple type constructor.
  -- A bare name lookup can find a different constructor with the same name.
  tyCon <- mkWiredTyCon wired fallbackKind
  let tupleType = TcTyCon tyCon argumentTypes
  tupleKind <- tcTypeKind tupleType
  pure (tupleType, tupleKind)

convertResolvedConstructorApplication :: TvKindEnv -> ResolutionAnnotation -> [Type] -> TcM (TcType, TcType)
convertResolvedConstructorApplication tvEnv resolution arguments = do
  maybeInfo <- lookupResolvedTypeSyntax resolution
  case maybeInfo of
    Nothing -> inferUnknownType
    Just info -> do
      constructorUse <- kindedTyConUse info
      applySurfaceTypeArguments tvEnv constructorUse arguments

applySurfaceTypeArguments :: TvKindEnv -> (TcType, TcType) -> [Type] -> TcM (TcType, TcType)
applySurfaceTypeArguments tvEnv = foldM (applyOneArgument tvEnv)

-- | Apply a converted type to one surface argument.
--
-- Only the expected kind tells @()@ at kind 'Type' from @()@ at kind
-- 'Constraint', so an argument of a constraint-kinded family such as
-- @Assert b ()@ has to be /checked/ against the kind the function demands
-- rather than converted on its own and unified afterwards.
--
-- The expectation is pushed down only when that kind is 'Constraint', which
-- is closed and holds no meta variables. Pushing every argument kind down
-- would unify a use site against the kind variables of a poly-kinded
-- declaration -- @Compose (f :: k -> Type) (g :: l -> k)@ -- and pin them,
-- which the convert-and-unify path below does not do.
applyOneArgument :: TvKindEnv -> (TcType, TcType) -> Type -> TcM (TcType, TcType)
applyOneArgument tvEnv (functionType, functionKind) argument = do
  functionKind' <- zonkKind functionKind
  kinds <- getKinds
  case functionKind' of
    -- An argument whose kind does not follow from its own form is checked
    -- against the parameter kind: @()@ is the unit type at kind 'Type' and
    -- the empty constraint tuple at kind 'Constraint', and a wildcard
    -- stands for a type of whatever kind is wanted (@Assert _ errMsg@ has
    -- one at kind 'Bool').
    --
    -- Every other argument is converted on its own and unified afterwards.
    -- Checking those against the parameter kind as well would pin the kind
    -- variables of a poly-kinded head that must stay open across an
    -- instance on it -- see @polykinded-newtype-deriving-instance@.
    KFun argumentKind resultKind
      | argumentKind == constraintKind kinds || isWildcardArgument argument -> do
          argumentType <- checkSurfaceType tvEnv argument argumentKind
          resultKind' <- zonkKind resultKind
          pure (mkAppTy functionType argumentType, resultKind')
    _ -> do
      (argumentType, argumentKind) <- convertSurfaceTypeWithKinds tvEnv argument
      resultKind <- freshKindMeta
      unifyKindsAt (surfaceTypeSpan argument) functionKind' (KFun argumentKind resultKind)
      resultKind' <- zonkKind resultKind
      pure (mkAppTy functionType argumentType, resultKind')

-- | Whether an argument is a bare wildcard, once parentheses and source
-- annotations are peeled off.
isWildcardArgument :: Type -> Bool
isWildcardArgument argument =
  case peelTypeHead argument of
    TWildcard -> True
    _ -> False

expandTypeSynonym :: TvKindEnv -> Type -> TcM (Maybe (TcType, TcType))
expandTypeSynonym tvEnv ty =
  case synonymApplicationSpine ty of
    Just (name, arguments) -> do
      maybeInfo <- lookupResolvedTyCon name
      case maybeInfo of
        Just info
          | Just synonym <- tciTypeSynonym info,
            Just {} <- tsiBody synonym -> do
              let ForAll variables _ _ = tciKindScheme info
              instantiation <- instantiateWithArgs (tciKindScheme info)
              let substitution = Map.fromList (zip (map tvUnique variables) (instTypeArgs instantiation))
                  specialize variable = do
                    kind <- zonkKind (tvKind variable)
                    pure (setTyVarKind (applySubst substitution kind) variable)
              parameters <- mapM specialize (tsiParams synonym)
              let specialized = synonym {tsiParams = parameters, tsiBody = applySubst substitution <$> tsiBody synonym}
              Just <$> instantiateTypeSynonym tvEnv (nameText name) specialized arguments
        _ -> pure Nothing
    Nothing -> pure Nothing

-- | The head name and the arguments of a type constructor application,
-- whether it is spelled prefix (@Assert b c@) or infix (@x <= y@). A
-- synonym applied infix expands exactly as one applied prefix does. A
-- promoted infix constructor names a data constructor, never a synonym.
synonymApplicationSpine :: Type -> Maybe (Name, [Type])
synonymApplicationSpine ty =
  case typeApplicationSpine ty of
    (TCon name _, arguments) -> Just (name, arguments)
    (TInfix lhs name Unpromoted rhs, arguments) -> Just (name, lhs : rhs : arguments)
    _ -> Nothing

instantiateTypeSynonym :: TvKindEnv -> Text -> TypeSynonymInfo -> [Type] -> TcM (TcType, TcType)
instantiateTypeSynonym tvEnv synonymName synonym arguments = do
  kinds <- getKinds
  case tsiBody synonym of
    Nothing -> do
      emitError Nothing (OtherError ("recursive or incomplete type synonym: " <> T.unpack synonymName))
      meta <- freshMetaTv
      pure (meta, typeKind kinds)
    Just body -> do
      let params = tsiParams synonym
          arity = length params
          (synonymArguments, remainingArguments) = splitAt arity arguments
      if length synonymArguments /= arity
        then do
          emitError Nothing (OtherError ("type synonym " <> T.unpack synonymName <> " is not fully applied"))
          meta <- freshMetaTv
          pure (meta, typeKind kinds)
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
      unifyKindsAt (surfaceTypeSpan argument) functionKind (KFun argumentKind resultKind)
      zonkedResultKind <- zonkKind resultKind
      applyRemainingArguments (mkAppTy functionType argumentType, zonkedResultKind) rest

typeApplicationSpine :: Type -> (Type, [Type])
typeApplicationSpine = go []
  where
    go arguments (TAnn _ inner) = go arguments inner
    go arguments (TApp function argument) = go (argument : arguments) function
    go arguments (TTypeApp function argument) = go (argument : arguments) function
    go arguments headType = (headType, arguments)

expandTcTypeSynonyms :: Set TcTypeKey -> TcType -> TcM TcType
expandTcTypeSynonyms expanding ty = do
  case ty of
    TcTyVar {} -> pure ty
    TcMetaTv {} -> pure ty
    TcArrowTy -> pure ty
    TcTyLit {} -> pure ty
    -- A synonym is never poly-kinded in this form: it is expanded where
    -- it is written.
    TcKindedTyCon tyCon kindArguments -> TcKindedTyCon tyCon <$> mapM (expandTcTypeSynonyms expanding) kindArguments
    TcTyCon tyCon arguments -> do
      expandedArguments <- mapM (expandTcTypeSynonyms expanding) arguments
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo >>= tciTypeSynonym of
        Just synonym
          | Just body <- tsiBody synonym,
            let params = tsiParams synonym,
            length expandedArguments >= length params ->
              if tyConKey tyCon `Set.member` expanding
                then do
                  emitError Nothing (OtherError ("recursive type synonym: " <> T.unpack (tyConName tyCon)))
                  pure (TcTyCon tyCon expandedArguments)
                else do
                  let (synonymArguments, remainingArguments) = splitAt (length params) expandedArguments
                      substitution = Map.fromList (zip (map tvUnique params) synonymArguments)
                      expandedBody = applySubst substitution body
                      expanding' = Set.insert (tyConKey tyCon) expanding
                  normalizedBody <- expandTcTypeSynonyms expanding' expandedBody
                  expandTcTypeSynonyms expanding' (foldl mkAppTy normalizedBody remainingArguments)
        _ -> pure (TcTyCon tyCon expandedArguments)
    TcFunTy argument result -> TcFunTy <$> expandTcTypeSynonyms expanding argument <*> expandTcTypeSynonyms expanding result
    TcForAllTy tyVar body -> TcForAllTy tyVar <$> expandTcTypeSynonyms expanding body
    TcQualTy predicates body -> TcQualTy <$> mapM expandPredicate predicates <*> expandTcTypeSynonyms expanding body
    TcAppTy function argument -> mkAppTy <$> expandTcTypeSynonyms expanding function <*> expandTcTypeSynonyms expanding argument
  where
    expandPredicate predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM (expandTcTypeSynonyms expanding) arguments
        EqPred left right -> EqPred <$> expandTcTypeSynonyms expanding left <*> expandTcTypeSynonyms expanding right
        IParamPred name payload -> IParamPred name <$> expandTcTypeSynonyms expanding payload
        IrredPred constraint -> IrredPred <$> expandTcTypeSynonyms expanding constraint
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            <$> mapM expandVariable variables
            <*> mapM expandPredicate antecedents
            <*> expandPredicate consequent
    expandVariable variable = do
      kind <- expandTcTypeSynonyms expanding (tvKind variable)
      pure (setTyVarKind kind variable)

inferTypeVariable :: TvKindEnv -> UnqualifiedName -> TcM (TcType, TcType)
inferTypeVariable tvEnv name =
  let n = unqualifiedNameText name
   in case Map.lookup n tvEnv of
        Just (tv, kind) -> pure (TcTyVar tv, kind)
        Nothing -> inferUnknownType

inferTypeConstructor :: Name -> TcM (TcType, TcType)
inferTypeConstructor name = do
  mInfo <- lookupResolvedTyCon name
  wiring <- getWiring
  let typeTyCon' = tcWiringTypeTyCon wiring
      constraintTyCon' = tcWiringConstraintTyCon wiring
      isWired wired info =
        tyConModuleName (tciTyCon info) == tyConModuleName wired
          && tciName info == tyConName wired
  case mInfo of
    Just info
      | isWired typeTyCon' info ->
          knownType tcWiringTypeTyCon
    Just info
      | isWired constraintTyCon' info ->
          knownType tcWiringConstraintTyCon
    Just info -> kindedTyConUse info
    Nothing
      | nameText name == tyConName typeTyCon' -> knownType tcWiringTypeTyCon
      | nameText name == tyConName constraintTyCon' -> knownType tcWiringConstraintTyCon
      | otherwise -> inferUnknownType

-- | A use of a type constructor with no visible argument yet, and its kind.
-- The kind scheme is instantiated with fresh metas, and a poly-kinded
-- constructor keeps them as its kind arguments. Unification solves them,
-- so the type keeps the kind at which the use site uses the constructor.
kindedTyConUse :: TyConInfo -> TcM (TcType, TcType)
kindedTyConUse info = do
  instantiation <- instantiateWithArgs (tciKindScheme info)
  pure (kindedTyCon (tciTyCon info) (instTypeArgs instantiation), instType instantiation)

-- | Give a bare type constructor the kind arguments of a use. A type
-- constructor with a visible argument, or with no kind variable, keeps its
-- form.
kindedTyConAt :: TcType -> TcType -> TcM TcType
kindedTyConAt useKind ty =
  case ty of
    TcTyCon tyCon [] -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo of
        Just info
          | ForAll (_ : _) _ _ <- tciKindScheme info -> do
              (kinded, kind) <- kindedTyConUse info
              unifyKinds useKind kind
              pure kinded
        _ -> pure ty
    _ -> pure ty

instantiateTyConKind :: TyConInfo -> TcM TcType
instantiateTyConKind info = do
  (kindType, _) <- instantiate (tciKindScheme info)
  pure kindType

-- | A built-in constructor used as a type: @[]@, @(:)@, @(,)@, @(->)@, and
-- their promoted forms such as @\'[]@.  A promoted constructor is a data
-- constructor lifted into the type namespace, so it gets the kind of the
-- data constructor rather than the kind of the type constructor.
inferBuiltinTypeConstructor :: BuiltinCon -> TypePromotion -> TcM (TcType, TcType)
inferBuiltinTypeConstructor builtin promotion =
  case builtin of
    BuiltinList
      | promoted -> do
          -- @\'[]@ has kind @[k]@.
          resultKind <- listType =<< freshKindMeta
          tyCon <- wiredTyCon tcWiringNilDataCon resultKind
          pure (TcTyCon tyCon [], resultKind)
      | otherwise -> do
          kinds <- getKinds
          tyCon <- wiredTyCon tcWiringListTyCon (KFun (typeKind kinds) (typeKind kinds))
          pure (TcTyCon tyCon [], KFun (typeKind kinds) (typeKind kinds))
    BuiltinCons
      | promoted -> do
          -- @\'(:)@ has kind @k -> [k] -> [k]@.
          elementKind <- freshKindMeta
          listKind <- listType elementKind
          let kind = KFun elementKind (KFun listKind listKind)
          tyCon <- wiredTyCon tcWiringConsDataCon kind
          pure (TcTyCon tyCon [], kind)
      | otherwise -> do
          kinds <- getKinds
          let kind = KFun (typeKind kinds) (KFun (listTypeKind (typeKind kinds)) (listTypeKind (typeKind kinds)))
          tyCon <- wiredTyCon tcWiringConsDataCon kind
          pure (TcTyCon tyCon [], kind)
    BuiltinTuple flavor arity
      | promoted -> do
          -- @\'(,)@ has kind @k1 -> k2 -> (k1, k2)@, so the result kind
          -- names the tuple type constructor of the same arity.
          argKinds <- replicateM arity freshKindMeta
          wiredType <- wiredTupleTyCon flavor arity
          kinds <- getKinds
          tupleTyCon <- mkWiredTyCon wiredType (foldr KFun (typeKind kinds) (replicate arity (typeKind kinds)))
          let resultKind = TcTyCon tupleTyCon argKinds
              kind = foldr KFun resultKind argKinds
          wiredData <- wiredTupleDataCon flavor arity
          tyCon <- mkWiredTyCon wiredData kind
          pure (TcTyCon tyCon [], kind)
      | otherwise -> do
          kinds <- getKinds
          wired <- wiredTupleTyCon flavor arity
          let argKinds = replicate arity (typeKind kinds)
              kind = foldr KFun (typeKind kinds) argKinds
          knownWiredType wired kind
    BuiltinArrow -> do
      kinds <- getKinds
      let kind = KFun (typeKind kinds) (KFun (typeKind kinds) (typeKind kinds))
      arrow <- arrowType
      pure (arrow, kind)
  where
    promoted = promotion == Promoted

-- | A type constructor of kind @Type@ whose identity comes from the
-- wiring tables.
knownType :: (TcWiring -> TyCon) -> TcM (TcType, TcType)
knownType select = do
  kinds <- getKinds
  let kind = typeKind kinds
  tyCon <- wiredTyCon select kind
  pure (TcTyCon tyCon [], kind)

-- | The same, for an identity already taken from the wiring.
knownWiredType :: TyCon -> TcType -> TcM (TcType, TcType)
knownWiredType wired kind = do
  tyCon <- mkWiredTyCon wired kind
  pure (TcTyCon tyCon [], kind)

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

-- | The kinds of the first visible arguments of a type constructor kind.
takeVisibleArgumentKinds :: Int -> TcType -> [TcType]
takeVisibleArgumentKinds = go
  where
    go remaining (KFun argument result)
      | remaining > 0 = argument : go (remaining - 1) result
    go _ _ = []

tyConKindFromParams :: [ParamInfo] -> Maybe Type -> TcM TcType
tyConKindFromParams = tyConKindFromParamsWith Map.empty

tyConKindFromParamsWith :: TvKindEnv -> [ParamInfo] -> Maybe Type -> TcM TcType
tyConKindFromParamsWith outerEnv params maybeResultKind = do
  kinds <- getKinds
  let tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params] <> outerEnv
  resultKind <- maybe (pure (typeKind kinds)) (kindFromSurfaceType tvEnv) maybeResultKind
  pure (foldr (KFun . paramKind) resultKind params)

kindFromSurfaceType :: TvKindEnv -> Type -> TcM TcType
kindFromSurfaceType tvEnv ty = do
  kinds <- getKinds
  case peelTypeHead ty of
    TStar {} -> pure (typeKind kinds)
    other -> do
      (tcType, kind) <- convertSurfaceTypeWithKinds tvEnv other
      unifyKindsAt (surfaceTypeSpan ty) kind (typeKind kinds)
      pure tcType

unifyKinds :: TcType -> TcType -> TcM ()
unifyKinds = unifyKindsAt Nothing

-- | Unify two kinds and report a mismatch at the given span.
unifyKindsAt :: Maybe SourceSpan -> TcType -> TcType -> TcM ()
unifyKindsAt sp expected actual = do
  expected' <- zonkKind expected >>= refineGivenKind
  actual' <- zonkKind actual >>= refineGivenKind
  case (expected', actual') of
    (TcMetaTv unique, kind) -> bindKindMetaAt sp unique kind
    (kind, TcMetaTv unique) -> bindKindMetaAt sp unique kind
    (TcTyVar left, TcTyVar right)
      | left == right -> pure ()
    (TcTyVar {}, kind)
      | isConcreteRuntimeRep kind -> pure ()
    (kind, TcTyVar {})
      | isConcreteRuntimeRep kind -> pure ()
    (TcTyCon left leftArguments, TcTyCon right rightArguments)
      | left == right,
        length leftArguments == length rightArguments ->
          zipWithM_ (unifyKindsAt sp) leftArguments rightArguments
    (TcKindedTyCon left leftArguments, TcKindedTyCon right rightArguments)
      | left == right,
        length leftArguments == length rightArguments ->
          zipWithM_ (unifyKindsAt sp) leftArguments rightArguments
    -- A bare constructor that no use site kinded agrees with every kinding.
    (TcKindedTyCon left _, TcTyCon right [])
      | left == right -> pure ()
    (TcTyCon left [], TcKindedTyCon right _)
      | left == right -> pure ()
    (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
      unifyKindsAt sp leftArgument rightArgument >> unifyKindsAt sp leftResult rightResult
    (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) ->
      unifyKindsAt sp leftFunction rightFunction >> unifyKindsAt sp leftArgument rightArgument
    (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody)
      | leftVar == rightVar -> unifyKindsAt sp leftBody rightBody
    (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody)
      | leftPredicates == rightPredicates -> unifyKindsAt sp leftBody rightBody
    _ -> emitError sp (KindMismatch expected' actual')

-- | Use scoped equality evidence when a pattern refines a kind variable.
-- With no equality in scope there is nothing to rewrite with, and the walk
-- would only rebuild the kind as it already is. Most scopes are like that
-- and every kind unification asks.
refineGivenKind :: TcType -> TcM TcType
refineGivenKind kind = do
  predicates <- getGivenPredicates
  case [(left, right) | EqPred left right <- predicates] of
    [] -> pure kind
    givenEqualities -> do
      equalities <- mapM zonkEquality givenEqualities
      pure (rewrite equalities Set.empty kind)
  where
    zonkEquality (left, right) = (,) <$> zonkKind left <*> zonkKind right
    rewrite equalities visited ty
      | ty `Set.member` visited = ty
      | otherwise =
          case [replacement | (left, right) <- equalities, Just replacement <- [replace ty left right]] of
            replacement : _ -> rewrite equalities (Set.insert ty visited) replacement
            [] -> case ty of
              TcTyCon constructor arguments -> TcTyCon constructor (map recur arguments)
              TcFunTy argument result -> TcFunTy (recur argument) (recur result)
              TcAppTy function argument -> TcAppTy (recur function) (recur argument)
              _ -> ty
      where
        recur = rewrite equalities visited
    replace ty left@(TcTyVar a) right@(TcTyVar b)
      | tvUnique a > tvUnique b, sameType ty left = Just right
      | tvUnique b > tvUnique a, sameType ty right = Just left
    replace _ TcTyVar {} TcTyVar {} = Nothing
    replace ty left@TcTyVar {} right | sameType ty left, not (sameType left right) = Just right
    replace ty left right@TcTyVar {} | sameType ty right, not (sameType left right) = Just left
    replace _ _ _ = Nothing

-- | Rewrite the kind of every type variable in a type with the equality
-- evidence that is in scope, the way 'refineGivenKind' rewrites one kind.
--
-- A GADT match can refine a kind variable: matching @typeRepKind f@
-- against the @Fun@ pattern gives @k ~ (arg -> res)@, which is what makes
-- @f x@ well kinded for an @f :: k@ bound by an earlier match. The
-- refinement lives in the constraint solver, so a type recorded without it
-- carries the unrefined variable, and a later pass that recomputes kinds
-- with no evidence to hand -- the FC converter -- cannot apply @f@ to
-- anything. Applying the refinement before the type is recorded keeps
-- those passes working from a type whose kinds already agree.
refineGivenTyVarKinds :: TcType -> TcM TcType
refineGivenTyVarKinds ty = do
  predicates <- getGivenPredicates
  if null [() | EqPred _ _ <- predicates]
    then pure ty
    else go ty
  where
    go t =
      case t of
        TcTyVar tyVar -> do
          kind <- refineGivenKind (tvKind tyVar)
          pure (TcTyVar (setTyVarKind kind tyVar))
        TcTyCon tyCon arguments -> TcTyCon tyCon <$> mapM go arguments
        TcFunTy argument result -> TcFunTy <$> go argument <*> go result
        TcAppTy function argument -> TcAppTy <$> go function <*> go argument
        _ -> pure t

-- | Whether a kind is a runtime representation with no variables left in
-- it, so that a representation-polymorphic variable may take it.
--
-- The head has to be a promoted representation constructor, and every
-- argument has to be free of variables: a @TupleRep@ whose fields are
-- still unsolved fixes no layout, and neither does a @BoxedRep@ of an
-- unknown levity. The head is recognised by its namespace and its name,
-- as the representation matchers in "Aihc.Tc.Types" are.
isConcreteRuntimeRep :: TcType -> Bool
isConcreteRuntimeRep ty =
  case ty of
    TcTyCon tyCon arguments ->
      tyConNamespace tyCon == ResolutionNamespaceTerm
        && "Rep" `T.isSuffixOf` tyConName tyCon
        && all isGroundKind arguments
    _ -> False

-- | Whether a kind contains no type variable and no unsolved meta.
isGroundKind :: TcType -> Bool
isGroundKind ty =
  case ty of
    TcTyVar {} -> False
    TcMetaTv {} -> False
    TcArrowTy -> False
    -- A literal names one type and mentions nothing, so it is ground.
    TcTyLit {} -> True
    TcTyCon _ arguments -> all isGroundKind arguments
    TcKindedTyCon _ kindArguments -> all isGroundKind kindArguments
    TcFunTy argument result -> isGroundKind argument && isGroundKind result
    TcForAllTy {} -> False
    TcQualTy _ body -> isGroundKind body
    TcAppTy function argument -> isGroundKind function && isGroundKind argument

-- | Solve a kind meta with a kind, with no source span to report at.
bindKindMeta :: Unique -> TcType -> TcM ()
bindKindMeta = bindKindMetaAt Nothing

bindKindMetaAt :: Maybe SourceSpan -> Unique -> TcType -> TcM ()
bindKindMetaAt sp u kind
  | kind == TcMetaTv u = pure ()
  | occursInKind u kind = emitError sp (KindMismatch (KMeta u) kind)
  | otherwise = writeMetaTv u kind

-- | Zonk a kind, giving back the kind itself when nothing in it changes.
--
-- A kind is walked over and over -- every meta-variable the checker
-- allocates carries one, and every unification zonks both sides -- and
-- almost every one of those walks finds a settled kind such as
-- @TYPE (BoxedRep Lifted)@. Asking first whether the kind holds anything
-- to rewrite is a pure walk that allocates nothing, and it keeps the
-- checker from filling the heap with equal copies of the kinds it has.
zonkKind :: TcType -> TcM TcType
zonkKind kind = do
  state <- lift get
  if kindNeedsZonkIn state kind then rebuildZonkedKind kind else pure kind

-- | Whether 'rebuildZonkedKind' would change anything in a kind: a solved
-- meta-variable, or a type synonym to expand. The two walk the same shapes.
kindNeedsZonkIn :: TcState -> TcType -> Bool
kindNeedsZonkIn state = goKind
  where
    goKind kind =
      case kind of
        TcArrowTy -> False
        TcTyLit {} -> False
        TcMetaTv (Unique key) -> IntMap.member key (tcsMetaSolutions state)
        TcTyVar tyVar -> goKind (tvKind tyVar)
        TcTyCon tyCon arguments
          | Just synonym <- kindSynonymIn state tyCon,
            Just {} <- tsiBody synonym,
            length arguments >= length (tsiParams synonym) ->
              True
          | otherwise -> any goKind arguments
        TcKindedTyCon _ kindArguments -> any goKind kindArguments
        TcFunTy argument result -> goKind argument || goKind result
        TcForAllTy tyVar body -> goKind (tvKind tyVar) || goKind body
        TcQualTy predicates body -> any goPred predicates || goKind body
        TcAppTy function argument -> goKind function || goKind argument
    goPred predicate =
      case predicate of
        ClassPred _ arguments -> any goKind arguments
        EqPred left right -> goKind left || goKind right
        IParamPred _ payload -> goKind payload
        IrredPred constraint -> goKind constraint
        QuantifiedPred variables antecedents consequent ->
          any (goKind . tvKind) variables || any goPred antecedents || goPred consequent

rebuildZonkedKind :: TcType -> TcM TcType
rebuildZonkedKind kind =
  case kind of
    TcArrowTy -> pure kind
    TcTyLit {} -> pure kind
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      case solution of
        Nothing -> pure kind
        Just solved -> do
          state <- lift get
          -- Keep a settled solution and avoid another store write.
          if kindNeedsZonkIn state solved
            then do
              zonked <- rebuildZonkedKind solved
              writeMetaTv unique zonked
              pure zonked
            else pure solved
    TcTyVar tyVar -> do
      kind' <- rebuildZonkedKind (tvKind tyVar)
      pure (TcTyVar (setTyVarKind kind' tyVar))
    TcTyCon tyCon arguments -> do
      let tyCon' = tyCon
      maybeSynonym <- lookupKindSynonym tyCon'
      case maybeSynonym of
        Just synonym
          | Just {} <- tsiBody synonym,
            length arguments >= length (tsiParams synonym) ->
              rebuildZonkedKind =<< expandTcTypeSynonyms Set.empty (TcTyCon tyCon' arguments)
        _ -> TcTyCon tyCon' <$> mapM rebuildZonkedKind arguments
    TcKindedTyCon tyCon kindArguments -> TcKindedTyCon tyCon <$> mapM rebuildZonkedKind kindArguments
    TcFunTy argument result -> TcFunTy <$> rebuildZonkedKind argument <*> rebuildZonkedKind result
    TcForAllTy tyVar body -> do
      kind' <- rebuildZonkedKind (tvKind tyVar)
      TcForAllTy (setTyVarKind kind' tyVar) <$> rebuildZonkedKind body
    TcQualTy predicates body -> TcQualTy <$> mapM zonkKindPred predicates <*> rebuildZonkedKind body
    TcAppTy function argument -> TcAppTy <$> rebuildZonkedKind function <*> rebuildZonkedKind argument
  where
    zonkKindPred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM rebuildZonkedKind arguments
        EqPred left right -> EqPred <$> rebuildZonkedKind left <*> rebuildZonkedKind right
        IParamPred name payload -> IParamPred name <$> rebuildZonkedKind payload
        IrredPred constraint -> IrredPred <$> rebuildZonkedKind constraint
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            <$> mapM zonkVariable variables
            <*> mapM zonkKindPred antecedents
            <*> zonkKindPred consequent
    zonkVariable variable = setTyVarKind <$> rebuildZonkedKind (tvKind variable) <*> pure variable

-- | The synonym declaration of a type constructor in a kind, if it has one.
--
-- A promoted data constructor is never a synonym. The common kind
-- constructors @BoxedRep@ and @Lifted@ thus need no environment lookup.
lookupKindSynonym :: TyCon -> TcM (Maybe TypeSynonymInfo)
lookupKindSynonym tyCon = do
  state <- lift get
  pure (kindSynonymIn state tyCon)

kindSynonymIn :: TcState -> TyCon -> Maybe TypeSynonymInfo
kindSynonymIn state tyCon
  | tyConNamespace tyCon == ResolutionNamespaceTerm = Nothing
  | otherwise = tciTypeSynonym =<< Map.lookup (tyConKey tyCon) (tcsGlobalTyCons state)

defaultKindMetas :: TcType -> TcM TcType
defaultKindMetas = settleKindMetas Nothing

-- | Like 'defaultKindMetas', but a kind variable proper is left open and
-- reported to the callback instead of defaulting to 'Type'. Everything
-- else still settles, a representation meta-variable included: 'Type' is
-- the only kind a variable may still be open in.
deferKindMetas :: (Unique -> TcM ()) -> TcType -> TcM TcType
deferKindMetas = settleKindMetas . Just

-- A kind with no meta-variable in it has nothing to settle, and walking it
-- would only rebuild it as it already is.
settleKindMetas :: Maybe (Unique -> TcM ()) -> TcType -> TcM TcType
settleKindMetas _ kind
  | not (kindMentionsMeta kind) = pure kind
settleKindMetas defer kind =
  case kind of
    TcArrowTy -> pure kind
    TcTyLit {} -> pure kind
    TcMetaTv unique -> do
      solution <- readMetaTv unique
      case solution of
        Just solved -> do
          -- A partially solved kind such as @k1 -> k2@ keeps its shape; only
          -- the metas that remain inside it default.
          defaulted <- recur =<< zonkKind solved
          writeMetaTv unique defaulted
          pure defaulted
        Nothing -> do
          tracked <- isTrackedKindMeta unique
          case (tracked, defer) of
            (False, _) -> pure kind
            (True, Just onDefer) -> onDefer unique >> pure kind
            (True, Nothing) -> do
              kinds <- getKinds
              writeMetaTv unique (typeKind kinds) >> pure (typeKind kinds)
    TcTyVar tyVar -> do
      kind' <- recur (tvKind tyVar)
      pure (TcTyVar (setTyVarKind kind' tyVar))
    KTYPE (TcMetaTv representation) -> do
      -- An open representation defaults to lifted, not to 'Type'. The meta
      -- may come from instantiating a representation-polymorphic kind, so
      -- this does not depend on kind-meta tracking.
      solution <- readMetaTv representation
      case solution of
        Just {} -> recur =<< zonkKind kind
        Nothing -> do
          kinds <- getKinds
          writeMetaTv representation (liftedRep kinds) >> pure (typeKind kinds)
    BoxedRep (TcMetaTv levity) -> do
      -- An open boxed levity defaults to Lifted at a kind boundary.
      solution <- readMetaTv levity
      case solution of
        Just {} -> recur =<< zonkKind kind
        Nothing -> do
          kinds <- getKinds
          writeMetaTv levity (TcTyCon (kindsDataCon kinds "Lifted" 0) [])
          pure (liftedRep kinds)
    TcTyCon tyCon arguments -> TcTyCon tyCon <$> mapM recur arguments
    TcKindedTyCon tyCon kindArguments -> TcKindedTyCon tyCon <$> mapM recur kindArguments
    TcFunTy argument result -> TcFunTy <$> recur argument <*> recur result
    TcForAllTy tyVar body -> do
      kind' <- recur (tvKind tyVar)
      TcForAllTy (setTyVarKind kind' tyVar) <$> recur body
    TcQualTy predicates body -> TcQualTy <$> mapM defaultKindPred predicates <*> recur body
    TcAppTy function argument -> TcAppTy <$> recur function <*> recur argument
  where
    recur = settleKindMetas defer
    defaultKindPred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className <$> mapM recur arguments
        EqPred left right -> EqPred <$> recur left <*> recur right
        IParamPred name payload -> IParamPred name <$> recur payload
        IrredPred constraint -> IrredPred <$> recur constraint
        QuantifiedPred variables antecedents consequent ->
          QuantifiedPred
            <$> mapM defaultVariable variables
            <*> mapM defaultKindPred antecedents
            <*> defaultKindPred consequent
    defaultVariable variable = setTyVarKind <$> recur (tvKind variable) <*> pure variable

-- | Whether a kind mentions a meta-variable anywhere, the kind of a type
-- variable it binds or occurs in included.
kindMentionsMeta :: TcType -> Bool
kindMentionsMeta kind =
  case kind of
    TcMetaTv {} -> True
    TcArrowTy -> False
    TcTyLit {} -> False
    TcTyVar tyVar -> kindMentionsMeta (tvKind tyVar)
    TcTyCon _ arguments -> any kindMentionsMeta arguments
    TcKindedTyCon _ kindArguments -> any kindMentionsMeta kindArguments
    TcFunTy argument result -> kindMentionsMeta argument || kindMentionsMeta result
    TcForAllTy tyVar body -> kindMentionsMeta (tvKind tyVar) || kindMentionsMeta body
    TcQualTy predicates body -> any predicateMentions predicates || kindMentionsMeta body
    TcAppTy function argument -> kindMentionsMeta function || kindMentionsMeta argument
  where
    predicateMentions predicate =
      case predicate of
        ClassPred _ arguments -> any kindMentionsMeta arguments
        EqPred left right -> kindMentionsMeta left || kindMentionsMeta right
        IParamPred _ payload -> kindMentionsMeta payload
        IrredPred constraint -> kindMentionsMeta constraint
        QuantifiedPred variables antecedents consequent ->
          any (kindMentionsMeta . tvKind) variables
            || any predicateMentions antecedents
            || predicateMentions consequent

freshKindMeta :: TcM TcType
freshKindMeta = do
  unique <- freshUnique
  trackKindMeta unique
  pure (TcMetaTv unique)

occursInKind :: Unique -> TcType -> Bool
occursInKind needle kind =
  case kind of
    TcArrowTy -> False
    TcTyLit {} -> False
    TcMetaTv unique -> unique == needle
    TcTyVar tyVar -> occursInKind needle (tvKind tyVar)
    TcTyCon _ arguments -> any (occursInKind needle) arguments
    TcKindedTyCon _ kindArguments -> any (occursInKind needle) kindArguments
    TcFunTy argument result -> occursInKind needle argument || occursInKind needle result
    TcForAllTy tyVar body -> occursInKind needle (tvKind tyVar) || occursInKind needle body
    TcQualTy predicates body -> any occursInPred predicates || occursInKind needle body
    TcAppTy function argument -> occursInKind needle function || occursInKind needle argument
  where
    occursInPred predicate =
      case predicate of
        ClassPred _ arguments -> any (occursInKind needle) arguments
        EqPred left right -> occursInKind needle left || occursInKind needle right
        IParamPred _ payload -> occursInKind needle payload
        IrredPred constraint -> occursInKind needle constraint
        QuantifiedPred variables antecedents consequent ->
          any (occursInKind needle . tvKind) variables
            || any occursInPred antecedents
            || occursInPred consequent

tcTypeKind :: TcType -> TcM TcType
tcTypeKind ty =
  case ty of
    -- @(->)@ takes two ordinary types and gives one.
    TcArrowTy -> do
      kinds <- getKinds
      pure (KFun (typeKind kinds) (KFun (typeKind kinds) (typeKind kinds)))
    TcTyVar tyVar -> zonkKind (tvKind tyVar)
    TcTyLit literal -> do
      kinds <- getKinds
      pure (tyLitKind kinds literal)
    TcMetaTv unique -> readMetaTvKind unique >>= zonkKind
    TcKindedTyCon tyCon kindArguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo of
        Just info -> do
          let ForAll variables _ body = tciKindScheme info
          zonkKind (applySubst (Map.fromList (zip (map tvUnique variables) kindArguments)) body)
        Nothing -> tcTypeKind (TcTyCon tyCon [])
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      initialKind <-
        case maybeInfo of
          Just info -> instantiateTyConKind info
          Nothing -> do
            emitError Nothing (OtherError ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
            kinds <- getKinds
            pure (foldr KFun (typeKind kinds) (replicate (tyConArity tyCon) (typeKind kinds)))
      foldM applyArgument initialKind arguments
    TcFunTy {} -> typeKind <$> getKinds
    TcForAllTy _ body -> tcTypeKind body
    TcQualTy _ _ -> typeKind <$> getKinds
    TcAppTy function argument -> tcTypeKind function >>= (`applyArgument` argument)
  where
    applyArgument functionKind argument = do
      functionKind' <- zonkKind functionKind >>= refineGivenKind
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
          kinds <- getKinds
          emitError Nothing (KindMismatch (TcFunTy (typeKind kinds) (typeKind kinds)) functionKind')
          pure (typeKind kinds)

listType :: TcType -> TcM TcType
listType ty = do
  kinds <- getKinds
  tyCon <- wiredTyCon tcWiringListTyCon (KFun (typeKind kinds) (typeKind kinds))
  pure (TcTyCon tyCon [ty])

listTypeKind :: TcType -> TcType
listTypeKind kind = KFun kind kind

runtimeRepOrLifted :: TcKinds -> TcType -> TcType
runtimeRepOrLifted kinds kind =
  case runtimeRepFromKind kind of
    Right runtimeRep -> runtimeRep
    Left _ -> liftedRep kinds

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

-- | Peel the @forall@ binders and context of a signature, continuing
-- through a @forall@ that follows the context, so @C a => forall s. body@
-- yields the same scheme as @forall s. C a => body@. A second context
-- stays in the body: a pattern synonym signature reads
-- @required => provided => body@.
splitSigma :: Type -> ([TyVarBinder], [Type], Type)
splitSigma ty =
  let (binders, qualifiedBody) = splitForalls ty
      (context, body) = splitContext qualifiedBody
      (innerBinders, innerBody) = splitForalls body
   in (binders <> innerBinders, context, innerBody)

splitForalls :: Type -> ([TyVarBinder], Type)
splitForalls ty =
  case ty of
    TAnn _ inner -> splitForalls inner
    TParen inner -> splitForalls inner
    TForall telescope inner ->
      let (binders, body) = splitForalls inner
       in (forallTelescopeBinders telescope <> binders, body)
    _ -> ([], ty)

-- | The predicates of one context item. The empty constraint @()@ gives
-- no predicate. A quantified constraint with the conclusion @()@ is always
-- true, so it also gives no predicate.
surfacePredToPreds :: TvKindEnv -> Type -> TcM [Pred]
surfacePredToPreds tvEnv ty = do
  let (binders, qualifiedBody) = splitForalls (peelTypeHead ty)
      (antecedentTypes, consequentType) = splitContext (peelTypeHead qualifiedBody)
  if null binders && null antecedentTypes
    then surfaceAtomicPredToPred tvEnv consequentType
    else do
      params <- makeParamEnvWith tvEnv binders
      let quantifiedEnv =
            tvEnv
              <> Map.fromList
                [ (paramName param, (paramTyVar param, paramKind param))
                | param <- params
                ]
      antecedents <- surfaceContextToPreds quantifiedEnv antecedentTypes
      consequents <- concat <$> mapM (surfaceAtomicPredToPred quantifiedEnv) (flattenSurfaceContext [consequentType])
      pure [QuantifiedPred (map paramTyVar params) antecedents consequent | consequent <- consequents]

surfaceAtomicPredToPred :: TvKindEnv -> Type -> TcM [Pred]
surfaceAtomicPredToPred tvEnv ty =
  case peelTypeHead ty of
    TImplicitParam name payload -> do
      kinds <- getKinds
      payloadType <- checkSurfaceType tvEnv payload (typeKind kinds)
      pure [IParamPred name payloadType]
    _ -> surfaceClassPredToPred tvEnv ty

surfaceClassPredToPred :: TvKindEnv -> Type -> TcM [Pred]
surfaceClassPredToPred tvEnv ty = do
  kinds <- getKinds
  case instanceHeadName (peelTypeHead ty) of
    Just className -> do
      let classNameText = nameText className
          headArgs = instanceHeadTypes (peelTypeHead ty)
      maybeClassInfo <- lookupResolvedTyCon className
      case maybeClassInfo of
        Just classInfo
          | Just {} <- tciTypeSynonym classInfo -> do
              -- A constraint synonym expands to one constraint. The
              -- expansion is rebuilt from a type, which does not know
              -- whether its head is a class or a family, so a
              -- family-headed one is reclassified as irreducible here.
              (expanded, _) <- convertSurfaceTypeWithKinds tvEnv ty
              isUnit <- isUnitConstraintType expanded
              case constraintTypeToPred kinds expanded of
                _ | isUnit -> pure []
                Just predicate -> pure <$> normalizeFamilyPred predicate
                Nothing -> do
                  emitError Nothing (OtherError ("constraint synonym does not expand to one constraint: " <> T.unpack classNameText))
                  abortTc "invalid constraint synonym expansion"
        Just classInfo
          | isEqualityTyCon kinds (tciTyCon classInfo),
            [left, right] <- headArgs -> do
              (leftType, leftKind) <- convertSurfaceTypeWithKinds tvEnv left
              (rightType, rightKind) <- convertSurfaceTypeWithKinds tvEnv right
              when (tciTyCon classInfo == kindsEqualityTyCon kinds) (unifyKinds leftKind rightKind)
              pure [EqPred leftType rightType]
        Just classInfo
          | tciFlavor classInfo == TypeFamilyTyCon -> do
              -- A constraint whose head is a type family names no class yet.
              -- It is kept whole and reclassified once the family reduces.
              constraint <- checkSurfaceType tvEnv ty (constraintKind kinds)
              pure [IrredPred constraint]
        Just classInfo -> do
          classKind <- predicateClassKind classInfo
          let argKinds = takeClassArgKinds kinds (length headArgs) classKind
          args <- zipWithM (checkSurfaceType tvEnv) headArgs argKinds
          pure [ClassPred (tciTyCon classInfo) args]
        Nothing -> do
          emitError Nothing (OtherError ("unknown class predicate: " <> T.unpack classNameText))
          abortTc ("missing checked type constructor for class predicate " <> T.unpack classNameText)
    Nothing -> do
      emitError Nothing (OtherError ("invalid class predicate: " <> show ty))
      abortTc "invalid checked class predicate"

classPredicateArgKinds :: Name -> Int -> TcM [TcType]
classPredicateArgKinds className argCount = do
  mInfo <- lookupResolvedTyCon className
  case mInfo of
    Just info -> do
      kinds <- getKinds
      takeClassArgKinds kinds argCount <$> predicateClassKind info
    Nothing -> mapM (const freshKindMeta) [1 .. argCount]

predicateClassKind :: TyConInfo -> TcM TcType
predicateClassKind info
  | tciName info == "Lift" = do
      kinds <- getKinds
      representation <- freshMetaTvOfKind (runtimeRepKind kinds)
      pure (KFun (mkTYPEKind kinds representation) (constraintKind kinds))
predicateClassKind info = do
  kind <- instantiateTyConKind info
  zonkKind kind

takeClassArgKinds :: TcKinds -> Int -> TcType -> [TcType]
takeClassArgKinds kinds n kind
  | n <= 0 = []
  | otherwise =
      case kind of
        KFun arg rest -> arg : takeClassArgKinds kinds (n - 1) rest
        _ -> replicate n (typeKind kinds)
