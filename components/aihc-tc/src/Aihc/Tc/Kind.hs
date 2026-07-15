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
    kindToTcType,
    classPredicateArgKinds,
    makeParamEnv,
    sigToScheme,
    surfacePredToPred,
    tyConKindFromParams,
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
import Aihc.Tc.Env (TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Control.Monad (zipWithM)
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

type TvKindEnv = Map Text (TyVarId, Kind)

data ParamInfo = ParamInfo
  { paramName :: !Text,
    paramTyVar :: !TyVarId,
    paramKind :: !Kind
  }
  deriving (Show)

sigToScheme :: Type -> TcM TypeScheme
sigToScheme ty = do
  let (context, body) = splitContext ty
      freeVars = freeTypeVars ty
  rawTvs <- mapM freshSkolemTv freeVars
  kinds <- mapM (const freshKindMeta) freeVars
  let tvs = zipWith setTyVarKind kinds rawTvs
  let tvEnv = Map.fromList (zip freeVars (zip tvs kinds))
  tcTy <- checkRuntimeType tvEnv body
  preds <- mapM (surfacePredToPred tvEnv) context
  pure (ForAll tvs preds tcTy)

convertSurfaceType :: Map Text TyVarId -> Type -> TcM TcType
convertSurfaceType tvMap ty = do
  let tvEnv = Map.map (\tv -> (tv, tvKind tv)) tvMap
  checkRuntimeType tvEnv ty

checkSurfaceType :: TvKindEnv -> Type -> Kind -> TcM TcType
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

convertSurfaceTypeWithKinds :: TvKindEnv -> Type -> TcM (TcType, Kind)
convertSurfaceTypeWithKinds tvEnv ty =
  case peelTypeHead ty of
    TAnn _ inner ->
      convertSurfaceTypeWithKinds tvEnv inner
    TVar name ->
      inferTypeVariable tvEnv name
    TCon name promoted ->
      inferTypeConstructor promoted name
    TBuiltinCon builtin ->
      inferBuiltinTypeConstructor builtin
    TStar {} ->
      pure (TcTyCon (TyCon "*" 0) [], KType)
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
      let arity = length tys
          resultKind =
            case flavor of
              Boxed -> KType
              Unboxed -> KTYPE (TupleRep (map runtimeRepOrLifted tys))
          tyConKind' = foldr (KFun . typeKind) resultKind tys
      pure (TcTyCon (mkTyCon (tupleConText flavor arity) arity tyConKind') tys, resultKind)
    TUnboxedSum args -> do
      tys <- mapM (checkRuntimeType tvEnv) args
      let arity = length tys
          resultKind = KTYPE (SumRep (map runtimeRepOrLifted tys))
          tyConKind' = foldr (KFun . typeKind) resultKind tys
      pure (TcTyCon (mkTyCon ("(#" <> bars (arity - 1) <> "#)") arity tyConKind') tys, resultKind)
    TList Unpromoted [arg] -> do
      argTy <- checkSurfaceType tvEnv arg KType
      pure (listType argTy, KType)
    TList Promoted args -> do
      elemKind <- freshKindMeta
      args' <- mapM (\arg -> checkSurfaceType tvEnv arg elemKind) args
      pure (TcTyCon (TyCon "'[]" (length args')) args', KType)
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

inferTypeVariable :: TvKindEnv -> UnqualifiedName -> TcM (TcType, Kind)
inferTypeVariable tvEnv name =
  let n = unqualifiedNameText name
   in case Map.lookup n tvEnv of
        Just (tv, kind) -> pure (TcTyVar tv, kind)
        Nothing -> inferOpenTypeConstructor n

inferTypeConstructor :: TypePromotion -> Name -> TcM (TcType, Kind)
inferTypeConstructor promoted name =
  case promoted of
    Promoted -> inferPromotedTypeConstructor (nameText name)
    Unpromoted ->
      case nameText name of
        "String" -> pure (listType (TcTyCon (TyCon "Char" 0) []), KType)
        "Type" -> pure (TcTyCon (TyCon "Type" 0) [], KType)
        "Constraint" -> pure (TcTyCon (TyCon "Constraint" 0) [], KType)
        raw -> do
          mInfo <- lookupTyCon raw
          case mInfo of
            Just info -> pure (TcTyCon (tciTyCon info) [], tciKind info)
            Nothing -> inferBuiltinOrOpenTypeConstructor raw

inferBuiltinTypeConstructor :: TypeBuiltinCon -> TcM (TcType, Kind)
inferBuiltinTypeConstructor builtin =
  case builtin of
    TBuiltinList ->
      pure (TcTyCon (TyCon "[]" 1) [], KFun KType KType)
    TBuiltinCons ->
      pure (TcTyCon (TyCon ":" 2) [], KFun KType (KFun (listTypeKind KType) (listTypeKind KType)))
    TBuiltinTuple arity ->
      let argKinds = replicate arity KType
       in pure (TcTyCon (TyCon (tupleConText Boxed arity) arity) [], foldr KFun KType argKinds)
    TBuiltinArrow ->
      pure (TcTyCon (TyCon "(->)" 2) [], KFun KType (KFun KType KType))

inferBuiltinOrOpenTypeConstructor :: Text -> TcM (TcType, Kind)
inferBuiltinOrOpenTypeConstructor name =
  case wiredInTypeKind name of
    Just kind -> pure (TcTyCon (mkTyCon name 0 kind) [], kind)
    Nothing -> inferOpenTypeConstructor name

inferPromotedTypeConstructor :: Text -> TcM (TcType, Kind)
inferPromotedTypeConstructor name =
  case runtimeRepConstructor name of
    Just _ -> pure (TcTyCon (mkTyCon ("'" <> name) 0 KRuntimeRep) [], KRuntimeRep)
    Nothing -> inferOpenTypeConstructor ("'" <> name)

inferOpenTypeConstructor :: Text -> TcM (TcType, Kind)
inferOpenTypeConstructor name = do
  kind <- freshKindMeta
  pure (TcTyCon (mkTyCon name 0 kind) [], kind)

makeParamEnv :: [TyVarBinder] -> TcM [ParamInfo]
makeParamEnv = go Map.empty
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

tyConKindFromParams :: [ParamInfo] -> Maybe Type -> TcM Kind
tyConKindFromParams params maybeResultKind = do
  let tvEnv = Map.fromList [(paramName param, (paramTyVar param, paramKind param)) | param <- params]
  resultKind <- maybe (pure KType) (kindFromSurfaceType tvEnv) maybeResultKind
  pure (foldr (KFun . paramKind) resultKind params)

kindFromSurfaceType :: TvKindEnv -> Type -> TcM Kind
kindFromSurfaceType tvEnv ty =
  case peelTypeHead ty of
    TStar {} -> pure KType
    TCon name Unpromoted
      | nameText name == "Type" -> pure KType
      | nameText name == "Constraint" -> pure KConstraint
      | nameText name == "RuntimeRep" -> pure KRuntimeRep
      | nameText name == "Levity" -> pure KLevity
      | nameText name == "VecCount" -> pure KVecCount
      | nameText name == "VecElem" -> pure KVecElem
    TApp function repTy
      | TCon name Unpromoted <- peelTypeHead function,
        nameText name == "TYPE" ->
          KTYPE <$> runtimeRepFromSurfaceType tvEnv repTy
    TFun _ a b -> KFun <$> kindFromSurfaceType tvEnv a <*> kindFromSurfaceType tvEnv b
    TParen inner -> kindFromSurfaceType tvEnv inner
    TAnn _ inner -> kindFromSurfaceType tvEnv inner
    other -> do
      (_tcTy, kind) <- convertSurfaceTypeWithKinds tvEnv other
      unifyKinds kind KType
      pure KType

unifyKinds :: Kind -> Kind -> TcM ()
unifyKinds expected actual = do
  expected' <- zonkKind expected
  actual' <- zonkKind actual
  case (expected', actual') of
    (KMeta u, kind) -> bindKindMeta u kind
    (kind, KMeta u) -> bindKindMeta u kind
    (KTYPE expectedRep, KTYPE actualRep)
      | expectedRep == actualRep -> pure ()
    (KConstraint, KConstraint) -> pure ()
    (KRuntimeRep, KRuntimeRep) -> pure ()
    (KLevity, KLevity) -> pure ()
    (KVecCount, KVecCount) -> pure ()
    (KVecElem, KVecElem) -> pure ()
    (KFun a1 b1, KFun a2 b2) -> unifyKinds a1 a2 >> unifyKinds b1 b2
    _ -> emitError NoSourceSpan (KindMismatch expected' actual')

bindKindMeta :: Unique -> Kind -> TcM ()
bindKindMeta u kind
  | kind == KMeta u = pure ()
  | occursInKind u kind = emitError NoSourceSpan (KindMismatch (KMeta u) kind)
  | otherwise = writeKindMeta u kind

zonkKind :: Kind -> TcM Kind
zonkKind kind =
  case kind of
    KMeta u -> do
      mKind <- readKindMeta u
      case mKind of
        Nothing -> pure kind
        Just solved -> zonkKind solved
    KFun a b -> KFun <$> zonkKind a <*> zonkKind b
    KTYPE runtimeRep -> pure (KTYPE runtimeRep)
    KConstraint -> pure KConstraint
    KRuntimeRep -> pure KRuntimeRep
    KLevity -> pure KLevity
    KVecCount -> pure KVecCount
    KVecElem -> pure KVecElem

defaultKindMetas :: Kind -> TcM Kind
defaultKindMetas kind =
  case kind of
    KMeta u -> do
      mKind <- readKindMeta u
      case mKind of
        Nothing -> writeKindMeta u KType >> pure KType
        Just solved -> defaultKindMetas solved
    KFun a b -> KFun <$> defaultKindMetas a <*> defaultKindMetas b
    KTYPE runtimeRep -> pure (KTYPE runtimeRep)
    KConstraint -> pure KConstraint
    KRuntimeRep -> pure KRuntimeRep
    KLevity -> pure KLevity
    KVecCount -> pure KVecCount
    KVecElem -> pure KVecElem

freshKindMeta :: TcM Kind
freshKindMeta = KMeta <$> freshUnique

occursInKind :: Unique -> Kind -> Bool
occursInKind needle kind =
  case kind of
    KMeta u -> u == needle
    KFun a b -> occursInKind needle a || occursInKind needle b
    KTYPE runtimeRep -> occursInRuntimeRep needle runtimeRep
    KConstraint -> False
    KRuntimeRep -> False
    KLevity -> False
    KVecCount -> False
    KVecElem -> False

kindToTcType :: Kind -> TcType
kindToTcType kind =
  case kind of
    KTYPE runtimeRep
      | runtimeRep == liftedRuntimeRep -> TcTyCon (TyCon "*" 0) []
      | otherwise -> TcTyCon (TyCon "TYPE" 1) [runtimeRepToTcType runtimeRep]
    KConstraint -> TcTyCon (TyCon "Constraint" 0) []
    KRuntimeRep -> TcTyCon (TyCon "RuntimeRep" 0) []
    KLevity -> TcTyCon (TyCon "Levity" 0) []
    KVecCount -> TcTyCon (TyCon "VecCount" 0) []
    KVecElem -> TcTyCon (TyCon "VecElem" 0) []
    KMeta u -> TcMetaTv u
    KFun a b -> TcFunTy (kindToTcType a) (kindToTcType b)

applyType :: TcType -> TcType -> TcType
applyType (TcTyCon tc args) arg = TcTyCon tc (args ++ [arg])
applyType f arg = TcAppTy f arg

listType :: TcType -> TcType
listType ty = TcTyCon (TyCon "[]" 1) [ty]

listTypeKind :: Kind -> Kind
listTypeKind kind = KFun kind kind

runtimeRepOrLifted :: TcType -> RuntimeRep
runtimeRepOrLifted ty =
  case runtimeRepOfType ty of
    Right runtimeRep -> runtimeRep
    Left _ -> liftedRuntimeRep

wiredInTypeKind :: Text -> Maybe Kind
wiredInTypeKind name =
  case name of
    "Int" -> Just KType
    "Integer" -> Just KType
    "Double" -> Just KType
    "Float" -> Just KType
    "Char" -> Just KType
    "Bool" -> Just KType
    "Int#" -> Just (KTYPE IntRep)
    "Int8#" -> Just (KTYPE Int8Rep)
    "Int16#" -> Just (KTYPE Int16Rep)
    "Int32#" -> Just (KTYPE Int32Rep)
    "Int64#" -> Just (KTYPE Int64Rep)
    "Word#" -> Just (KTYPE WordRep)
    "Word8#" -> Just (KTYPE Word8Rep)
    "Word16#" -> Just (KTYPE Word16Rep)
    "Word32#" -> Just (KTYPE Word32Rep)
    "Word64#" -> Just (KTYPE Word64Rep)
    "Addr#" -> Just (KTYPE AddrRep)
    "Float#" -> Just (KTYPE FloatRep)
    "Double#" -> Just (KTYPE DoubleRep)
    "Char#" -> Just (KTYPE WordRep)
    _ -> Nothing

runtimeRepFromSurfaceType :: TvKindEnv -> Type -> TcM RuntimeRep
runtimeRepFromSurfaceType tvEnv ty =
  case peelTypeHead ty of
    TVar name ->
      case Map.lookup (unqualifiedNameText name) tvEnv of
        Just (tyVar, KRuntimeRep) -> pure (RuntimeRepVar (tvUnique tyVar))
        _ -> invalidRuntimeRep
    TCon name _ ->
      maybe invalidRuntimeRep pure (runtimeRepConstructor (nameText name))
    TApp function levityTy
      | TCon name _ <- peelTypeHead function,
        nameText name == "BoxedRep" ->
          BoxedRep <$> levityFromSurfaceType levityTy
    _ -> invalidRuntimeRep
  where
    invalidRuntimeRep = do
      emitError NoSourceSpan (OtherError ("invalid RuntimeRep: " <> take 80 (show ty)))
      pure liftedRuntimeRep

runtimeRepConstructor :: Text -> Maybe RuntimeRep
runtimeRepConstructor rawName =
  lookup
    (T.dropWhile (== '\'') rawName)
    [ ("LiftedRep", liftedRuntimeRep),
      ("UnliftedRep", BoxedRep Unlifted),
      ("IntRep", IntRep),
      ("Int8Rep", Int8Rep),
      ("Int16Rep", Int16Rep),
      ("Int32Rep", Int32Rep),
      ("Int64Rep", Int64Rep),
      ("WordRep", WordRep),
      ("Word8Rep", Word8Rep),
      ("Word16Rep", Word16Rep),
      ("Word32Rep", Word32Rep),
      ("Word64Rep", Word64Rep),
      ("AddrRep", AddrRep),
      ("FloatRep", FloatRep),
      ("DoubleRep", DoubleRep)
    ]

levityFromSurfaceType :: Type -> TcM Levity
levityFromSurfaceType ty =
  case peelTypeHead ty of
    TCon name _
      | T.dropWhile (== '\'') (nameText name) == "Lifted" -> pure Lifted
      | T.dropWhile (== '\'') (nameText name) == "Unlifted" -> pure Unlifted
    _ -> emitError NoSourceSpan (OtherError ("invalid Levity: " <> take 80 (show ty))) >> pure Lifted

occursInRuntimeRep :: Unique -> RuntimeRep -> Bool
occursInRuntimeRep needle runtimeRep =
  case runtimeRep of
    VecRep {} -> False
    TupleRep reps -> any (occursInRuntimeRep needle) reps
    SumRep reps -> any (occursInRuntimeRep needle) reps
    RuntimeRepVar unique -> unique == needle
    RuntimeRepMeta unique -> unique == needle
    _ -> False

runtimeRepToTcType :: RuntimeRep -> TcType
runtimeRepToTcType runtimeRep =
  case runtimeRep of
    BoxedRep Lifted -> TcTyCon (TyCon "'LiftedRep" 0) []
    BoxedRep Unlifted -> TcTyCon (TyCon "'UnliftedRep" 0) []
    IntRep -> promoted "IntRep"
    Int8Rep -> promoted "Int8Rep"
    Int16Rep -> promoted "Int16Rep"
    Int32Rep -> promoted "Int32Rep"
    Int64Rep -> promoted "Int64Rep"
    WordRep -> promoted "WordRep"
    Word8Rep -> promoted "Word8Rep"
    Word16Rep -> promoted "Word16Rep"
    Word32Rep -> promoted "Word32Rep"
    Word64Rep -> promoted "Word64Rep"
    AddrRep -> promoted "AddrRep"
    FloatRep -> promoted "FloatRep"
    DoubleRep -> promoted "DoubleRep"
    RuntimeRepVar unique ->
      TcTyVar
        ( setTyVarKind
            KRuntimeRep
            (TyVarId ("rep" <> T.pack (showUnique unique)) unique)
        )
    RuntimeRepMeta unique -> TcMetaTv unique
    TupleRep _ -> promoted "TupleRep"
    SumRep _ -> promoted "SumRep"
    VecRep {} -> promoted "VecRep"
  where
    promoted name = TcTyCon (TyCon ("'" <> name) 0) []
    showUnique (Unique value) = show value

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
      (concatMap binderKindVars (forallTelescopeBinders telescope) ++ go inner)
        \\ map tyVarBinderName (forallTelescopeBinders telescope)
    go _ = []
    binderKindVars binder = maybe [] go (tyVarBinderKind binder)

splitContext :: Type -> ([Type], Type)
splitContext (TAnn _ inner) = splitContext inner
splitContext (TContext preds inner) = (preds, inner)
splitContext ty = ([], ty)

surfacePredToPred :: TvKindEnv -> Type -> TcM Pred
surfacePredToPred tvEnv ty =
  case instanceHeadName ty of
    Just className -> do
      let classNameText = nameText className
          headArgs = instanceHeadTypes ty
      argKinds <- classPredicateArgKinds classNameText (length headArgs)
      args <- zipWithM (checkSurfaceType tvEnv) headArgs argKinds
      pure (ClassPred (nameText className) args)
    Nothing -> do
      emitError NoSourceSpan (OtherError ("invalid class predicate: " <> show ty))
      pure (ClassPred "<invalid-predicate>" [])

classPredicateArgKinds :: Text -> Int -> TcM [Kind]
classPredicateArgKinds className argCount = do
  mInfo <- lookupTyCon className
  case mInfo of
    Just info -> takeClassArgKinds argCount <$> defaultKindMetas (tciKind info)
    Nothing -> mapM (const freshKindMeta) [1 .. argCount]

takeClassArgKinds :: Int -> Kind -> [Kind]
takeClassArgKinds n kind
  | n <= 0 = []
  | otherwise =
      case kind of
        KFun arg rest -> arg : takeClassArgKinds (n - 1) rest
        _ -> replicate n KType

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor arity =
  case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = mconcat (replicate (n - 1) ",")

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = mconcat (replicate n "|")
