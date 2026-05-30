{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Aihc.Tc.Kind
  ( TvKindEnv,
    ParamInfo (..),
    checkSurfaceType,
    convertSurfaceType,
    convertSurfaceTypeWithKinds,
    defaultKindMetas,
    freeTypeVars,
    freshKindMeta,
    kindToTcType,
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
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

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
  tvs <- mapM freshSkolemTv freeVars
  kinds <- mapM (const freshKindMeta) freeVars
  let tvEnv = Map.fromList (zip freeVars (zip tvs kinds))
  tcTy <- checkSurfaceType tvEnv body KType
  preds <- mapM (surfacePredToPred tvEnv) context
  pure (ForAll tvs preds tcTy)

convertSurfaceType :: Map Text TyVarId -> Type -> TcM TcType
convertSurfaceType tvMap ty = do
  let tvEnv = Map.map (,KType) tvMap
  checkSurfaceType tvEnv ty KType

checkSurfaceType :: TvKindEnv -> Type -> Kind -> TcM TcType
checkSurfaceType tvEnv ty expected = do
  (tcTy, actual) <- convertSurfaceTypeWithKinds tvEnv ty
  unifyKinds expected actual
  pure tcTy

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
      aTy <- checkSurfaceType tvEnv a KType
      bTy <- checkSurfaceType tvEnv b KType
      pure (TcFunTy aTy bTy, KType)
    TTuple flavor _ args -> do
      tys <- mapM (\arg -> checkSurfaceType tvEnv arg KType) args
      let arity = length tys
      pure (TcTyCon (TyCon (tupleConText flavor arity) arity) tys, KType)
    TUnboxedSum args -> do
      tys <- mapM (\arg -> checkSurfaceType tvEnv arg KType) args
      let arity = length tys
      pure (TcTyCon (TyCon ("(#" <> bars (arity - 1) <> "#)") arity) tys, KType)
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
    Promoted -> inferOpenTypeConstructor ("'" <> nameText name)
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
  case name of
    "Int" -> pure (TcTyCon (TyCon "Int" 0) [], KType)
    "Integer" -> pure (TcTyCon (TyCon "Integer" 0) [], KType)
    "Double" -> pure (TcTyCon (TyCon "Double" 0) [], KType)
    "Float" -> pure (TcTyCon (TyCon "Float" 0) [], KType)
    "Char" -> pure (TcTyCon (TyCon "Char" 0) [], KType)
    "Bool" -> pure (TcTyCon (TyCon "Bool" 0) [], KType)
    _ -> inferOpenTypeConstructor name

inferOpenTypeConstructor :: Text -> TcM (TcType, Kind)
inferOpenTypeConstructor name = do
  kind <- freshKindMeta
  pure (TcTyCon (TyCon name 0) [], kind)

makeParamEnv :: [TyVarBinder] -> TcM [ParamInfo]
makeParamEnv =
  mapM makeParam
  where
    makeParam binder = do
      tv <- freshSkolemTv (tyVarBinderName binder)
      kind <- maybe freshKindMeta (kindFromSurfaceType Map.empty) (tyVarBinderKind binder)
      pure
        ParamInfo
          { paramName = tyVarBinderName binder,
            paramTyVar = tv,
            paramKind = kind
          }

tyConKindFromParams :: [ParamInfo] -> Maybe Type -> TcM Kind
tyConKindFromParams params maybeResultKind = do
  resultKind <- maybe (pure KType) (kindFromSurfaceType Map.empty) maybeResultKind
  pure (foldr (KFun . paramKind) resultKind params)

kindFromSurfaceType :: TvKindEnv -> Type -> TcM Kind
kindFromSurfaceType tvEnv ty =
  case peelTypeHead ty of
    TStar {} -> pure KType
    TCon name Unpromoted
      | nameText name == "Type" -> pure KType
      | nameText name == "Constraint" -> pure KConstraint
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
    (KType, KType) -> pure ()
    (KConstraint, KConstraint) -> pure ()
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
    KType -> pure KType
    KConstraint -> pure KConstraint

defaultKindMetas :: Kind -> TcM Kind
defaultKindMetas kind =
  case kind of
    KMeta u -> do
      mKind <- readKindMeta u
      case mKind of
        Nothing -> writeKindMeta u KType >> pure KType
        Just solved -> defaultKindMetas solved
    KFun a b -> KFun <$> defaultKindMetas a <*> defaultKindMetas b
    KType -> pure KType
    KConstraint -> pure KConstraint

freshKindMeta :: TcM Kind
freshKindMeta = KMeta <$> freshUnique

occursInKind :: Unique -> Kind -> Bool
occursInKind needle kind =
  case kind of
    KMeta u -> u == needle
    KFun a b -> occursInKind needle a || occursInKind needle b
    KType -> False
    KConstraint -> False

kindToTcType :: Kind -> TcType
kindToTcType kind =
  case kind of
    KType -> TcTyCon (TyCon "*" 0) []
    KConstraint -> TcTyCon (TyCon "Constraint" 0) []
    KMeta u -> TcMetaTv u
    KFun a b -> TcFunTy (kindToTcType a) (kindToTcType b)

applyType :: TcType -> TcType -> TcType
applyType (TcTyCon tc args) arg = TcTyCon tc (args ++ [arg])
applyType f arg = TcAppTy f arg

listType :: TcType -> TcType
listType ty = TcTyCon (TyCon "[]" 1) [ty]

listTypeKind :: Kind -> Kind
listTypeKind kind = KFun kind kind

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
      args <- mapM (\arg -> checkSurfaceType tvEnv arg KType) (instanceHeadTypes ty)
      pure (ClassPred (nameText className) args)
    Nothing -> do
      emitError NoSourceSpan (OtherError ("invalid class predicate: " <> show ty))
      pure (ClassPred "<invalid-predicate>" [])

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
