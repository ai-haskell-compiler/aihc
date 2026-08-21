{-# LANGUAGE OverloadedStrings #-}

-- | Convert checked kinds and types into System FC 2 types.
module Aihc.Fc2.Convert
  ( ConvertEnv (..),
    emptyConvertEnv,
    withTyVar,
    withTyVars,
    withClassTyCons,
    withAxioms,
    convertKind,
    convertRep,
    convertType,
    convertPred,
    tyVarBinder,
    tyConNameFc2,
    classDictTypeName,
    classDictConName,
    lookupAxiomName,
    funType,
    liftedRepType,
    typeRep,
    extraKindVars,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.Wired
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types
  ( Kind (..),
    Levity (..),
    Pred (..),
    RuntimeRep (..),
    TcType (..),
    TyCon,
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    kindFromTypeScheme,
    liftedRuntimeRep,
    runtimeRepOfType,
    tvKind,
    tyConKey,
    tyConKindScheme,
    tyConModuleName,
    tyConName,
    tyConPackageId,
    typeKind,
  )
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data ConvertEnv = ConvertEnv
  { cePrimPackage :: PackageId,
    ceTyVars :: Map Unique TyVarId,
    ceClassTyCons :: Set (PackageId, Text, Text),
    ceAxioms :: Map Text Name
  }

emptyConvertEnv :: PackageId -> ConvertEnv
emptyConvertEnv package =
  ConvertEnv
    { cePrimPackage = package,
      ceTyVars = Map.empty,
      ceClassTyCons = Set.empty,
      ceAxioms = Map.empty
    }

withClassTyCons :: [(PackageId, Text, Text)] -> ConvertEnv -> ConvertEnv
withClassTyCons keys env =
  env {ceClassTyCons = Set.fromList keys <> ceClassTyCons env}

withAxioms :: [(Text, Name)] -> ConvertEnv -> ConvertEnv
withAxioms axioms env =
  env {ceAxioms = Map.fromList axioms <> ceAxioms env}

classDictTypeName :: TyCon -> Name
classDictTypeName tyCon =
  Name ("$Dict$" <> tyConName tyCon) SortTypeConstructor (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

classDictConName :: TyCon -> Name
classDictConName tyCon =
  Name ("$Dict$" <> tyConName tyCon) SortDataConstructor (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

lookupAxiomName :: ConvertEnv -> Text -> Name
lookupAxiomName env name =
  Map.findWithDefault (Name name SortAxiom (OriginLocal (Unique 0))) name (ceAxioms env)

withTyVar :: TyVarId -> ConvertEnv -> ConvertEnv
withTyVar tyVar env =
  env {ceTyVars = Map.insert (tvUnique tyVar) tyVar (ceTyVars env)}

withTyVars :: [TyVarId] -> ConvertEnv -> ConvertEnv
withTyVars tyVars env = foldr withTyVar env tyVars

convertKind :: ConvertEnv -> Kind -> Either String Type
convertKind env kind =
  case kind of
    KTYPE runtimeRep
      | runtimeRep == liftedRuntimeRep -> Right (typeSynonym (cePrimPackage env))
      | otherwise -> TyApp (TyCon (typeConstructor (cePrimPackage env))) <$> convertRep env runtimeRep
    KConstraint -> Right (TyCon (constraintName (cePrimPackage env)))
    KRuntimeRep -> Right (TyCon (runtimeRepConstructor (cePrimPackage env)))
    KLevity -> Right (TyCon (levityConstructor (cePrimPackage env)))
    KVecCount -> Right (TyCon (wiredGhcTypes (cePrimPackage env) "VecCount" SortTypeConstructor))
    KVecElem -> Right (TyCon (wiredGhcTypes (cePrimPackage env) "VecElem" SortTypeConstructor))
    KFun argument result ->
      funType env <$> convertKind env argument <*> convertKind env result
    KMeta {} -> Left "kind still has a meta variable"

convertRep :: ConvertEnv -> RuntimeRep -> Either String Type
convertRep env runtimeRep =
  case runtimeRep of
    BoxedRep Lifted -> Right (TyCon (liftedRepName (cePrimPackage env)))
    BoxedRep Unlifted -> Right (TyCon (unliftedRepName (cePrimPackage env)))
    IntRep -> Right (repCon env "IntRep")
    Int8Rep -> Right (repCon env "Int8Rep")
    Int16Rep -> Right (repCon env "Int16Rep")
    Int32Rep -> Right (repCon env "Int32Rep")
    Int64Rep -> Right (repCon env "Int64Rep")
    WordRep -> Right (repCon env "WordRep")
    Word8Rep -> Right (repCon env "Word8Rep")
    Word16Rep -> Right (repCon env "Word16Rep")
    Word32Rep -> Right (repCon env "Word32Rep")
    Word64Rep -> Right (repCon env "Word64Rep")
    AddrRep -> Right (repCon env "AddrRep")
    FloatRep -> Right (repCon env "FloatRep")
    DoubleRep -> Right (repCon env "DoubleRep")
    TupleRep fields -> do
      converted <- mapM (convertRep env) fields
      Right (TyApp (repCon env "TupleRep") (promotedRuntimeRepList env converted))
    SumRep fields -> do
      converted <- mapM (convertRep env) fields
      Right (TyApp (repCon env "SumRep") (promotedRuntimeRepList env converted))
    VecRep count element ->
      Right
        ( TyApp
            (TyApp (repCon env "VecRep") (repCon env (T.pack (show count))))
            (repCon env (T.pack (show element)))
        )
    RuntimeRepVar unique ->
      case Map.lookup unique (ceTyVars env) of
        Just tyVar -> Right (tyVarType tyVar)
        Nothing ->
          Right
            ( TyVar
                ( Name
                    ("rep" <> T.pack (show uniqueValue))
                    SortTypeVariable
                    (OriginLocal unique)
                )
            )
      where
        Unique uniqueValue = unique
    RuntimeRepMeta {} -> Left "runtime representation still has a meta variable"

promotedRuntimeRepList :: ConvertEnv -> [Type] -> Type
promotedRuntimeRepList env =
  foldr cons nil
  where
    runtimeRep = TyCon (runtimeRepConstructor (cePrimPackage env))
    nil = TyApp (repCon env "[]") runtimeRep
    cons item = TyApp (TyApp (TyApp (repCon env ":") runtimeRep) item)

repCon :: ConvertEnv -> Text -> Type
repCon env name = TyCon (wiredGhcTypes (cePrimPackage env) name SortDataConstructor)

convertType :: ConvertEnv -> TcType -> Either String Type
convertType env ty =
  case ty of
    TcTyVar tyVar -> Right (tyVarType tyVar)
    TcMetaTv {} -> Left "type still has a meta variable"
    TcTyCon tyCon arguments -> do
      kindArgs <- invisibleKindArgs env tyCon arguments
      converted <- mapM (convertType env) arguments
      pure (foldl TyApp (TyCon (tyConNameFc2 env tyCon)) (kindArgs <> converted))
    TcFunTy argument result -> do
      convertedArgument <- convertType env argument
      convertedResult <- convertType env result
      r1 <- typeRep env argument
      r2 <- typeRep env result
      pure (TyFun r1 r2 convertedArgument convertedResult)
    TcForAllTy tyVar body -> do
      binder <- tyVarBinder env tyVar
      convertedBody <- convertType (withTyVar tyVar env) body
      pure (TyForAll binder convertedBody)
    TcQualTy predicates body -> do
      convertedPredicates <- mapM (convertPred env) predicates
      convertedBody <- convertType env body
      pure (foldr (funType env) convertedBody convertedPredicates)
    TcAppTy function argument ->
      TyApp <$> convertType env function <*> convertType env argument
    TcBuiltinTyCon name _ arguments -> do
      headType <- builtinType env name
      converted <- mapM (convertType env) arguments
      case (bareBuiltin name, converted) of
        ("[]", items) -> Right (promotedRuntimeRepList env items)
        _ -> pure (foldl TyApp headType converted)

convertPred :: ConvertEnv -> Pred -> Either String Type
convertPred env predicate =
  case predicate of
    ClassPred tyCon arguments -> do
      converted <- mapM (convertType env) arguments
      pure (foldl TyApp (TyCon (classDictTypeName tyCon)) converted)
    EqPred left right ->
      TyEq <$> convertType env left <*> convertType env right

typeRep :: ConvertEnv -> TcType -> Either String Type
typeRep env ty =
  case runtimeRepOfType ty of
    Left message -> Left message
    Right runtimeRep -> convertRep env runtimeRep

funType :: ConvertEnv -> Type -> Type -> Type
funType env = TyFun (liftedRepType env) (liftedRepType env)

liftedRepType :: ConvertEnv -> Type
liftedRepType env = TyCon (liftedRepName (cePrimPackage env))

tyVarBinder :: ConvertEnv -> TyVarId -> Either String Binder
tyVarBinder env tyVar = do
  kind <- convertKind (withTyVar tyVar env) (tvKind tyVar)
  pure (Binder (tyVarName tyVar) kind)

tyVarName :: TyVarId -> Name
tyVarName tyVar =
  Name (tvName tyVar) SortTypeVariable (OriginLocal (tvUnique tyVar))

tyVarType :: TyVarId -> Type
tyVarType tyVar = TyVar (tyVarName tyVar)

tyConNameFc2 :: ConvertEnv -> TyCon -> Name
tyConNameFc2 env tyCon =
  if Set.member (tyConKey tyCon) (ceClassTyCons env)
    then classDictTypeName tyCon
    else case promotedNameFc2 tyCon of
      Just name -> name
      Nothing -> Name (tyConName tyCon) SortTypeConstructor (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon))

-- | Convert the type-checker promotion marker to an FC data-constructor name.
promotedNameFc2 :: TyCon -> Maybe Name
promotedNameFc2 tyCon =
  if T.isPrefixOf "'" (tyConName tyCon)
    then wiredBuiltinName tyCon
    else Nothing

wiredBuiltinName :: TyCon -> Maybe Name
wiredBuiltinName tyCon =
  case Map.lookup (bareBuiltin (tyConName tyCon)) builtinTable of
    Just (sort, _) ->
      Just (Name (bareBuiltin (tyConName tyCon)) sort (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)))
    Nothing -> Nothing

-- | Invisible kind parameters that the type constructor quantifies before visible arguments.
extraKindVars :: TyCon -> [TyVarId] -> [TyVarId]
extraKindVars tyCon visible =
  case tyConKindScheme tyCon of
    ForAll vars _ _ ->
      let seen = map tvUnique visible
       in filter (\tyVar -> tvUnique tyVar `notElem` seen) vars

invisibleKindArgs :: ConvertEnv -> TyCon -> [TcType] -> Either String [Type]
invisibleKindArgs env tyCon arguments =
  mapM (kindVarToType env tyCon arguments) (extraKindVars tyCon [])

kindVarToType :: ConvertEnv -> TyCon -> [TcType] -> TyVarId -> Either String Type
kindVarToType env tyCon arguments tyVar =
  case Map.lookup (tvUnique tyVar) (ceTyVars env) of
    Just found -> Right (tyVarType found)
    Nothing ->
      case Map.lookup (tvUnique tyVar) (repSubst tyCon arguments) of
        Just runtimeRep -> convertRep env runtimeRep
        Nothing -> convertRep env (RuntimeRepVar (tvUnique tyVar))

repSubst :: TyCon -> [TcType] -> Map Unique RuntimeRep
repSubst tyCon =
  go (kindFromTypeScheme (tyConKindScheme tyCon))
  where
    go (KFun formal result) (argument : rest) =
      matchKind formal (typeKind argument) <> go result rest
    go _ _ = Map.empty

    matchKind (KTYPE (RuntimeRepVar unique)) (KTYPE runtimeRep) =
      Map.singleton unique runtimeRep
    matchKind (KFun left right) (KFun left' right') =
      matchKind left left' <> matchKind right right'
    matchKind _ _ = Map.empty

builtinType :: ConvertEnv -> Text -> Either String Type
builtinType env name =
  case Map.lookup (bareBuiltin name) builtinTable of
    Nothing -> Left ("unknown builtin type " <> T.unpack name)
    Just (sort, moduleName) ->
      Right (TyCon (Name (bareBuiltin name) sort (OriginTop (cePrimPackage env) moduleName)))

bareBuiltin :: Text -> Text
bareBuiltin = T.dropWhile (== '\'')

builtinTable :: Map Text (Sort, Text)
builtinTable =
  Map.fromList
    [ ("Type", (SortSynonym, "GHC.Types")),
      ("TYPE", (SortTypeConstructor, "GHC.Types")),
      ("Constraint", (SortTypeConstructor, "GHC.Types")),
      ("RuntimeRep", (SortTypeConstructor, "GHC.Types")),
      ("Levity", (SortTypeConstructor, "GHC.Types")),
      ("VecCount", (SortTypeConstructor, "GHC.Types")),
      ("VecElem", (SortTypeConstructor, "GHC.Types")),
      ("LiftedRep", (SortSynonym, "GHC.Types")),
      ("UnliftedRep", (SortSynonym, "GHC.Types")),
      ("IntRep", (SortDataConstructor, "GHC.Types")),
      ("Int8Rep", (SortDataConstructor, "GHC.Types")),
      ("Int16Rep", (SortDataConstructor, "GHC.Types")),
      ("Int32Rep", (SortDataConstructor, "GHC.Types")),
      ("Int64Rep", (SortDataConstructor, "GHC.Types")),
      ("WordRep", (SortDataConstructor, "GHC.Types")),
      ("Word8Rep", (SortDataConstructor, "GHC.Types")),
      ("Word16Rep", (SortDataConstructor, "GHC.Types")),
      ("Word32Rep", (SortDataConstructor, "GHC.Types")),
      ("Word64Rep", (SortDataConstructor, "GHC.Types")),
      ("AddrRep", (SortDataConstructor, "GHC.Types")),
      ("FloatRep", (SortDataConstructor, "GHC.Types")),
      ("DoubleRep", (SortDataConstructor, "GHC.Types")),
      ("BoxedRep", (SortDataConstructor, "GHC.Types")),
      ("Lifted", (SortDataConstructor, "GHC.Types")),
      ("Unlifted", (SortDataConstructor, "GHC.Types")),
      ("TupleRep", (SortDataConstructor, "GHC.Types")),
      ("SumRep", (SortDataConstructor, "GHC.Types")),
      ("VecRep", (SortDataConstructor, "GHC.Types")),
      ("[]", (SortDataConstructor, "GHC.Types")),
      (":", (SortDataConstructor, "GHC.Types")),
      ("Vec2", (SortDataConstructor, "GHC.Types")),
      ("Vec4", (SortDataConstructor, "GHC.Types")),
      ("Vec8", (SortDataConstructor, "GHC.Types")),
      ("Vec16", (SortDataConstructor, "GHC.Types")),
      ("Vec32", (SortDataConstructor, "GHC.Types")),
      ("Vec64", (SortDataConstructor, "GHC.Types")),
      ("Int8ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Int16ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Int32ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Int64ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Word8ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Word16ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Word32ElemRep", (SortDataConstructor, "GHC.Types")),
      ("Word64ElemRep", (SortDataConstructor, "GHC.Types")),
      ("FloatElemRep", (SortDataConstructor, "GHC.Types")),
      ("DoubleElemRep", (SortDataConstructor, "GHC.Types"))
    ]
