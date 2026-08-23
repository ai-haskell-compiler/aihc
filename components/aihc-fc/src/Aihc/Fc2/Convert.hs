{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Convert checked kinds and types into System FC 2 types.
module Aihc.Fc2.Convert
  ( ConvertEnv (..),
    emptyConvertEnv,
    withTyVar,
    withTyVars,
    withKindEnv,
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
    typeKindInEnv,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.Wired
import Aihc.Resolve (PackageId)
import Aihc.Tc.Types
  ( Pred (..),
    TcKindEnv,
    TcType (..),
    TcTypeKey,
    TyCon,
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    applySubst,
    liftedRep,
    runtimeRepFromKind,
    tvKind,
    tyConKey,
    tyConModuleName,
    tyConName,
    tyConPackageId,
    pattern AddrRep,
    pattern BoxedRep,
    pattern DoubleRep,
    pattern FloatRep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern Int8Rep,
    pattern IntRep,
    pattern KConstraint,
    pattern KFun,
    pattern KLevity,
    pattern KMeta,
    pattern KRuntimeRep,
    pattern KTYPE,
    pattern KVecCount,
    pattern KVecElem,
    pattern Lifted,
    pattern SumRep,
    pattern TupleRep,
    pattern Unlifted,
    pattern VecRep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern Word8Rep,
    pattern WordRep,
  )
import Aihc.Tc.Types qualified as Tc
import Control.Monad (zipWithM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data ConvertEnv = ConvertEnv
  { cePrimPackage :: PackageId,
    ceTyVars :: Map Unique TyVarId,
    ceKindEnv :: TcKindEnv,
    ceClassTyCons :: Set TcTypeKey,
    ceAxioms :: Map Text Name
  }

emptyConvertEnv :: PackageId -> ConvertEnv
emptyConvertEnv package =
  ConvertEnv
    { cePrimPackage = package,
      ceTyVars = Map.empty,
      ceKindEnv = Map.empty,
      ceClassTyCons = Set.empty,
      ceAxioms = Map.empty
    }

withClassTyCons :: [TcTypeKey] -> ConvertEnv -> ConvertEnv
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

withKindEnv :: TcKindEnv -> ConvertEnv -> ConvertEnv
withKindEnv kindEnv env = env {ceKindEnv = kindEnv <> ceKindEnv env}

convertKind :: ConvertEnv -> TcType -> Either String Type
convertKind env kind =
  case kind of
    KTYPE runtimeRep
      | runtimeRep == liftedRep -> Right (typeSynonym (cePrimPackage env))
      | otherwise -> TyApp (TyCon (typeConstructor (cePrimPackage env))) <$> convertRep env runtimeRep
    KConstraint -> Right (TyCon (constraintName (cePrimPackage env)))
    KRuntimeRep -> Right (TyCon (runtimeRepConstructor (cePrimPackage env)))
    KLevity -> Right (TyCon (levityConstructor (cePrimPackage env)))
    KVecCount -> Right (TyCon (wiredGhcTypes (cePrimPackage env) "VecCount" SortTypeConstructor))
    KVecElem -> Right (TyCon (wiredGhcTypes (cePrimPackage env) "VecElem" SortTypeConstructor))
    KFun argument result ->
      funType env <$> convertKind env argument <*> convertKind env result
    KMeta {} -> Left "kind still has a meta variable"
    _ -> convertType env kind

convertRep :: ConvertEnv -> TcType -> Either String Type
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
    TupleRep fields -> convertTuple fields
    SumRep fields -> do
      converted <- mapM (convertRep env) fields
      Right (TyApp (repCon env "SumRep") (promotedRuntimeRepList env converted))
    VecRep count element ->
      Right
        ( TyApp
            (TyApp (repCon env "VecRep") (repCon env (T.pack (show count))))
            (repCon env (T.pack (show element)))
        )
    TcTyVar tyVar ->
      let unique@(Unique uniqueValue) = tvUnique tyVar
       in case Map.lookup unique (ceTyVars env) of
            Just found -> Right (tyVarType found)
            Nothing -> Left ("unbound runtime-representation variable: rep" <> show uniqueValue)
    TcMetaTv {} -> Left "runtime representation still has a meta variable"
    _ -> convertType env runtimeRep
  where
    convertTuple fields = do
      converted <- mapM (convertRep env) fields
      Right (TyApp (repCon env "TupleRep") (promotedRuntimeRepList env converted))

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
convertType env = convertTypeWithExpectedKind env Nothing

convertTypeWithExpectedKind :: ConvertEnv -> Maybe TcType -> TcType -> Either String Type
convertTypeWithExpectedKind env expectedKind ty =
  case ty of
    TcTyVar tyVar -> Right (tyVarType tyVar)
    TcMetaTv {} -> Left "type still has a meta variable"
    TcTyCon tyCon arguments -> do
      kindArgs <- invisibleKindArgs env tyCon arguments expectedKind
      argumentKinds <- visibleArgumentKinds env tyCon arguments expectedKind
      converted <- zipWithM (convertTypeWithExpectedKind env) (map Just argumentKinds <> repeat Nothing) arguments
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

convertPred :: ConvertEnv -> Pred -> Either String Type
convertPred env predicate =
  case predicate of
    ClassPred tyCon arguments -> do
      converted <- mapM (convertType env) arguments
      pure (foldl TyApp (TyCon (classDictTypeName tyCon)) converted)
    EqPred left right ->
      TyEq <$> convertType env left <*> convertType env right

typeRep :: ConvertEnv -> TcType -> Either String Type
typeRep env ty = do
  kind <- typeKindInEnv env ty
  case runtimeRepFromKind kind of
    Left message -> Left (message <> " for " <> show ty)
    Right runtimeRep ->
      case convertRep env runtimeRep of
        Left message -> Left (message <> " for " <> show ty)
        Right converted -> Right converted

typeKindInEnv :: ConvertEnv -> TcType -> Either String TcType
typeKindInEnv env = Tc.typeKindInEnv (ceKindEnv env)

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
  case Map.lookup name builtinTable of
    Just (sort, _) ->
      Just (Name name sort (OriginTop (tyConPackageId tyCon) (tyConModuleName tyCon)))
    Nothing -> Nothing
  where
    name = T.dropWhile (== '\'') (tyConName tyCon)

-- | Invisible kind parameters that the type constructor quantifies before visible arguments.
extraKindVars :: ConvertEnv -> TyCon -> [TyVarId] -> Either String [TyVarId]
extraKindVars env tyCon visible = do
  ForAll vars _ _ <- kindScheme env tyCon
  let seen = map tvUnique visible
  pure (filter (\tyVar -> tvUnique tyVar `notElem` seen) vars)

invisibleKindArgs :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String [Type]
invisibleKindArgs env tyCon arguments expectedKind = do
  variables <- extraKindVars env tyCon []
  mapM (kindVarToType env tyCon arguments expectedKind) variables

kindVarToType :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> TyVarId -> Either String Type
kindVarToType env tyCon arguments expectedKind tyVar =
  case Map.lookup (tvUnique tyVar) (ceTyVars env) of
    Just found -> Right (tyVarType found)
    Nothing -> do
      substitution <- kindSubst env tyCon arguments expectedKind
      case Map.lookup (tvUnique tyVar) substitution of
        Just runtimeRep -> convertRep env runtimeRep
        Nothing ->
          Left
            ( "cannot infer the invisible kind argument "
                <> show (tvUnique tyVar)
                <> " for "
                <> T.unpack (tyConName tyCon)
            )

kindSubst :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String (Map Unique TcType)
kindSubst env tyCon arguments expectedKind = do
  ForAll quantified _ resultKind <- kindScheme env tyCon
  let quantifiedUniques = map tvUnique quantified
      (argumentSubstitution, remainingKind) = go quantifiedUniques Map.empty resultKind arguments
      resultSubstitution =
        case expectedKind of
          Just expected -> matchKind quantifiedUniques remainingKind expected
          Nothing -> Map.empty
  pure (argumentSubstitution <> resultSubstitution)
  where
    go quantifiedUniques substitution (KFun formal result) (argument : rest) =
      case typeKindInEnv env argument of
        Right argumentKind ->
          let found = matchKind quantifiedUniques (applySubst substitution formal) argumentKind
           in go quantifiedUniques (substitution <> found) (applySubst found result) rest
        Left _ -> go quantifiedUniques substitution result rest
    go _ substitution kind _ = (substitution, applySubst substitution kind)

    matchKind quantifiedUniques (TcTyVar tyVar) actual
      | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) actual
    matchKind quantifiedUniques (KTYPE (TcTyVar tyVar)) (KTYPE runtimeRep)
      | tvUnique tyVar `elem` quantifiedUniques = Map.singleton (tvUnique tyVar) runtimeRep
    matchKind quantifiedUniques (KFun left right) (KFun left' right') =
      matchKind quantifiedUniques left left' <> matchKind quantifiedUniques right right'
    matchKind quantifiedUniques (TcTyCon left formalArguments) (TcTyCon right actualArguments)
      | left == right,
        length formalArguments == length actualArguments =
          Map.unions (zipWith (matchKind quantifiedUniques) formalArguments actualArguments)
    matchKind _ _ _ = Map.empty

visibleArgumentKinds :: ConvertEnv -> TyCon -> [TcType] -> Maybe TcType -> Either String [TcType]
visibleArgumentKinds env tyCon arguments expectedKind = do
  ForAll _ _ resultKind <- kindScheme env tyCon
  substitution <- kindSubst env tyCon arguments expectedKind
  pure (takeArgumentKinds (applySubst substitution resultKind))
  where
    takeArgumentKinds (KFun argument result) = argument : takeArgumentKinds result
    takeArgumentKinds _ = []

kindScheme :: ConvertEnv -> TyCon -> Either String TypeScheme
kindScheme env tyCon =
  case Map.lookup (tyConKey tyCon) (ceKindEnv env) of
    Just scheme -> Right scheme
    Nothing -> Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon))

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
