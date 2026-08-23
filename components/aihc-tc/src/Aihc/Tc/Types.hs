{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | Core type representation for the type checker.
module Aihc.Tc.Types
  ( Unique (..),
    TyVarId (TyVarId, tvName, tvUnique),
    tvKind,
    setTyVarKind,
    TcType (..),
    TcTypeKey,
    TcKindEnv,
    TyCon (TyCon, tyConName, tyConArity),
    tyConKey,
    tyConPackageId,
    tyConModuleName,
    mkTyConWithOrigin,
    TypeScheme (..),
    boxedTupleTyConName,
    unboxedTupleTyConName,
    isUnboxedTupleType,
    typeTyCon,
    constraintTyCon,
    runtimeRepTyCon,
    levityTyCon,
    vecCountTyCon,
    vecElemTyCon,
    typeKindInEnv,
    constraintKind,
    runtimeRepKind,
    levityKind,
    vecCountKind,
    vecElemKind,
    mkTYPEKind,
    boxedRep,
    tupleRep,
    sumRep,
    vecRep,
    liftedRep,
    unliftedRep,
    intRep,
    int8Rep,
    int16Rep,
    int32Rep,
    int64Rep,
    wordRep,
    word8Rep,
    word16Rep,
    word32Rep,
    word64Rep,
    addrRep,
    floatRep,
    doubleRep,
    typeKindType,
    runtimeRepFromKind,
    runtimeRepOfTypeInEnv,
    isLiftedTypeInEnv,
    isUnliftedTypeInEnv,
    isUnboxedTupleTypeWithKind,
    pattern KTYPE,
    pattern KConstraint,
    pattern KRuntimeRep,
    pattern KLevity,
    pattern KVecCount,
    pattern KVecElem,
    pattern KFun,
    pattern KMeta,
    pattern KType,
    pattern BoxedRep,
    pattern TupleRep,
    pattern SumRep,
    pattern VecRep,
    pattern Lifted,
    pattern Unlifted,
    pattern IntRep,
    pattern Int8Rep,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern WordRep,
    pattern Word8Rep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern AddrRep,
    pattern FloatRep,
    pattern DoubleRep,
    typeSchemeBody,
    applySubst,
    applySubstPred,
    Pred (..),
    TcLevel (..),
    topTcLevel,
    pushLevel,
  )
where

import Aihc.Resolve (PackageId (..))
import Control.Monad (zipWithM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

newtype Unique = Unique Int
  deriving (Eq, Ord, Show, Read)

-- | A type variable and its type-level kind.
data TyVarId = TyVarIdInternal !Text !Unique !TcType
  deriving (Eq, Ord, Show, Read)

pattern TyVarId :: Text -> Unique -> TyVarId
pattern TyVarId {tvName, tvUnique} <- TyVarIdInternal tvName tvUnique _
  where
    TyVarId name unique = TyVarIdInternal name unique typeKindType

{-# COMPLETE TyVarId #-}

tvKind :: TyVarId -> TcType
tvKind (TyVarIdInternal _ _ kind) = kind

setTyVarKind :: TcType -> TyVarId -> TyVarId
setTyVarKind kind (TyVarIdInternal name unique _) = TyVarIdInternal name unique kind

-- | A type-constructor identity. Kind schemes live in the type-constructor environment.
data TyCon = TyConInternal !PackageId !Text !Text !Int
  deriving (Eq, Ord, Show, Read)

pattern TyCon :: Text -> Int -> TyCon
pattern TyCon {tyConName, tyConArity} <- TyConInternal _ _ tyConName tyConArity

{-# COMPLETE TyCon #-}

type TcTypeKey = (PackageId, Text, Text)

type TcKindEnv = Map TcTypeKey TypeScheme

tyConPackageId :: TyCon -> PackageId
tyConPackageId (TyConInternal packageId _ _ _) = packageId

tyConModuleName :: TyCon -> Text
tyConModuleName (TyConInternal _ moduleName _ _) = moduleName

tyConKey :: TyCon -> TcTypeKey
tyConKey tyCon = (tyConPackageId tyCon, tyConModuleName tyCon, tyConName tyCon)

mkTyConWithOrigin :: PackageId -> Text -> Text -> Int -> TyCon
mkTyConWithOrigin = TyConInternal

-- | Internal types. Kinds use this same representation.
data TcType
  = TcTyVar !TyVarId
  | TcMetaTv !Unique
  | TcTyCon !TyCon ![TcType]
  | TcFunTy !TcType !TcType
  | TcForAllTy !TyVarId !TcType
  | TcQualTy ![Pred] !TcType
  | TcAppTy !TcType !TcType
  deriving (Eq, Ord, Show, Read)

data TypeScheme = ForAll ![TyVarId] ![Pred] !TcType
  deriving (Eq, Ord, Show, Read)

typeSchemeBody :: TypeScheme -> TcType
typeSchemeBody (ForAll _ _ body) = body

data Pred
  = ClassPred !TyCon ![TcType]
  | EqPred !TcType !TcType
  deriving (Eq, Ord, Show, Read)

boxedTupleTyConName :: Int -> Text
boxedTupleTyConName arity =
  case arity of
    0 -> "Unit"
    1 -> "Solo"
    _ -> "Tuple" <> T.pack (show arity)

unboxedTupleTyConName :: Int -> Text
unboxedTupleTyConName arity = "Tuple" <> T.pack (show arity) <> "#"

-- These values identify source declarations. They do not contain kind data.
primTypeCon :: Text -> Int -> TyCon
primTypeCon = mkTyConWithOrigin (PackageId "aihc-prim") "GHC.Types"

promotedTypeCon :: Text -> Int -> TyCon
promotedTypeCon name = primTypeCon ("'" <> name)

typeTyCon, constraintTyCon, runtimeRepTyCon, levityTyCon, vecCountTyCon, vecElemTyCon :: TyCon
typeTyCon = primTypeCon "Type" 0
constraintTyCon = primTypeCon "Constraint" 0
runtimeRepTyCon = primTypeCon "RuntimeRep" 0
levityTyCon = primTypeCon "Levity" 0
vecCountTyCon = primTypeCon "VecCount" 0
vecElemTyCon = primTypeCon "VecElem" 0

typeKindType, constraintKind, runtimeRepKind, levityKind, vecCountKind, vecElemKind :: TcType
typeKindType = mkTYPEKind liftedRep
constraintKind = TcTyCon constraintTyCon []
runtimeRepKind = TcTyCon runtimeRepTyCon []
levityKind = TcTyCon levityTyCon []
vecCountKind = TcTyCon vecCountTyCon []
vecElemKind = TcTyCon vecElemTyCon []

mkTYPEKind :: TcType -> TcType
mkTYPEKind representation = TcTyCon (primTypeCon "TYPE" 1) [representation]

nullaryRep :: Text -> TcType
nullaryRep name = TcTyCon (promotedTypeCon name 0) []

liftedRep, unliftedRep, intRep, int8Rep, int16Rep, int32Rep, int64Rep :: TcType
wordRep, word8Rep, word16Rep, word32Rep, word64Rep, addrRep, floatRep, doubleRep :: TcType
liftedRep = boxedRep (TcTyCon (promotedTypeCon "Lifted" 0) [])
unliftedRep = boxedRep (TcTyCon (promotedTypeCon "Unlifted" 0) [])
intRep = nullaryRep "IntRep"
int8Rep = nullaryRep "Int8Rep"
int16Rep = nullaryRep "Int16Rep"
int32Rep = nullaryRep "Int32Rep"
int64Rep = nullaryRep "Int64Rep"

wordRep = nullaryRep "WordRep"

word8Rep = nullaryRep "Word8Rep"

word16Rep = nullaryRep "Word16Rep"

word32Rep = nullaryRep "Word32Rep"

word64Rep = nullaryRep "Word64Rep"

addrRep = nullaryRep "AddrRep"

floatRep = nullaryRep "FloatRep"

doubleRep = nullaryRep "DoubleRep"

boxedRep :: TcType -> TcType
boxedRep levity = TcTyCon (promotedTypeCon "BoxedRep" 1) [levity]

tupleRep :: [TcType] -> TcType
tupleRep fields = TcTyCon (promotedTypeCon "TupleRep" 1) [promotedList fields]

sumRep :: [TcType] -> TcType
sumRep fields = TcTyCon (promotedTypeCon "SumRep" 1) [promotedList fields]

vecRep :: TcType -> TcType -> TcType
vecRep count element = TcTyCon (promotedTypeCon "VecRep" 2) [count, element]

promotedList :: [TcType] -> TcType
promotedList = foldr promotedCons promotedNil
  where
    promotedNil = TcTyCon (promotedTypeCon "[]" 0) []
    promotedCons field rest = TcTyCon (promotedTypeCon ":" 2) [field, rest]

-- | Get a type kind from the complete type-constructor identity table.
typeKindInEnv :: TcKindEnv -> TcType -> Either String TcType
typeKindInEnv kindEnv = go
  where
    go rawType =
      case configurePrimitiveType rawType of
        TcTyVar tyVar -> Right (configurePrimitiveType (tvKind tyVar))
        TcMetaTv {} -> Left "type still has a meta variable"
        TcTyCon tyCon arguments -> do
          scheme <-
            maybe
              (Left ("missing kind scheme for type constructor: " <> T.unpack (tyConName tyCon)))
              Right
              (Map.lookup (tyConKey tyCon) kindEnv)
          applyArguments scheme arguments
        TcFunTy {} -> Right (configurePrimitiveType typeKindType)
        TcForAllTy _ body -> go body
        TcQualTy _ body -> go body
        TcAppTy function argument -> do
          functionKind <- go function
          applyKind functionKind argument

    applyArguments (ForAll quantified _ body) = applyMany (map tvUnique quantified) (configurePrimitiveType body)

    applyMany _ kind [] = Right kind
    applyMany quantified kind (argument : rest) = do
      kind' <- applyKindWith quantified kind argument
      applyMany quantified kind' rest

    applyKind = applyKindWith []

    applyKindWith quantified (TcFunTy formal result) argument = do
      actual <- go argument
      substitution <- matchKinds quantified formal actual
      Right (applySubst substitution result)
    applyKindWith _ kind _ = Left ("type application uses a non-function kind: " <> show kind)

    matchKinds quantified formal actual =
      case (formal, actual) of
        (TcTyVar tyVar, _)
          | tvUnique tyVar `elem` quantified -> Right (Map.singleton (tvUnique tyVar) actual)
        (KTYPE formalRep, KTYPE actualRep) -> matchKinds quantified formalRep actualRep
        (TcFunTy left right, TcFunTy left' right') ->
          Map.union <$> matchKinds quantified left left' <*> matchKinds quantified right right'
        (TcTyCon left leftArguments, TcTyCon right rightArguments)
          | left == right,
            length leftArguments == length rightArguments ->
              Map.unions <$> zipWithM (matchKinds quantified) leftArguments rightArguments
        _
          | equivalentKind formal actual -> Right Map.empty
          | otherwise -> Left ("kind mismatch: expected " <> show formal <> ", got " <> show actual)

    equivalentKind left right =
      configurePrimitiveType left == configurePrimitiveType right
        || case (left, right) of
          (KTYPE leftRep, KTYPE rightRep) -> leftRep == rightRep
          _ -> False

    configurePrimitiveType ty =
      case ty of
        TcTyVar tyVar -> TcTyVar (setTyVarKind (configurePrimitiveType (tvKind tyVar)) tyVar)
        TcMetaTv {} -> ty
        TcTyCon tyCon arguments -> TcTyCon (configurePrimitiveTyCon tyCon) (map configurePrimitiveType arguments)
        TcFunTy argument result -> TcFunTy (configurePrimitiveType argument) (configurePrimitiveType result)
        TcForAllTy tyVar body ->
          TcForAllTy
            (setTyVarKind (configurePrimitiveType (tvKind tyVar)) tyVar)
            (configurePrimitiveType body)
        TcQualTy predicates body -> TcQualTy (map configurePred predicates) (configurePrimitiveType body)
        TcAppTy function argument -> TcAppTy (configurePrimitiveType function) (configurePrimitiveType argument)

    configurePred predicate =
      case predicate of
        ClassPred className arguments -> ClassPred (configurePrimitiveTyCon className) (map configurePrimitiveType arguments)
        EqPred left right -> EqPred (configurePrimitiveType left) (configurePrimitiveType right)

    configurePrimitiveTyCon tyCon
      | tyConPackageId tyCon == PackageId "aihc-prim",
        tyConModuleName tyCon == "GHC.Types" =
          mkTyConWithOrigin primitivePackage "GHC.Types" (tyConName tyCon) (tyConArity tyCon)
      | otherwise = tyCon

    primitivePackage =
      case [ packageId
           | ((packageId, moduleName, name), _) <- Map.toList kindEnv,
             moduleName == "GHC.Types",
             name == "TYPE"
           ] of
        packageId : _ -> packageId
        [] -> PackageId "aihc-prim"

runtimeRepOfTypeInEnv :: TcKindEnv -> TcType -> Either String TcType
runtimeRepOfTypeInEnv kindEnv ty = typeKindInEnv kindEnv ty >>= runtimeRepFromKind

isLiftedTypeInEnv :: TcKindEnv -> TcType -> Bool
isLiftedTypeInEnv kindEnv ty =
  case runtimeRepOfTypeInEnv kindEnv ty of
    Right representation -> matchesLiftedRuntimeRep representation
    Left _ -> False

isUnliftedTypeInEnv :: TcKindEnv -> TcType -> Bool
isUnliftedTypeInEnv kindEnv ty =
  case runtimeRepOfTypeInEnv kindEnv ty of
    Right representation -> not (matchesLiftedRuntimeRep representation)
    Left _ -> False

-- | Apply a type-variable substitution to a type.
applySubst :: Map Unique TcType -> TcType -> TcType
applySubst substitution = go
  where
    go ty =
      case ty of
        TcTyVar tyVar -> Map.findWithDefault ty (tvUnique tyVar) substitution
        TcMetaTv {} -> ty
        TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments)
        TcFunTy argument result -> TcFunTy (go argument) (go result)
        TcForAllTy tyVar body ->
          TcForAllTy tyVar (applySubst (Map.delete (tvUnique tyVar) substitution) body)
        TcQualTy predicates body -> TcQualTy (map (applySubstPred substitution) predicates) (go body)
        TcAppTy function argument -> applyType (go function) (go argument)

    applyType (TcTyCon tyCon arguments) argument = TcTyCon tyCon (arguments <> [argument])
    applyType function argument = TcAppTy function argument

-- | Apply a type-variable substitution to a predicate.
applySubstPred :: Map Unique TcType -> Pred -> Pred
applySubstPred substitution predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map (applySubst substitution) arguments)
    EqPred left right -> EqPred (applySubst substitution left) (applySubst substitution right)

pattern KTYPE :: TcType -> TcType
pattern KTYPE representation <- (matchTYPEKind -> Just representation)
  where
    KTYPE representation = mkTYPEKind representation

pattern KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KType :: TcType
pattern KConstraint <- (matchesNullary "Constraint" -> True) where KConstraint = constraintKind
pattern KRuntimeRep <- (matchesNullary "RuntimeRep" -> True) where KRuntimeRep = runtimeRepKind
pattern KLevity <- (matchesNullary "Levity" -> True) where KLevity = levityKind
pattern KVecCount <- (matchesNullary "VecCount" -> True) where KVecCount = vecCountKind
pattern KVecElem <- (matchesNullary "VecElem" -> True) where KVecElem = vecElemKind
pattern KType <- (matchesLiftedTypeKind -> True) where KType = typeKindType

pattern KFun :: TcType -> TcType -> TcType
pattern KFun argument result = TcFunTy argument result

pattern KMeta :: Unique -> TcType
pattern KMeta unique = TcMetaTv unique

matchTYPEKind :: TcType -> Maybe TcType
matchTYPEKind kind =
  case kind of
    TcTyCon tyCon []
      | tyConName tyCon `elem` ["Type", "LiftedType"] -> Just liftedRep
      | tyConName tyCon == "UnliftedType" -> Just unliftedRep
    TcTyCon tyCon [representation]
      | tyConName tyCon == "TYPE" -> Just representation
    _ -> Nothing

matchesLiftedTypeKind :: TcType -> Bool
matchesLiftedTypeKind = maybe False matchesLiftedRuntimeRep . matchTYPEKind

matchesLiftedRuntimeRep :: TcType -> Bool
matchesLiftedRuntimeRep representation =
  case representation of
    TcTyCon boxed [TcTyCon levity []] ->
      tyConName boxed == "'BoxedRep"
        && tyConName levity == "'Lifted"
    _ -> False

pattern BoxedRep :: TcType -> TcType
pattern BoxedRep levity <- (matchUnaryRep "BoxedRep" -> Just levity)
  where
    BoxedRep levity = boxedRep levity

pattern TupleRep :: [TcType] -> TcType
pattern TupleRep fields <- (matchListRep "TupleRep" -> Just fields)
  where
    TupleRep fields = tupleRep fields

pattern SumRep :: [TcType] -> TcType
pattern SumRep fields <- (matchListRep "SumRep" -> Just fields)
  where
    SumRep fields = sumRep fields

pattern VecRep :: TcType -> TcType -> TcType
pattern VecRep count element <- (matchBinaryRep "VecRep" -> Just (count, element))
  where
    VecRep count element = vecRep count element

pattern Lifted, Unlifted :: TcType
pattern Lifted <- (matchesNullary "Lifted" -> True)
  where
    Lifted = TcTyCon (promotedTypeCon "Lifted" 0) []
pattern Unlifted <- (matchesNullary "Unlifted" -> True)
  where
    Unlifted = TcTyCon (promotedTypeCon "Unlifted" 0) []

pattern IntRep, Int8Rep, Int16Rep, Int32Rep, Int64Rep :: TcType

pattern WordRep, Word8Rep, Word16Rep, Word32Rep, Word64Rep :: TcType

pattern AddrRep, FloatRep, DoubleRep :: TcType

pattern IntRep <- (matchesNullary "IntRep" -> True) where IntRep = intRep

pattern Int8Rep <- (matchesNullary "Int8Rep" -> True) where Int8Rep = int8Rep

pattern Int16Rep <- (matchesNullary "Int16Rep" -> True) where Int16Rep = int16Rep

pattern Int32Rep <- (matchesNullary "Int32Rep" -> True) where Int32Rep = int32Rep

pattern Int64Rep <- (matchesNullary "Int64Rep" -> True) where Int64Rep = int64Rep

pattern WordRep <- (matchesNullary "WordRep" -> True) where WordRep = wordRep

pattern Word8Rep <- (matchesNullary "Word8Rep" -> True) where Word8Rep = word8Rep

pattern Word16Rep <- (matchesNullary "Word16Rep" -> True) where Word16Rep = word16Rep

pattern Word32Rep <- (matchesNullary "Word32Rep" -> True) where Word32Rep = word32Rep

pattern Word64Rep <- (matchesNullary "Word64Rep" -> True) where Word64Rep = word64Rep

pattern AddrRep <- (matchesNullary "AddrRep" -> True) where AddrRep = addrRep

pattern FloatRep <- (matchesNullary "FloatRep" -> True) where FloatRep = floatRep

pattern DoubleRep <- (matchesNullary "DoubleRep" -> True) where DoubleRep = doubleRep

matchUnaryRep :: Text -> TcType -> Maybe TcType
matchUnaryRep expected (TcTyCon tyCon [argument])
  | T.dropWhile (== '\'') (tyConName tyCon) == expected = Just argument
matchUnaryRep _ _ = Nothing

matchBinaryRep :: Text -> TcType -> Maybe (TcType, TcType)
matchBinaryRep expected (TcTyCon tyCon [left, right])
  | T.dropWhile (== '\'') (tyConName tyCon) == expected = Just (left, right)
matchBinaryRep _ _ = Nothing

matchListRep :: Text -> TcType -> Maybe [TcType]
matchListRep expected (TcTyCon tyCon [listType])
  | T.dropWhile (== '\'') (tyConName tyCon) == expected = decodePromotedList listType
matchListRep _ _ = Nothing

decodePromotedList :: TcType -> Maybe [TcType]
decodePromotedList ty =
  case ty of
    TcTyCon tyCon []
      | tyConName tyCon == "'[]" -> Just []
    TcTyCon tyCon [field, rest]
      | tyConName tyCon == "':" -> (field :) <$> decodePromotedList rest
    _ -> Nothing

matchesNullary :: Text -> TcType -> Bool
matchesNullary expected (TcTyCon tyCon []) = T.dropWhile (== '\'') (tyConName tyCon) == expected
matchesNullary _ _ = False

runtimeRepFromKind :: TcType -> Either String TcType
runtimeRepFromKind kind =
  case kind of
    TcTyCon tyCon []
      | tyConName tyCon `elem` ["Type", "LiftedType", "Constraint"] -> Right liftedRep
      | tyConName tyCon == "UnliftedType" -> Right unliftedRep
    TcTyCon tyCon [representation]
      | tyConName tyCon == "TYPE" -> Right representation
    _ -> Left ("type does not have a runtime representation: " <> show kind)

isUnboxedTupleTypeWithKind :: TcType -> TcType -> Bool
isUnboxedTupleTypeWithKind ty kind =
  case (ty, runtimeRepFromKind kind) of
    (TcTyCon tyCon arguments, Right (TupleRep fields)) ->
      tyConName tyCon == unboxedTupleTyConName (length arguments)
        && tyConArity tyCon == length arguments
        && length fields == length arguments
    _ -> False

isUnboxedTupleType :: TcType -> Bool
isUnboxedTupleType (TcTyCon tyCon arguments) =
  tyConName tyCon == unboxedTupleTyConName (length arguments)
    && tyConArity tyCon == length arguments
isUnboxedTupleType _ = False

newtype TcLevel = TcLevel Int
  deriving (Eq, Ord, Show, Read)

topTcLevel :: TcLevel
topTcLevel = TcLevel 0

pushLevel :: TcLevel -> TcLevel
pushLevel (TcLevel level) = TcLevel (level + 1)
