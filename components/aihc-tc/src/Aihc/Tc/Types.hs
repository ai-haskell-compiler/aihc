{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Core type representation for the type checker.
--
-- These are the semantic types used during type checking, distinct from
-- the surface syntax types in "Aihc.Parser.Syntax". Surface types are
-- syntax; internal types are semantic.
--
-- For the MVP, meta-variable solutions are stored in a map in the TcM
-- state rather than using STRef. This keeps the API simpler while being
-- functionally equivalent. The module structure supports migrating to
-- STRef-backed meta-variables later without changing the public interface.
module Aihc.Tc.Types
  ( -- * Unique identifiers
    Unique (..),

    -- * Type variables
    TyVarId (TyVarId, tvName, tvUnique),
    tvKind,
    setTyVarKind,

    -- * Types
    TcType (..),
    TyCon (TyCon, tyConName, tyConArity),
    tyConPackageId,
    tyConModuleName,
    tyConKind,
    tyConKindScheme,
    mkTyCon,
    mkTyConWithOrigin,
    mkTyConWithOriginScheme,
    setTyConKindScheme,
    Kind (KTYPE, KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KFun, KMeta, KType),
    RuntimeRep (..),
    Levity (..),
    VecCount (..),
    VecElem (..),
    TypeScheme (..),
    kindFromTypeScheme,
    boxedTupleTyConName,
    unboxedTupleTyConName,
    isUnboxedTupleType,
    liftedRuntimeRep,
    liftedTypeKind,
    typeKind,
    runtimeRepOfType,
    runtimeRepFromType,
    isLiftedType,
    isUnliftedType,

    -- * Predicates
    Pred (..),

    -- * Tc level
    TcLevel (..),
    topTcLevel,
    pushLevel,
  )
where

import Aihc.Resolve (PackageId (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Source-level names of the lifted tuple types declared by @ghc-prim@.
-- Their data constructors retain the familiar parenthesized comma syntax.
boxedTupleTyConName :: Int -> Text
boxedTupleTyConName arity =
  case arity of
    0 -> "Unit"
    1 -> "Solo"
    _ -> "Tuple" <> T.pack (show arity)

-- | Source-level names of the unboxed tuple types declared by @GHC.Types@.
unboxedTupleTyConName :: Int -> Text
unboxedTupleTyConName arity = "Tuple" <> T.pack (show arity) <> "#"

-- | Unique identifier for type variables and evidence variables.
newtype Unique = Unique Int
  deriving (Eq, Ord, Show, Read)

-- | A type variable identifier, carrying both a human-readable name and
-- a unique tag for alpha-equivalence.
data TyVarId = TyVarIdInternal !Text !Unique !Kind
  deriving (Show, Read)

pattern TyVarId :: Text -> Unique -> TyVarId
pattern TyVarId {tvName, tvUnique} <- TyVarIdInternal tvName tvUnique _
  where
    TyVarId name unique = TyVarIdInternal name unique liftedTypeKind

{-# COMPLETE TyVarId #-}

tvKind :: TyVarId -> Kind
tvKind (TyVarIdInternal _ _ kind) = kind

setTyVarKind :: Kind -> TyVarId -> TyVarId
setTyVarKind kind (TyVarIdInternal name unique _) = TyVarIdInternal name unique kind

instance Eq TyVarId where
  a == b = tvUnique a == tvUnique b

instance Ord TyVarId where
  compare a b = compare (tvUnique a) (tvUnique b)

-- | Type constructor. The kind scheme is the only stored kind representation.
data TyCon = TyConInternal !PackageId !Text !Text !Int !TypeScheme
  deriving (Show, Read)

instance Eq TyCon where
  left == right =
    (tyConPackageId left, tyConModuleName left, tyConName left, tyConArity left)
      == (tyConPackageId right, tyConModuleName right, tyConName right, tyConArity right)

instance Ord TyCon where
  compare left right =
    compare
      (tyConPackageId left, tyConModuleName left, tyConName left, tyConArity left)
      (tyConPackageId right, tyConModuleName right, tyConName right, tyConArity right)

pattern TyCon :: Text -> Int -> TyCon
pattern TyCon {tyConName, tyConArity} <- TyConInternal _ _ tyConName tyConArity _
  where
    TyCon name arity =
      TyConInternal
        (PackageId "aihc-internal")
        "Aihc.Internal"
        name
        arity
        (kindSchemeFromKind (wiredInTyConKind name arity))

{-# COMPLETE TyCon #-}

tyConKind :: TyCon -> Kind
tyConKind = kindFromTypeScheme . tyConKindScheme

tyConKindScheme :: TyCon -> TypeScheme
tyConKindScheme (TyConInternal _ _ _ _ scheme) = scheme

tyConPackageId :: TyCon -> PackageId
tyConPackageId (TyConInternal packageId _ _ _ _) = packageId

tyConModuleName :: TyCon -> Text
tyConModuleName (TyConInternal _ moduleName _ _ _) = moduleName

mkTyCon :: Text -> Int -> Kind -> TyCon
mkTyCon = mkTyConWithOrigin (PackageId "aihc-internal") "Aihc.Internal"

-- | Make a type constructor with its installed package and module identity.
mkTyConWithOrigin :: PackageId -> Text -> Text -> Int -> Kind -> TyCon
mkTyConWithOrigin packageId moduleName name arity inferredKind =
  TyConInternal packageId moduleName name arity (kindSchemeFromKind (fromMaybe inferredKind (fixedTyConKind name)))

-- | Make a type constructor from its authoritative kind scheme.
mkTyConWithOriginScheme :: PackageId -> Text -> Text -> Int -> TypeScheme -> TyCon
mkTyConWithOriginScheme = TyConInternal

-- | Replace a type constructor's authoritative kind scheme.
setTyConKindScheme :: TypeScheme -> TyCon -> TyCon
setTyConKindScheme scheme (TyConInternal packageId moduleName name arity _) =
  TyConInternal packageId moduleName name arity scheme

-- | Kinds for the type language checked by @aihc-tc@.
data Kind
  = KTYPE !RuntimeRep
  | KConstraint
  | KRuntimeRep
  | KLevity
  | KVecCount
  | KVecElem
  | KFun !Kind !Kind
  | KMeta !Unique
  deriving (Eq, Ord, Show, Read)

-- | The traditional @Type@ / @*@ kind.
pattern KType :: Kind
pattern KType = KTYPE (BoxedRep Lifted)

data RuntimeRep
  = VecRep !VecCount !VecElem
  | TupleRep ![RuntimeRep]
  | SumRep ![RuntimeRep]
  | BoxedRep !Levity
  | IntRep
  | Int8Rep
  | Int16Rep
  | Int32Rep
  | Int64Rep
  | WordRep
  | Word8Rep
  | Word16Rep
  | Word32Rep
  | Word64Rep
  | AddrRep
  | FloatRep
  | DoubleRep
  | RuntimeRepVar !Unique
  | RuntimeRepMeta !Unique
  deriving (Eq, Ord, Show, Read)

data Levity = Lifted | Unlifted
  deriving (Eq, Ord, Show, Read)

data VecCount = Vec2 | Vec4 | Vec8 | Vec16 | Vec32 | Vec64
  deriving (Eq, Ord, Show, Read)

data VecElem
  = Int8ElemRep
  | Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep
  deriving (Eq, Ord, Show, Read)

liftedRuntimeRep :: RuntimeRep
liftedRuntimeRep = BoxedRep Lifted

liftedTypeKind :: Kind
liftedTypeKind = KTYPE liftedRuntimeRep

-- | Internal type representation.
--
-- Note: 'TcForAllTy', 'TcQualTy', 'TcAppTy' are included from the start
-- to support polymorphism and type classes. For the MVP only
-- 'TcTyVar', 'TcMetaTv', 'TcTyCon', and 'TcFunTy' are actively used
-- during constraint generation and solving.
data TcType
  = -- | Rigid (skolem) type variable.
    TcTyVar !TyVarId
  | -- | Meta (unification) variable, identified by 'Unique'.
    TcMetaTv !Unique
  | -- | Saturated or partially applied type constructor.
    TcTyCon !TyCon ![TcType]
  | -- | Function type @a -> b@.
    TcFunTy !TcType !TcType
  | -- | Universal quantification @forall a. ty@.
    TcForAllTy !TyVarId !TcType
  | -- | Qualified type @(constraints) => ty@.
    TcQualTy ![Pred] !TcType
  | -- | Unsaturated type application @f a@.
    TcAppTy !TcType !TcType
  | -- | A primitive type constructor used to define kind schemes without a recursive 'TyCon'.
    TcBuiltinTyCon !Text !Int ![TcType]
  deriving (Eq, Show, Read)

-- | Whether a type has an unlifted runtime representation in the subset of
-- primitive types and runtime representations currently modeled by AIHC.
-- This is deliberately semantic rather than a @#@ suffix check: user-defined
-- lifted type constructors may legally end in @#@.
isUnliftedType :: TcType -> Bool
isUnliftedType ty =
  case runtimeRepOfType ty of
    Right runtimeRep -> runtimeRep /= liftedRuntimeRep
    Left _ -> False

isLiftedType :: TcType -> Bool
isLiftedType ty = runtimeRepOfType ty == Right liftedRuntimeRep

runtimeRepOfType :: TcType -> Either String RuntimeRep
runtimeRepOfType ty =
  case typeKind ty of
    KTYPE runtimeRep -> Right runtimeRep
    KConstraint -> Right liftedRuntimeRep
    other -> Left ("type does not have a runtime representation: " <> show other)

typeKind :: TcType -> Kind
typeKind ty =
  case ty of
    TcTyVar tyVar -> tvKind tyVar
    TcMetaTv {} -> liftedTypeKind
    TcTyCon tyCon args
      | isUnboxedSumTyCon (tyConName tyCon) (tyConArity tyCon),
        length args == tyConArity tyCon ->
          KTYPE (SumRep (map runtimeRepOrLifted args))
      | otherwise -> applyTyConKind tyCon args
    TcFunTy {} -> liftedTypeKind
    TcForAllTy _ body -> typeKind body
    TcQualTy _ body -> typeKind body
    TcAppTy function _ -> applyKindArgumentCount (typeKind function) 1
    TcBuiltinTyCon name arity arguments ->
      applyKindArgumentCount (wiredInTyConKind name arity) (length arguments)
  where
    runtimeRepOrLifted argument =
      case runtimeRepOfType argument of
        Right runtimeRep -> runtimeRep
        Left _ -> liftedRuntimeRep

applyKindArgumentCount :: Kind -> Int -> Kind
applyKindArgumentCount kind count
  | count <= 0 = kind
applyKindArgumentCount (KFun _ result) count = applyKindArgumentCount result (count - 1)
applyKindArgumentCount kind _ = kind

applyTyConKind :: TyCon -> [TcType] -> Kind
applyTyConKind tyCon = go Map.empty authoritativeKind
  where
    scheme@(ForAll kindTyVars _ _) = tyConKindScheme tyCon
    authoritativeKind = kindFromTypeScheme scheme
    quantified =
      kindParameterRuntimeRepVariables authoritativeKind
        <> Set.fromList (map tvUnique kindTyVars)

    go substitution kind [] = substituteKindRuntimeReps substitution kind
    go substitution (KFun formal result) (argument : arguments) =
      let formal' = substituteKindRuntimeReps substitution formal
          actual = typeKind argument
          substitution' = matchKindRuntimeReps quantified formal' actual <> substitution
       in go substitution' (substituteKindRuntimeReps substitution' result) arguments
    go substitution kind _ = substituteKindRuntimeReps substitution kind

kindParameterRuntimeRepVariables :: Kind -> Set.Set Unique
kindParameterRuntimeRepVariables kind =
  case kind of
    KFun parameter result -> runtimeRepVariablesInKind parameter <> kindParameterRuntimeRepVariables result
    _ -> Set.empty

runtimeRepVariablesInKind :: Kind -> Set.Set Unique
runtimeRepVariablesInKind kind =
  case kind of
    KTYPE runtimeRep -> runtimeRepVariables runtimeRep
    KFun argument result -> runtimeRepVariablesInKind argument <> runtimeRepVariablesInKind result
    _ -> Set.empty
  where
    runtimeRepVariables runtimeRep =
      case runtimeRep of
        RuntimeRepVar unique -> Set.singleton unique
        TupleRep fields -> Set.unions (map runtimeRepVariables fields)
        SumRep fields -> Set.unions (map runtimeRepVariables fields)
        _ -> Set.empty

matchKindRuntimeReps :: Set.Set Unique -> Kind -> Kind -> Map.Map Unique RuntimeRep
matchKindRuntimeReps quantified formal actual =
  case (formal, actual) of
    (KTYPE formalRep, KTYPE actualRep) -> matchRuntimeRep formalRep actualRep
    (KFun formalArgument formalResult, KFun actualArgument actualResult) ->
      matchKindRuntimeReps quantified formalArgument actualArgument
        <> matchKindRuntimeReps quantified formalResult actualResult
    _ -> Map.empty
  where
    matchRuntimeRep formalRep actualRep =
      case (formalRep, actualRep) of
        (RuntimeRepVar unique, _)
          | unique `Set.member` quantified -> Map.singleton unique actualRep
        (TupleRep formalFields, TupleRep actualFields) ->
          Map.unionsWith const (zipWith matchRuntimeRep formalFields actualFields)
        (SumRep formalFields, SumRep actualFields) ->
          Map.unionsWith const (zipWith matchRuntimeRep formalFields actualFields)
        _ -> Map.empty

substituteKindRuntimeReps :: Map.Map Unique RuntimeRep -> Kind -> Kind
substituteKindRuntimeReps substitution kind =
  case kind of
    KTYPE runtimeRep -> KTYPE (substituteRuntimeRep runtimeRep)
    KFun argument result ->
      KFun
        (substituteKindRuntimeReps substitution argument)
        (substituteKindRuntimeReps substitution result)
    _ -> kind
  where
    substituteRuntimeRep runtimeRep =
      case runtimeRep of
        RuntimeRepVar unique -> Map.findWithDefault runtimeRep unique substitution
        TupleRep fields -> TupleRep (map substituteRuntimeRep fields)
        SumRep fields -> SumRep (map substituteRuntimeRep fields)
        _ -> runtimeRep

-- | Test whether a constructed type has an explicit unboxed-tuple representation.
isUnboxedTupleType :: TcType -> Bool
isUnboxedTupleType (TcTyCon tyCon arguments) =
  let arity = length arguments
   in tyConName tyCon == unboxedTupleTyConName arity
        && arity == tyConArity tyCon
        && case applyTyConKind tyCon arguments of
          KTYPE (TupleRep fields) -> length fields == length arguments
          _ -> False
isUnboxedTupleType _ = False

isUnboxedSumTyCon :: Text -> Int -> Bool
isUnboxedSumTyCon name arity =
  arity >= 2
    && name == "(#" <> T.replicate (arity - 1) "|" <> "#)"

wiredInTyConKind :: Text -> Int -> Kind
wiredInTyConKind name arity =
  fromMaybe (defaultTyConKind name arity) (fixedTyConKind name)

fixedTyConKind :: Text -> Maybe Kind
fixedTyConKind name =
  case name of
    "State#" -> Just (KFun liftedTypeKind (KTYPE (TupleRep [])))
    "Array#" -> Just (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted)))
    "ByteArray#" -> Just (KTYPE (BoxedRep Unlifted))
    "MutableArray#" -> Just (KFun liftedTypeKind (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted))))
    "MutableByteArray#" -> Just (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted)))
    "MVar#" -> Just (KFun liftedTypeKind (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted))))
    "MutVar#" -> Just (KFun liftedTypeKind (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted))))
    "StableName#" -> Just (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted)))
    "ThreadId#" -> Just (KTYPE (BoxedRep Unlifted))
    _
      | Just runtimeRep <- primitiveRuntimeRep name -> Just (KTYPE runtimeRep)
      | isPromotedRuntimeRep name -> Just KRuntimeRep
      | otherwise ->
          lookup
            name
            [ ("TYPE", KFun KRuntimeRep liftedTypeKind),
              ("RuntimeRep", liftedTypeKind),
              ("Levity", liftedTypeKind),
              ("VecCount", liftedTypeKind),
              ("VecElem", liftedTypeKind),
              ("Constraint", liftedTypeKind),
              ("*", liftedTypeKind),
              ("Type", liftedTypeKind),
              ("(->)", KFun liftedTypeKind (KFun liftedTypeKind liftedTypeKind)),
              ("[]", KFun liftedTypeKind liftedTypeKind),
              (":", KFun liftedTypeKind (KFun (KFun liftedTypeKind liftedTypeKind) (KFun liftedTypeKind liftedTypeKind)))
            ]

defaultTyConKind :: Text -> Int -> Kind
defaultTyConKind _ arity = foldr KFun liftedTypeKind (replicate arity liftedTypeKind)

isPromotedRuntimeRep :: Text -> Bool
isPromotedRuntimeRep name =
  T.dropWhile (== '\'') name
    `elem` [ "LiftedRep",
             "UnliftedRep",
             "IntRep",
             "Int8Rep",
             "Int16Rep",
             "Int32Rep",
             "Int64Rep",
             "WordRep",
             "Word8Rep",
             "Word16Rep",
             "Word32Rep",
             "Word64Rep",
             "AddrRep",
             "FloatRep",
             "DoubleRep"
           ]

primitiveRuntimeRep :: Text -> Maybe RuntimeRep
primitiveRuntimeRep name =
  lookup
    name
    [ ("Addr#", AddrRep),
      ("Char#", WordRep),
      ("Double#", DoubleRep),
      ("Float#", FloatRep),
      ("Int#", IntRep),
      ("Int8#", Int8Rep),
      ("Int16#", Int16Rep),
      ("Int32#", Int32Rep),
      ("Int64#", Int64Rep),
      ("Word#", WordRep),
      ("Word8#", Word8Rep),
      ("Word16#", Word16Rep),
      ("Word32#", Word32Rep),
      ("Word64#", Word64Rep)
    ]

-- | A type scheme: universally quantified type with constraints.
--
-- @ForAll [a, b] [Eq a] (a -> b -> Bool)@
-- represents @forall a b. Eq a => a -> b -> Bool@.
data TypeScheme = ForAll ![TyVarId] ![Pred] !TcType
  deriving (Eq, Show, Read)

kindSchemeFromKind :: Kind -> TypeScheme
kindSchemeFromKind = ForAll [] [] . kindAsType

kindAsType :: Kind -> TcType
kindAsType kind =
  case kind of
    KTYPE runtimeRep
      | runtimeRep == liftedRuntimeRep -> builtin "Type" 0 []
      | otherwise -> builtin "TYPE" 1 [runtimeRepAsType runtimeRep]
    KConstraint -> builtin "Constraint" 0 []
    KRuntimeRep -> builtin "RuntimeRep" 0 []
    KLevity -> builtin "Levity" 0 []
    KVecCount -> builtin "VecCount" 0 []
    KVecElem -> builtin "VecElem" 0 []
    KMeta unique -> TcMetaTv unique
    KFun argument result -> TcFunTy (kindAsType argument) (kindAsType result)
  where
    builtin = TcBuiltinTyCon

runtimeRepAsType :: RuntimeRep -> TcType
runtimeRepAsType runtimeRep =
  case runtimeRep of
    BoxedRep Lifted -> promoted "LiftedRep" 0 []
    BoxedRep Unlifted -> promoted "UnliftedRep" 0 []
    IntRep -> nullary "IntRep"
    Int8Rep -> nullary "Int8Rep"
    Int16Rep -> nullary "Int16Rep"
    Int32Rep -> nullary "Int32Rep"
    Int64Rep -> nullary "Int64Rep"
    WordRep -> nullary "WordRep"
    Word8Rep -> nullary "Word8Rep"
    Word16Rep -> nullary "Word16Rep"
    Word32Rep -> nullary "Word32Rep"
    Word64Rep -> nullary "Word64Rep"
    AddrRep -> nullary "AddrRep"
    FloatRep -> nullary "FloatRep"
    DoubleRep -> nullary "DoubleRep"
    RuntimeRepVar unique ->
      TcTyVar (setTyVarKind KRuntimeRep (TyVarId ("rep" <> T.pack (showUnique unique)) unique))
    RuntimeRepMeta unique -> TcMetaTv unique
    TupleRep fields -> promotedList "TupleRep" fields
    SumRep fields -> promotedList "SumRep" fields
    VecRep count element ->
      promoted
        "VecRep"
        2
        [nullary (T.pack (show count)), nullary (T.pack (show element))]
  where
    nullary name = promoted name 0 []
    promoted name = TcBuiltinTyCon ("'" <> name)
    promotedList name fields =
      promoted name 1 [promoted "[]" (length fields) (map runtimeRepAsType fields)]
    showUnique (Unique value) = show value

kindFromTypeScheme :: TypeScheme -> Kind
kindFromTypeScheme (ForAll _ _ body) = typeAsKind body

typeAsKind :: TcType -> Kind
typeAsKind ty =
  case ty of
    TcTyCon tyCon []
      | tyConName tyCon `elem` ["*", "Type"] -> KType
      | tyConName tyCon == "Constraint" -> KConstraint
      | tyConName tyCon == "RuntimeRep" -> KRuntimeRep
      | tyConName tyCon == "Levity" -> KLevity
      | tyConName tyCon == "VecCount" -> KVecCount
      | tyConName tyCon == "VecElem" -> KVecElem
    TcTyCon tyCon [runtimeRep]
      | tyConName tyCon == "TYPE" -> KTYPE (runtimeRepFromType runtimeRep)
    TcFunTy argument result -> KFun (typeAsKind argument) (typeAsKind result)
    TcForAllTy _ body -> typeAsKind body
    TcMetaTv unique -> KMeta unique
    TcTyVar tyVar -> tvKind tyVar
    TcBuiltinTyCon name _ arguments -> typeAsBuiltinKind name arguments
    _ -> KType

typeAsBuiltinKind :: Text -> [TcType] -> Kind
typeAsBuiltinKind name arguments =
  case (bareName name, arguments) of
    ("*", []) -> KType
    ("Type", []) -> KType
    ("Constraint", []) -> KConstraint
    ("RuntimeRep", []) -> KRuntimeRep
    ("Levity", []) -> KLevity
    ("VecCount", []) -> KVecCount
    ("VecElem", []) -> KVecElem
    ("TYPE", [runtimeRep]) -> KTYPE (runtimeRepFromType runtimeRep)
    _ -> KType

runtimeRepFromType :: TcType -> RuntimeRep
runtimeRepFromType ty =
  case ty of
    TcTyVar tyVar -> RuntimeRepVar (tvUnique tyVar)
    TcMetaTv unique -> RuntimeRepMeta unique
    TcTyCon tyCon [] ->
      fromMaybe liftedRuntimeRep (runtimeRepConstructorByName (tyConName tyCon))
    TcTyCon tyCon [levity]
      | bareName (tyConName tyCon) == "BoxedRep" -> BoxedRep (typeAsLevity levity)
    TcTyCon tyCon [fields]
      | bareName (tyConName tyCon) == "TupleRep" -> TupleRep (map runtimeRepFromType (promotedListItems fields))
      | bareName (tyConName tyCon) == "SumRep" -> SumRep (map runtimeRepFromType (promotedListItems fields))
    TcTyCon tyCon [count, element]
      | bareName (tyConName tyCon) == "VecRep" -> VecRep (typeAsVecCount count) (typeAsVecElem element)
    TcBuiltinTyCon name _ arguments -> runtimeRepFromBuiltin name arguments
    _ -> liftedRuntimeRep

runtimeRepFromBuiltin :: Text -> [TcType] -> RuntimeRep
runtimeRepFromBuiltin name arguments =
  case (bareName name, arguments) of
    ("BoxedRep", [levity]) -> BoxedRep (typeAsLevity levity)
    ("TupleRep", [fields]) -> TupleRep (map runtimeRepFromType (promotedListItems fields))
    ("SumRep", [fields]) -> SumRep (map runtimeRepFromType (promotedListItems fields))
    ("VecRep", [count, element]) -> VecRep (typeAsVecCount count) (typeAsVecElem element)
    (constructor, []) -> fromMaybe liftedRuntimeRep (runtimeRepConstructorByName constructor)
    _ -> liftedRuntimeRep

typeAsLevity :: TcType -> Levity
typeAsLevity (TcTyCon tyCon [])
  | bareName (tyConName tyCon) == "Unlifted" = Unlifted
typeAsLevity (TcBuiltinTyCon name _ [])
  | bareName name == "Unlifted" = Unlifted
typeAsLevity _ = Lifted

typeAsVecCount :: TcType -> VecCount
typeAsVecCount (TcTyCon tyCon []) =
  case bareName (tyConName tyCon) of
    "Vec2" -> Vec2
    "Vec4" -> Vec4
    "Vec8" -> Vec8
    "Vec16" -> Vec16
    "Vec32" -> Vec32
    "Vec64" -> Vec64
    _ -> Vec2
typeAsVecCount (TcBuiltinTyCon name _ []) = typeAsVecCountName (bareName name)
typeAsVecCount _ = Vec2

typeAsVecCountName :: Text -> VecCount
typeAsVecCountName name =
  case name of
    "Vec2" -> Vec2
    "Vec4" -> Vec4
    "Vec8" -> Vec8
    "Vec16" -> Vec16
    "Vec32" -> Vec32
    "Vec64" -> Vec64
    _ -> Vec2

typeAsVecElem :: TcType -> VecElem
typeAsVecElem (TcTyCon tyCon []) =
  fromMaybe Int8ElemRep (lookup (bareName (tyConName tyCon)) vecElemNames)
typeAsVecElem (TcBuiltinTyCon name _ []) =
  fromMaybe Int8ElemRep (lookup (bareName name) vecElemNames)
typeAsVecElem _ = Int8ElemRep

vecElemNames :: [(Text, VecElem)]
vecElemNames =
  [ ("Int8ElemRep", Int8ElemRep),
    ("Int16ElemRep", Int16ElemRep),
    ("Int32ElemRep", Int32ElemRep),
    ("Int64ElemRep", Int64ElemRep),
    ("Word8ElemRep", Word8ElemRep),
    ("Word16ElemRep", Word16ElemRep),
    ("Word32ElemRep", Word32ElemRep),
    ("Word64ElemRep", Word64ElemRep),
    ("FloatElemRep", FloatElemRep),
    ("DoubleElemRep", DoubleElemRep)
  ]

promotedListItems :: TcType -> [TcType]
promotedListItems (TcTyCon tyCon items)
  | bareName (tyConName tyCon) == "[]" = items
promotedListItems (TcBuiltinTyCon name _ items)
  | bareName name == "[]" = items
promotedListItems _ = []

runtimeRepConstructorByName :: Text -> Maybe RuntimeRep
runtimeRepConstructorByName name =
  lookup
    (bareName name)
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

bareName :: Text -> Text
bareName = T.dropWhile (== '\'')

-- | A predicate (primitive constraint).
--
-- OutsideIn(X) is parameterized over the constraint domain. For our
-- Haskell-like language, the domain includes class predicates and
-- equality predicates.
data Pred
  = -- | Class predicate, e.g. @Eq a@.
    ClassPred !Text ![TcType]
  | -- | Type equality predicate, e.g. @a ~ Int@.
    EqPred !TcType !TcType
  deriving (Eq, Show, Read)

-- | The nesting level of implication constraints.
--
-- Meta-variables created at level N cannot be unified by the solver
-- when processing constraints at level N+1, unless the solution
-- involves only types visible at level N. This enforces the
-- OutsideIn discipline.
newtype TcLevel = TcLevel Int
  deriving (Eq, Ord, Show)

-- | The top level (outermost scope).
topTcLevel :: TcLevel
topTcLevel = TcLevel 0

-- | Enter a deeper implication level.
pushLevel :: TcLevel -> TcLevel
pushLevel (TcLevel n) = TcLevel (n + 1)
