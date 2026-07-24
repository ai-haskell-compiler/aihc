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
    tyConKind,
    mkTyCon,
    Kind (KTYPE, KConstraint, KRuntimeRep, KLevity, KVecCount, KVecElem, KFun, KMeta, KType),
    RuntimeRep (..),
    Levity (..),
    VecCount (..),
    VecElem (..),
    TypeScheme (..),
    liftedRuntimeRep,
    liftedTypeKind,
    typeKind,
    runtimeRepOfType,
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

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

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

-- | Type constructor. Every constructor carries its fully applied kind so
-- downstream phases can recover the kind of any 'TcType' without consulting
-- the type-checker environment.
data TyCon = TyConInternal !Text !Int !Kind
  deriving (Show, Read)

instance Eq TyCon where
  left == right =
    (tyConName left, tyConArity left) == (tyConName right, tyConArity right)

instance Ord TyCon where
  compare left right =
    compare (tyConName left, tyConArity left) (tyConName right, tyConArity right)

pattern TyCon :: Text -> Int -> TyCon
pattern TyCon {tyConName, tyConArity} <- TyConInternal tyConName tyConArity _
  where
    TyCon name arity = TyConInternal name arity (wiredInTyConKind name arity)

{-# COMPLETE TyCon #-}

tyConKind :: TyCon -> Kind
tyConKind (TyConInternal _ _ kind) = kind

mkTyCon :: Text -> Int -> Kind -> TyCon
mkTyCon name arity inferredKind =
  TyConInternal name arity (fromMaybe inferredKind (fixedTyConKind name))

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
    other -> Left ("type does not have a runtime representation: " <> show other)

typeKind :: TcType -> Kind
typeKind ty =
  case ty of
    TcTyVar tyVar -> tvKind tyVar
    TcMetaTv {} -> liftedTypeKind
    TcTyCon tyCon args
      | isUnboxedTupleTyCon (tyConName tyCon) (tyConArity tyCon),
        length args == tyConArity tyCon ->
          KTYPE (TupleRep (map runtimeRepOrLifted args))
      | isUnboxedSumTyCon (tyConName tyCon) (tyConArity tyCon),
        length args == tyConArity tyCon ->
          KTYPE (SumRep (map runtimeRepOrLifted args))
      | otherwise -> applyKindArguments (tyConKind tyCon) (length args)
    TcFunTy {} -> liftedTypeKind
    TcForAllTy _ body -> typeKind body
    TcQualTy _ body -> typeKind body
    TcAppTy function _ -> applyKindArguments (typeKind function) 1
  where
    runtimeRepOrLifted argument =
      case runtimeRepOfType argument of
        Right runtimeRep -> runtimeRep
        Left _ -> liftedRuntimeRep

applyKindArguments :: Kind -> Int -> Kind
applyKindArguments kind count
  | count <= 0 = kind
applyKindArguments (KFun _ result) count = applyKindArguments result (count - 1)
applyKindArguments kind _ = kind

isUnboxedTupleTyCon :: Text -> Int -> Bool
isUnboxedTupleTyCon name arity =
  arity /= 1
    && name == "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"

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
    "ByteArray#" -> Just (KTYPE (BoxedRep Unlifted))
    "MutableByteArray#" -> Just (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted)))
    "MutVar#" -> Just (KFun liftedTypeKind (KFun liftedTypeKind (KTYPE (BoxedRep Unlifted))))
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
