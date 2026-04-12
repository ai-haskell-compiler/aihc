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
    TyVarId (..),

    -- * Types
    TcType (..),
    TyCon (..),
    TypeScheme (..),

    -- * Predicates
    Pred (..),

    -- * Tc level
    TcLevel (..),
    topTcLevel,
    pushLevel,
  )
where

import Data.Text (Text)

-- | Unique identifier for type variables and evidence variables.
newtype Unique = Unique Int
  deriving (Eq, Ord, Show)

-- | A type variable identifier, carrying both a human-readable name and
-- a unique tag for alpha-equivalence.
data TyVarId = TyVarId
  { tvName :: !Text,
    tvUnique :: !Unique
  }
  deriving (Show)

instance Eq TyVarId where
  a == b = tvUnique a == tvUnique b

instance Ord TyVarId where
  compare a b = compare (tvUnique a) (tvUnique b)

-- | Type constructor.
data TyCon = TyCon
  { tyConName :: !Text,
    tyConArity :: !Int
  }
  deriving (Eq, Ord, Show)

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
  deriving (Eq, Show)

-- | A type scheme: universally quantified type with constraints.
--
-- @ForAll [a, b] [Eq a] (a -> b -> Bool)@
-- represents @forall a b. Eq a => a -> b -> Bool@.
data TypeScheme = ForAll ![TyVarId] ![Pred] !TcType
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
