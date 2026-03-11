{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Internal Syntax.

module Internal where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

import Lens

-- | Definition names are strings.

type Id = String

-- | Variables are de Bruijn indices.

newtype Index = Index { dbIndex :: Int }
  deriving (Eq, Ord, Show, Enum, Num)

-- | Size expressions.

type Size  = Term
type Level = Size

-- | Terms/types

type Type = Term

data Term
  = -- | Universe with level.
    Type Level
  | -- | Type of sizes (internal use only).
    Size
  | -- | Sized natural number type.
    Nat Size
  | -- | Zero constructor, or zero size (then @Size@ is ignored).
    Zero (Arg Size)
  | -- | Successor constructor, or successor size (then @Size@ is ignored).
    Suc (Arg Size) Term
  | -- | Infinity size.
    Infty
  | -- ^ (Dependent) function type.
    Pi (Dom Type) (Abs Term)
  | -- ^ Lambda abstraction
    Lam ArgInfo (Abs Term)
  | -- ^ Variable.
    Var Index
  | -- ^ Function call.
    Def Id
  | -- ^ Application/eliminiation.
    App Term Elim
  deriving (Eq, Ord, Show)

-- | Eliminations.

type Elims = [ Elim ]
type Elim  = Elim' Term

data Elim' a
  = -- | Function application.
    Apply (Arg a)
  | -- | Case distinction
    Case
    { caseReturn :: a -- ^ @T : Nat (b + 1) -> Setω@
    , caseZero   :: a -- ^ @tz : T zero@
    , caseTySuc  :: a -- ^ Type of @caseSuc@.  Stored here for convenience, must be
                      --        @(t : Nat b) -> T (suc t)@
    , caseSuc    :: a -- ^ @ts : (t : Nat b) -> T (suc t)@
    }
  | -- | Recursion
    Fix
    { fixReturn :: a
      -- ^ @T : ..(i : Size) -> Nat i -> Setω@
    , fixTyBody :: a
      -- ^ Type of @fixBody@.  Stored here for convenience, must be
      -- @.(i : Size) (f : (x : Nat i) -> T i x) (x : Nat (i + 1)) -> T (i + 1) x@.
    , fixBody   :: a
      -- ^ @t : .(i : Size) (f : (x : Nat i) -> T i x) (x : Nat (i + 1)) -> T (i + 1) x@
    }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Abstraction.

type AbsName = String

data Abs a
  = -- | Actual abstraction (body contains one more index).
    Abs   { absName :: AbsName, absBody :: a }
  | -- | No abstraction (argument will be ignored).
    NoAbs { absName :: AbsName, absBody :: a }
  deriving (Eq, Ord, Show)

-- | Function domain decoration.

data Dom a = Dom { _domInfo :: ArgInfo, unDom :: a }
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | Argument decoration.

data Arg a = Arg { argInfo :: ArgInfo, unArg :: a }
  deriving (Ord, Show, Functor, Foldable, Traversable)

instance Eq a => Eq (Arg a) where
   Arg Irrelevant _ == Arg Irrelevant _ = True
   Arg r a == Arg r' a' = r == r' && a == a'

type ArgInfo = Relevance

-- | Relevance lattice (order matters)
data Relevance
  = Relevant
  | ShapeIrr
  | Irrelevant
  deriving (Eq, Ord, Show)

makeLens ''Dom

-- * Smart constructor.

zero :: Size -> Term
zero = Zero . Arg Irrelevant

suc :: Size -> Term -> Term
suc = Suc . Arg Irrelevant

defaultArg :: a -> Arg a
defaultArg = Arg Relevant

defaultDom :: a -> Dom a
defaultDom = Dom Relevant

-- | Zero size.

sZero :: Term
sZero = zero Infty

-- | Successor size.

sSuc  :: Term -> Term
sSuc  = suc Infty

-- | Size increment.

sPlus :: Term -> Integer -> Term
sPlus t n = iterate sSuc t !! fromInteger n

-- | @fixKind = ..(i : Size) -> Nat i -> Setω@

fixKind :: Type
fixKind =
  Pi (Dom ShapeIrr Size) $ Abs "i" $
    Pi (Dom Relevant (Nat $ Var 0)) $ Abs "x" $
      Type Infty
