{-# language AllowAmbiguousTypes       #-}
{-# language ConstraintKinds           #-}
{-# language DataKinds                 #-}
{-# language DefaultSignatures         #-}
{-# language ExistentialQuantification #-}
{-# language FlexibleContexts          #-}
{-# language FlexibleInstances         #-}
{-# language GADTs                     #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MultiParamTypeClasses     #-}
{-# language PolyKinds                 #-}
{-# language QuantifiedConstraints     #-}
{-# language ScopedTypeVariables       #-}
{-# language StandaloneDeriving        #-}
{-# language TypeApplications          #-}
{-# language TypeFamilies              #-}
{-# language TypeOperators             #-}
{-# language UndecidableInstances      #-}
{-# language UndecidableSuperClasses   #-}
-- | Main module of @kind-generics@. Please refer to the @README@ file for documentation on how to use this package.
module Generics.Kind (
  -- * Generic representation types
  (:+:)(..), (:*:)(..), V1, U1(..), M1(..)
, Field(..), (:=>:)(..), Exists(..)
  -- ** Atoms for 'Field'
, Atom(..), TyVar(..), (:$:), (:~:), (:~~:)
, Var0, Var1, Var2, Var3, Var4, Var5, Var6, Var7, Var8, Var9
  -- ** Lists of types
, LoT(..), (:@@:), LoT1, LoT2, TyEnv(..)
  -- * Generic type classes
, GenericK(..)
, GenericF, fromF, toF
, GenericN, fromN, toN
  -- * Getting more instances almost for free
, fromRepK, toRepK, SubstRep, SubstRep', SubstAtom
  -- * Bridging with "GHC.Generics"
, Conv(..)
  -- * Re-exported from 'Data.PolyKinded.Atom'
  -- ** Interpretation of atoms
, Interpret, InterpretVar, Satisfies, ContainsTyVar
  -- ** Auxiliary data types for interpretation
, ForAllI(..), SuchThatI(..)
, WrappedI(..), toWrappedI, fromWrappedI
) where

import           Data.Kind
import           Data.PolyKinded
import           Data.PolyKinded.Atom
import           GHC.Generics.Extra   hiding (SuchThat, (:=>:))
import qualified GHC.Generics.Extra   as GG
-- import GHC.Exts

-- | Fields: used to represent each of the (visible) arguments to a constructor.
-- Replaces the 'K1' type from 'GHC.Generics'. The type of the field is
-- represented by an 'Atom' from 'Data.PolyKinded.Atom'.
--
-- > instance GenericK [] (a :&&: LoT0) where
-- >   type RepK [] = Field Var0 :*: Field ([] :$: Var0)
newtype Field (t :: Atom d Type) (x :: LoT d) where
  -- Field :: forall (r :: RuntimeRep) (k :: TYPE r) (d :: Type). Atom d k -> LoT d -> Type where
  -- Until https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0013-unlifted-newtypes.rst
  -- and https://ghc.haskell.org/trac/ghc/ticket/14917
  -- are implemented, we are restricted to the Type kind
  Field :: { unField :: Interpret t x } -> Field t x
deriving instance Eq (Interpret t x) => Eq (Field t x)
deriving instance Ord (Interpret t x) => Ord (Field t x)
deriving instance Semigroup (Interpret t x) => Semigroup (Field t x)
deriving instance Monoid (Interpret t x) => Monoid (Field t x)
deriving instance Show (Interpret t x) => Show (Field t x)

-- | Constraints: used to represent constraints in a constructor.
-- Replaces the '(:=>:)' type from "GHC.Generics.Extra".
--
-- > data Showable a = Show a => a -> X a
-- >
-- > instance GenericK Showable (a :&&: LoT0) where
-- >   type RepK Showable = (Show :$: a) :=>: (Field Var0)
data (:=>:) (c :: Atom d Constraint) (f :: LoT d -> Type) (x :: LoT d) where
  SuchThat :: Interpret c x => f x -> (c :=>: f) x
deriving instance (Interpret c x => Eq (f x)) => Eq ((c :=>: f) x)
deriving instance (Interpret c x => Ord (f x)) => Ord ((c :=>: f) x)
deriving instance (Interpret c x => Show (f x)) => Show ((c :=>: f) x)

-- | Existentials: a representation of the form @E f@ describes
-- a constructor whose inner type is represented by @f@, and where
-- the type variable at index 0, 'Var0', is existentially quantified.
--
-- > data E where
-- >  E :: t -> Exists
-- >
-- > instance GenericK E LoT0 where
-- >   type RepK E = Exists Type (Field Var0)
data Exists k (f :: LoT (k -> d) -> Type) (x :: LoT d) where
  Exists :: forall k (t :: k) d (f :: LoT (k -> d) -> Type) (x :: LoT d)
          .{ unExists :: f (t ':&&: x) } -> Exists k f x
deriving instance (forall t. Show (f (t ':&&: x))) => Show (Exists k f x)

-- THE TYPE CLASS

-- | Representable types of any kind. Examples:
--
-- > instance GenericK Int
-- > instance GenericK []
-- > instance GenericK Either
-- > instance GenericK (Either a)
-- > instance GenericK (Either a b)
class GenericK (f :: k) where
  type family RepK f :: LoT k -> Type

  -- | Convert the data type to its representation.
  fromK :: f :@@: x -> RepK f x
  default
    fromK :: (Generic (f :@@: x), Conv (Rep (f :@@: x)) (RepK f) x)
          => f :@@: x -> RepK f x
  fromK = toKindGenerics . from

  -- | Convert from a representation to its corresponding data type.
  toK   :: RepK f x -> f :@@: x
  default
    toK :: (Generic (f :@@: x), Conv (Rep (f :@@: x)) (RepK f) x)
        => RepK f x -> f :@@: x
  toK = to . toGhcGenerics

type GenericF t f x = (GenericK f, x ~ SplitF t f, t ~ (f :@@: x))
fromF :: forall f t x. GenericF t f x => t -> RepK f x
fromF = fromK @_ @f
toF :: forall f t x. GenericF t f x => RepK f x -> t
toF = toK @_ @f

type GenericN n t f x = (GenericK f, 'TyEnv f x ~ SplitN n t, t ~ (f :@@: x))
fromN :: forall n t f x. GenericN n t f x => t -> RepK f x
fromN = fromK @_ @f
toN :: forall n t f x. GenericN n t f x => RepK f x -> t
toN = toK @_ @f

-- CONVERSION BETWEEN FEWER AND MORE ARGUMENTS

fromRepK :: forall f x xs. (GenericK f, SubstRep' (RepK f) x xs)
         => f x :@@: xs -> SubstRep (RepK f) x xs
fromRepK = substRep . fromK @_ @f @(x ':&&: xs)

toRepK :: forall f x xs. (GenericK f, SubstRep' (RepK f) x xs)
       => SubstRep (RepK f) x xs -> f x :@@: xs
toRepK = toK @_ @f @(x ':&&: xs) . unsubstRep

class SubstRep' (f :: LoT (t -> k) -> Type) (x :: t) (xs :: LoT k) where
  type family SubstRep f x :: LoT k -> Type
  substRep   :: f (x ':&&: xs) -> SubstRep f x xs
  unsubstRep :: SubstRep f x xs -> f (x ':&&: xs)

instance SubstRep' U1 x xs where
  type SubstRep U1 x = U1
  substRep   U1 = U1
  unsubstRep U1 = U1

instance (SubstRep' f x xs, SubstRep' g x xs) => SubstRep' (f :+: g) x xs where
  type SubstRep (f :+: g)  x = SubstRep f x :+: SubstRep g x
  substRep   (L1 x) = L1 (substRep   x)
  substRep   (R1 x) = R1 (substRep   x)
  unsubstRep (L1 x) = L1 (unsubstRep x)
  unsubstRep (R1 x) = R1 (unsubstRep x)

instance (SubstRep' f x xs, SubstRep' g x xs) => SubstRep' (f :*: g) x xs where
  type SubstRep (f :*: g) x = SubstRep f x :*: SubstRep g x
  substRep   (x :*: y) = substRep   x :*: substRep   y
  unsubstRep (x :*: y) = unsubstRep x :*: unsubstRep y

instance SubstRep' f x xs => SubstRep' (M1 i c f) x xs where
  type SubstRep (M1 i c f) x = M1 i c (SubstRep f x)
  substRep   (M1 x) = M1 (substRep   x)
  unsubstRep (M1 x) = M1 (unsubstRep x)

-- The context says that @Interpret (SubstAtom c x) xs@
-- and @Interpret c (x ':&&: xs)@ are equivalent.
-- But because @Interpret@ is a type family, and the right-hand side of
-- a quantified constraint must be a class, we must use "class synonyms"
-- @InterpretCons@ and @InterpretSubst@.
instance ( Interpret (SubstAtom c x) xs => InterpretCons c x xs
         , Interpret c (x ':&&: xs) => InterpretSubst c x xs
         , SubstRep' f x xs )
         => SubstRep' (c :=>: f) x xs where
  type SubstRep (c :=>: f) x = SubstAtom c x :=>: SubstRep f x
  substRep   (SuchThat x) = SuchThat (substRep   x) :: InterpretSubst c x xs => SubstRep (c :=>: f) x xs
  unsubstRep (SuchThat x) = SuchThat (unsubstRep x) :: InterpretCons  c x xs => (c :=>: f) (x ':&&: xs)

class    Interpret c (x ':&&: xs) => InterpretCons c x xs
instance Interpret c (x ':&&: xs) => InterpretCons c x xs

class    Interpret (SubstAtom c x) xs => InterpretSubst c x xs
instance Interpret (SubstAtom c x) xs => InterpretSubst c x xs

instance (Interpret (SubstAtom t x) xs ~ Interpret t (x ':&&: xs))
         => SubstRep' (Field t) x xs where
  type SubstRep (Field t) x = Field (SubstAtom t x)
  substRep   (Field x) = Field x
  unsubstRep (Field x) = Field x

type family SubstAtom (f :: Atom (t -> k) d) (x :: t) :: Atom k d where
  SubstAtom ('Var 'VZ)     x = 'Kon x
  SubstAtom ('Var ('VS v)) x = 'Var v
  SubstAtom ('Kon t)       x = 'Kon t
  SubstAtom (t1 ':@: t2)   x = SubstAtom t1 x ':@: SubstAtom t2 x
  SubstAtom (t1 ':&: t2)   x = SubstAtom t1 x ':&: SubstAtom t2 x

-- CONVERSION BETWEEN GHC.GENERICS AND KIND-GENERICS

-- | Bridges a representation of a data type using the combinators
-- in "GHC.Generics" with a representation using this module.
-- You are never expected to manipulate this type class directly,
-- it is part of the deriving mechanism for 'GenericK'.
class Conv (gg :: Type -> Type) (kg :: LoT d -> Type) (tys :: LoT d) where
  toGhcGenerics  :: kg tys -> gg a
  toKindGenerics :: gg a -> kg tys

instance Conv U1 U1 tys where
  toGhcGenerics  U1 = U1
  toKindGenerics U1 = U1

instance (Conv f f' tys, Conv g g' tys) => Conv (f :+: g) (f' :+: g') tys where
  toGhcGenerics  (L1 x) = L1 (toGhcGenerics  x)
  toGhcGenerics  (R1 x) = R1 (toGhcGenerics  x)
  toKindGenerics (L1 x) = L1 (toKindGenerics x)
  toKindGenerics (R1 x) = R1 (toKindGenerics x)

instance (Conv f f' tys, Conv g g' tys) => Conv (f :*: g) (f' :*: g') tys where
  toGhcGenerics  (x :*: y) = toGhcGenerics  x :*: toGhcGenerics  y
  toKindGenerics (x :*: y) = toKindGenerics x :*: toKindGenerics y

instance {-# OVERLAPPABLE #-} (Conv f f' tys) => Conv (M1 i c f) f' tys where
  toGhcGenerics  x = M1 (toGhcGenerics  x)
  toKindGenerics (M1 x) = toKindGenerics x

instance {-# OVERLAPS #-} (Conv f f' tys) => Conv (M1 i c f) (M1 i c f') tys where
  toGhcGenerics  (M1 x) = M1 (toGhcGenerics  x)
  toKindGenerics (M1 x) = M1 (toKindGenerics x)

instance (k ~ Interpret t tys, Conv f f' tys)
         => Conv (k GG.:=>: f) (t :=>: f') tys where
  toGhcGenerics (SuchThat x) = GG.SuchThat (toGhcGenerics x)
  toKindGenerics (GG.SuchThat x) = SuchThat (toKindGenerics x)

instance (k ~ Interpret t tys) => Conv (K1 p k) (Field t) tys where
  toGhcGenerics  (Field x)  = K1 x
  toKindGenerics (K1 x) = Field x
