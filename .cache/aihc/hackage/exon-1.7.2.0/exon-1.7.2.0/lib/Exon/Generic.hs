{-# options_haddock prune #-}

-- | Description: Internal
module Exon.Generic where

import GHC.Generics (C1, D1, K1 (K1), M1 (M1), Meta (MetaData), Rep, S1, from, to)

type family PrepRep (rep :: Type -> Type) :: Maybe Type where
  PrepRep (D1 ('MetaData _ _ _ 'True) (C1 _ (S1 _ (K1 _ w)))) = 'Just w
  PrepRep (D1 ('MetaData _ _ _ 'True) (C1 _ (K1 _ w))) = 'Just w

class RepIsNewtype (rep :: Maybe Type) (wrapped :: Maybe Type) | rep -> wrapped
instance {-# incoherent #-} wrapped ~ 'Nothing => RepIsNewtype rep wrapped
instance wrapped ~ 'Just w => RepIsNewtype ('Just w) wrapped

class IsNewtype w (wrapped :: Maybe Type) | w -> wrapped
instance RepIsNewtype (PrepRep (Rep w)) wrapped => IsNewtype w wrapped

type NewtypeRep :: Symbol -> Symbol -> Symbol -> Meta -> Type -> Type -> Type -> Type
type NewtypeRep n m p c i w =
  D1 ('MetaData n m p 'True) (S1 c (K1 i w))

class UnwrapField (rep :: Type -> Type) (w :: Type) | rep -> w where
  unwrapField :: rep x -> w

-- | Record selector
instance UnwrapField (S1 s (K1 i w)) w where
  unwrapField (M1 (K1 w)) = w

-- | Plain field
instance UnwrapField (K1 i w) w where
  unwrapField (K1 w) = w

class Unwrap (a :: Type) (w :: Type) | a -> w where
  unwrap :: a -> w

-- | Constructor ('C1') in a data type ('D1') that's a newtype (@'True@)
instance (
    Generic a,
    Rep a ~ D1 ('MetaData n m p 'True) (C1 c field),
    UnwrapField field w
  ) => Unwrap a w where
    unwrap (from -> M1 (M1 field)) = unwrapField field

class WrapField (rep :: Type -> Type) (w :: Type) | rep -> w where
  wrapField :: w -> rep x

-- | Record selector
instance WrapField (S1 s (K1 i w)) w where
  wrapField = M1 . K1

-- | Plain field
instance WrapField (K1 i w) w where
  wrapField = K1

class Wrap (a :: Type) (w :: Type) | a -> w where
  wrap :: w -> a

instance (
    Generic a,
    Rep a ~ D1 ('MetaData n m p 'True) (C1 c field),
    WrapField field w
  ) => Wrap a w where
    wrap w = to (M1 (M1 (wrapField w)))

type OverNt (a :: Type) (w :: Type) =
  (Wrap a w, Unwrap a w)
