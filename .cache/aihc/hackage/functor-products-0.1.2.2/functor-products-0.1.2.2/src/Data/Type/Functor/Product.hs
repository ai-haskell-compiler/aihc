{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Type.Functor.Product
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Generalized functor products based on lifted 'Foldable's.
--
-- For example, @'Rec' f '[a,b,c]@ from /vinyl/ contains an @f a@, @f b@,
-- and @f c@.
--
-- @'PMaybe' f ('Just a)@ contains an @f a@ and @'PMaybe' f 'Nothing@
-- contains nothing.
--
-- Also provide data types for "indexing" into each foldable.
module Data.Type.Functor.Product (
  -- * Classes
  FProd (..),
  Shape,
  PureProd (..),
  pureShape,
  PureProdC (..),
  ReifyConstraintProd (..),
  AllConstrainedProd,

  -- ** Functions
  indexProd,
  mapProd,
  foldMapProd,
  hmap,
  zipProd,
  imapProd,
  itraverseProd,
  ifoldMapProd,
  generateProd,
  generateProdA,
  selectProd,
  indices,
  eqProd,
  compareProd,

  -- *** Over singletons
  indexSing,
  singShape,
  foldMapSing,
  ifoldMapSing,

  -- * Instances
  Rec (..),
  Index (..),
  withPureProdList,
  PMaybe (..),
  IJust (..),
  PEither (..),
  IRight (..),
  NERec (..),
  NEIndex (..),
  withPureProdNE,
  PTup (..),
  ISnd (..),
  PIdentity (..),
  IIdentity (..),
  sameIndexVal,
  sameNEIndexVal,

  -- ** Interfacing with vinyl
  rElemIndex,
  indexRElem,
  toCoRec,

  -- * Singletons
  SIndex (..),
  SIJust (..),
  SIRight (..),
  SNEIndex (..),
  SISnd (..),
  SIIdentity (..),

  -- * Defunctionalization symbols
  ElemSym0,
  ElemSym1,
  ElemSym2,
  ProdSym0,
  ProdSym1,
  ProdSym2,
) where

import Control.Applicative
import Data.Either.Singletons
import Data.Foldable.Singletons hiding (Elem, ElemSym0, ElemSym1, ElemSym2)
import Data.Function.Singletons
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Identity.Singletons
import Data.Functor.Singletons
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty.Singletons as NE
import Data.List.Singletons hiding (Elem, ElemSym0, ElemSym1, ElemSym2)
import Data.Maybe
import Data.Maybe.Singletons
import Data.Semigroup
import Data.Singletons
import Data.Singletons.Decide
import qualified Data.Text as T
import Data.Tuple.Singletons
import Data.Vinyl hiding ((:~:))
import Data.Vinyl.CoRec
import qualified Data.Vinyl.Functor as V
import qualified Data.Vinyl.TypeLevel as V
import GHC.Generics ((:*:) (..))
import Lens.Micro hiding ((%~))
import Lens.Micro.Extras
import Text.Show.Singletons
import Unsafe.Coerce

fmapIdent :: Fmap IdSym0 as :~: as
fmapIdent = unsafeCoerce Refl

-- | Simply witness the /shape/ of an argument (ie, @'Shape' [] as@
-- witnesses the length of @as@, and @'Shape' Maybe as@ witnesses whether
-- or not @as@ is 'Just' or 'Nothing').
type Shape f = (Prod f Proxy :: f k -> Type)

-- | Unify different functor products over a Foldable @f@.
class (PFunctor f, SFunctor f, PFoldable f, SFoldable f) => FProd (f :: Type -> Type) where
  type Elem f = (i :: f k -> k -> Type) | i -> f
  type Prod f = (p :: (k -> Type) -> f k -> Type) | p -> f

  -- | You can convert a singleton of a foldable value into a foldable product of
  -- singletons.  This essentially "breaks up" the singleton into its
  -- individual items.  Should be an inverse with 'prodSing'.
  singProd :: Sing as -> Prod f Sing as

  -- | Collect a collection of singletons back into a single singleton.
  -- Should be an inverse with 'singProd'.
  prodSing :: Prod f Sing as -> Sing as

  -- | Pair up each item in a foldable product with its index.
  withIndices ::
    Prod f g as ->
    Prod f (Elem f as :*: g) as

  -- | Traverse a foldable functor product with a RankN applicative function,
  -- mapping over each value and sequencing the effects.
  --
  -- This is the generalization of 'rtraverse'.
  traverseProd ::
    forall g h as m.
    Applicative m =>
    (forall a. g a -> m (h a)) ->
    Prod f g as ->
    m (Prod f h as)
  traverseProd = case fmapIdent @as of
    Refl -> htraverse (sing @IdSym0)

  -- | Zip together two foldable functor products with a Rank-N function.
  zipWithProd ::
    (forall a. g a -> h a -> j a) ->
    Prod f g as ->
    Prod f h as ->
    Prod f j as
  zipWithProd f xs ys = imapProd (\i x -> f x (indexProd i ys)) xs

  -- | Traverse a foldable functor product with a type-changing function.
  htraverse ::
    Applicative m =>
    Sing ff ->
    (forall a. g a -> m (h (ff @@ a))) ->
    Prod f g as ->
    m (Prod f h (Fmap ff as))

  -- | A 'Lens' into an item in a foldable functor product, given its
  -- index.
  --
  -- This roughly generalizes 'rlens'.
  ixProd ::
    Elem f as a ->
    Lens' (Prod f g as) (g a)

  -- | Fold a functor product into a 'Rec'.
  toRec :: Prod f g as -> Rec g (ToList as)

  -- | Get a 'PureProd' instance from a foldable functor product
  -- providing its shape.
  withPureProd ::
    Prod f g as ->
    (PureProd f as => r) ->
    r

-- | Create @'Prod' f@ if you can give a @g a@ for every slot.
class PureProd f as where
  pureProd :: (forall a. g a) -> Prod f g as

-- | Create @'Prod' f@ if you can give a @g a@ for every slot, given some
-- constraint.
class PureProdC f c as where
  pureProdC :: (forall a. c a => g a) -> Prod f g as

-- | Pair up each item in a @'Prod' f@ with a witness that @f a@ satisfies
-- some constraint.
class ReifyConstraintProd f c g as where
  reifyConstraintProd :: Prod f g as -> Prod f (Dict c V.:. g) as

data ElemSym0 (f :: Type -> Type) :: f k ~> k ~> Type
data ElemSym1 (f :: Type -> Type) :: f k -> k ~> Type
type ElemSym2 (f :: Type -> Type) (as :: f k) (a :: k) = Elem f as a

type instance Apply (ElemSym0 f) as = ElemSym1 f as
type instance Apply (ElemSym1 f as) a = Elem f as a

data ProdSym0 (f :: Type -> Type) :: (k -> Type) ~> f k ~> Type
data ProdSym1 (f :: Type -> Type) :: (k -> Type) -> f k ~> Type
type ProdSym2 (f :: Type -> Type) (g :: k -> Type) (as :: f k) = Prod f g as

type instance Apply (ProdSym0 f) g = ProdSym1 f g
type instance Apply (ProdSym1 f g) as = Prod f g as

-- | A convenient wrapper over 'V.AllConstrained' that works for any
-- Foldable @f@.
type AllConstrainedProd c as = V.AllConstrained c (ToList as)

-- | Create a 'Shape' given an instance of 'PureProd'.
pureShape :: PureProd f as => Shape f as
pureShape = pureProd Proxy

-- | Generate a 'Prod' of indices for an @as@.
indices :: (FProd f, PureProd f as) => Prod f (Elem f as) as
indices = imapProd const pureShape

-- | Convert a @'Sing' as@ into a @'Shape' f as@, witnessing the shape of
-- of @as@ but dropping all of its values.
singShape ::
  FProd f =>
  Sing as ->
  Shape f as
singShape = mapProd (const Proxy) . singProd

-- | Map a RankN function over a 'Prod'.  The generalization of 'rmap'.
mapProd ::
  FProd f =>
  (forall a. g a -> h a) ->
  Prod f g as ->
  Prod f h as
mapProd f = runIdentity . traverseProd (Identity . f)

-- | Zip together the values in two 'Prod's.
zipProd ::
  FProd f =>
  Prod f g as ->
  Prod f h as ->
  Prod f (g :*: h) as
zipProd = zipWithProd (:*:)

-- | Map a type-changing function over every item in a 'Prod'.
hmap ::
  FProd f =>
  Sing ff ->
  (forall a. g a -> h (ff @@ a)) ->
  Prod f g as ->
  Prod f h (Fmap ff as)
hmap ff f = runIdentity . htraverse ff (Identity . f)

-- | 'mapProd', but with access to the index at each element.
imapProd ::
  FProd f =>
  (forall a. Elem f as a -> g a -> h a) ->
  Prod f g as ->
  Prod f h as
imapProd f = mapProd (\(i :*: x) -> f i x) . withIndices

-- | Extract the item from the container witnessed by the 'Elem'
indexSing ::
  forall f as a.
  FProd f =>
  -- | Witness
  Elem f as a ->
  -- | Collection
  Sing as ->
  Sing a
indexSing i = indexProd i . singProd

-- | Use an 'Elem' to index a value out of a 'Prod'.
indexProd ::
  FProd f =>
  Elem f as a ->
  Prod f g as ->
  g a
indexProd i = view (ixProd i)

-- | 'traverseProd', but with access to the index at each element.
itraverseProd ::
  (FProd f, Applicative m) =>
  (forall a. Elem f as a -> g a -> m (h a)) ->
  Prod f g as ->
  m (Prod f h as)
itraverseProd f = traverseProd (\(i :*: x) -> f i x) . withIndices

-- | 'foldMapProd', but with access to the index at each element.
ifoldMapProd ::
  (FProd f, Monoid m) =>
  (forall a. Elem f as a -> g a -> m) ->
  Prod f g as ->
  m
ifoldMapProd f = getConst . itraverseProd (\i -> Const . f i)

-- | Map a RankN function over a 'Prod' and collect the results as
-- a 'Monoid'.
foldMapProd ::
  (FProd f, Monoid m) =>
  (forall a. g a -> m) ->
  Prod f g as ->
  m
foldMapProd f = ifoldMapProd (const f)

-- | 'foldMapSing' but with access to the index.
ifoldMapSing ::
  forall f k (as :: f k) m.
  (FProd f, Monoid m) =>
  (forall a. Elem f as a -> Sing a -> m) ->
  Sing as ->
  m
ifoldMapSing f = ifoldMapProd f . singProd

-- | A 'foldMap' over all items in a collection.
foldMapSing ::
  forall f k (as :: f k) m.
  (FProd f, Monoid m) =>
  (forall (a :: k). Sing a -> m) ->
  Sing as ->
  m
foldMapSing f = ifoldMapSing (const f)

-- | Rearrange or permute the items in a 'Prod' based on a 'Prod' of
-- indices.
--
-- @
-- 'selectProd' ('IS' 'IZ' ':&' IZ :& 'RNil') ("hi" :& "bye" :& "ok" :& RNil)
--      == "bye" :& "hi" :& RNil
-- @
selectProd ::
  FProd f =>
  Prod f (Elem f as) bs ->
  Prod f g as ->
  Prod f g bs
selectProd is xs = mapProd (`indexProd` xs) is

-- | An implementation of equality testing for all 'FProd' instances, as
-- long as each of the items are instances of 'Eq'.
eqProd ::
  (FProd f, ReifyConstraintProd f Eq g as) =>
  Prod f g as ->
  Prod f g as ->
  Bool
eqProd xs =
  getAll
    . foldMapProd getConst
    . zipWithProd
      (\(V.Compose (Dict x)) y -> Const (All (x == y)))
      (reifyConstraintProd @_ @Eq xs)

-- | An implementation of order comparison for all 'FProd' instances, as
-- long as each of the items are instances of 'Ord'.
compareProd ::
  (FProd f, ReifyConstraintProd f Ord g as) =>
  Prod f g as ->
  Prod f g as ->
  Ordering
compareProd xs =
  foldMapProd getConst
    . zipWithProd
      (\(V.Compose (Dict x)) y -> Const (compare x y))
      (reifyConstraintProd @_ @Ord xs)

-- | Construct a 'Prod' purely by providing a generating function for each
-- index.
generateProd ::
  (FProd f, PureProd f as) =>
  (forall a. Elem f as a -> g a) ->
  Prod f g as
generateProd f = mapProd f indices

-- | Construct a 'Prod' in an 'Applicative' context by providing
-- a generating function for each index.
generateProdA ::
  (FProd f, PureProd f as, Applicative m) =>
  (forall a. Elem f as a -> m (g a)) ->
  m (Prod f g as)
generateProdA f = traverseProd f indices

-- | Witness an item in a type-level list by providing its index.
--
-- The number of 'IS's correspond to the item's position in the list.
--
-- @
-- 'IZ'         :: 'Index' '[5,10,2] 5
-- 'IS' 'IZ'      :: 'Index' '[5,10,2] 10
-- 'IS' ('IS' 'IZ') :: 'Index' '[5,10,2] 2
-- @
data Index :: [k] -> k -> Type where
  IZ :: Index (a ': as) a
  IS :: Index bs a -> Index (b ': bs) a

deriving instance Show (Index as a)
deriving instance Eq (Index as a)
deriving instance Ord (Index as a)

-- | Kind-indexed singleton for 'Index'.
data SIndex (as :: [k]) (a :: k) :: Index as a -> Type where
  SIZ :: SIndex (a ': as) a 'IZ
  SIS :: SIndex bs a i -> SIndex (b ': bs) a ('IS i)

deriving instance Show (SIndex as a i)

#if __GLASGOW_HASKELL__ >= 908
type instance forall as a. Sing = SIndex as a :: Index as a -> Type
#else
type instance Sing = SIndex as a :: Index as a -> Type
#endif

instance SingI 'IZ where
  sing = SIZ

instance SingI i => SingI ('IS i) where
  sing = SIS sing

instance SingKind (Index as a) where
  type Demote (Index as a) = Index as a
  fromSing = \case
    SIZ -> IZ
    SIS j -> IS (fromSing j)
  toSing i = go i SomeSing
    where
      go :: Index bs b -> (forall i. SIndex bs b i -> r) -> r
      go = \case
        IZ -> ($ SIZ)
        IS j -> \f -> go j (f . SIS)

instance SDecide (Index as a) where
  (%~) = \case
    SIZ -> \case
      SIZ -> Proved Refl
      SIS _ -> Disproved $ \case {}
    SIS i' -> \case
      SIZ -> Disproved $ \case {}
      SIS j' -> case i' %~ j' of
        Proved Refl -> Proved Refl
        Disproved v -> Disproved $ \case Refl -> v Refl

instance FProd [] where
  type Elem [] = Index
  type Prod [] = Rec

  singProd = \case
    SNil -> RNil
    x `SCons` xs -> x :& singProd xs

  prodSing = \case
    RNil -> SNil
    x :& xs -> x `SCons` prodSing xs

  traverseProd ::
    forall g h m as.
    Applicative m =>
    (forall a. g a -> m (h a)) ->
    Prod [] g as ->
    m (Prod [] h as)
  traverseProd f = go
    where
      go :: Prod [] g bs -> m (Prod [] h bs)
      go = \case
        RNil -> pure RNil
        x :& xs -> (:&) <$> f x <*> go xs

  zipWithProd ::
    forall g h j as.
    () =>
    (forall a. g a -> h a -> j a) ->
    Prod [] g as ->
    Prod [] h as ->
    Prod [] j as
  zipWithProd f = go
    where
      go :: Prod [] g bs -> Prod [] h bs -> Prod [] j bs
      go = \case
        RNil -> \case
          RNil -> RNil
        x :& xs -> \case
          y :& ys -> f x y :& go xs ys

  htraverse ::
    forall ff g h as m.
    Applicative m =>
    Sing ff ->
    (forall a. g a -> m (h (ff @@ a))) ->
    Prod [] g as ->
    m (Prod [] h (Fmap ff as))
  htraverse _ f = go
    where
      go :: Prod [] g bs -> m (Prod [] h (Fmap ff bs))
      go = \case
        RNil -> pure RNil
        x :& xs -> (:&) <$> f x <*> go xs

  withIndices = \case
    RNil -> RNil
    x :& xs -> (IZ :*: x) :& mapProd (\(i :*: y) -> IS i :*: y) (withIndices xs)

  ixProd ::
    forall g as a.
    () =>
    Elem [] as a ->
    Lens' (Prod [] g as) (g a)
  ixProd i0 (f :: g a -> h (g a)) = go i0
    where
      go :: Elem [] bs a -> Prod [] g bs -> h (Prod [] g bs)
      go = \case
        IZ -> \case
          x :& xs -> (:& xs) <$> f x
        IS i -> \case
          x :& xs -> (x :&) <$> go i xs

  toRec = id

  withPureProd = withPureProdList

-- | A stronger version of 'withPureProd' for 'Rec', providing
-- a 'RecApplicative' instance as well.
withPureProdList ::
  Rec f as ->
  ((RecApplicative as, PureProd [] as) => r) ->
  r
withPureProdList = \case
  RNil -> id
  _ :& xs -> withPureProdList xs

instance RecApplicative as => PureProd [] as where
  pureProd = rpure

instance RPureConstrained c as => PureProdC [] c as where
  pureProdC = rpureConstrained @c

instance ReifyConstraint c f as => ReifyConstraintProd [] c f as where
  reifyConstraintProd = reifyConstraint @c

-- | Witness an item in a type-level 'Maybe' by proving the 'Maybe' is
-- 'Just'.
data IJust :: Maybe k -> k -> Type where
  IJust :: IJust ('Just a) a

deriving instance Show (IJust as a)
deriving instance Read (IJust ('Just a) a)
deriving instance Eq (IJust as a)
deriving instance Ord (IJust as a)

-- | Kind-indexed singleton for 'IJust'.
data SIJust (as :: Maybe k) (a :: k) :: IJust as a -> Type where
  SIJust :: SIJust ('Just a) a 'IJust

deriving instance Show (SIJust as a i)

#if __GLASGOW_HASKELL__ >= 908
type instance forall as a. Sing = SIJust as a :: IJust as a -> Type
#else
type instance Sing = SIJust as a :: IJust as a -> Type
#endif

instance SingI 'IJust where
  sing = SIJust

instance SingKind (IJust as a) where
  type Demote (IJust as a) = IJust as a
  fromSing SIJust = IJust
  toSing IJust = SomeSing SIJust

instance SDecide (IJust as a) where
  SIJust %~ SIJust = Proved Refl

-- | A @'PMaybe' f 'Nothing@ contains nothing, and a @'PMaybe' f ('Just a)@
-- contains an @f a@.
--
-- In practice this can be useful to write polymorphic
-- functions/abstractions that contain an argument that can be "turned off"
-- for different instances.
data PMaybe :: (k -> Type) -> Maybe k -> Type where
  PNothing :: PMaybe f 'Nothing
  PJust :: f a -> PMaybe f ('Just a)

instance ReifyConstraintProd Maybe Show f as => Show (PMaybe f as) where
  showsPrec d xs = case reifyConstraintProd @_ @Show xs of
    PNothing -> showString "PNothing"
    PJust (V.Compose (Dict x)) -> showsUnaryWith showsPrec "PJust" d x
instance ReifyConstraintProd Maybe Eq f as => Eq (PMaybe f as) where
  (==) = eqProd
instance (ReifyConstraintProd Maybe Eq f as, ReifyConstraintProd Maybe Ord f as) => Ord (PMaybe f as) where
  compare = compareProd

instance FProd Maybe where
  type Elem Maybe = IJust
  type Prod Maybe = PMaybe

  singProd = \case
    SNothing -> PNothing
    SJust x -> PJust x
  prodSing = \case
    PNothing -> SNothing
    PJust x -> SJust x
  withIndices = \case
    PNothing -> PNothing
    PJust x -> PJust (IJust :*: x)
  traverseProd f = \case
    PNothing -> pure PNothing
    PJust x -> PJust <$> f x
  zipWithProd f = \case
    PNothing -> \case
      PNothing -> PNothing
    PJust x -> \case
      PJust y -> PJust (f x y)
  htraverse _ f = \case
    PNothing -> pure PNothing
    PJust x -> PJust <$> f x
  ixProd = \case
    IJust -> \f -> \case
      PJust x -> PJust <$> f x
  toRec = \case
    PNothing -> RNil
    PJust x -> x :& RNil
  withPureProd = \case
    PNothing -> id
    PJust _ -> id

instance PureProd Maybe 'Nothing where
  pureProd _ = PNothing
instance PureProd Maybe ('Just a) where
  pureProd = PJust

instance PureProdC Maybe c 'Nothing where
  pureProdC _ = PNothing
instance c a => PureProdC Maybe c ('Just a) where
  pureProdC = PJust

instance ReifyConstraintProd Maybe c g 'Nothing where
  reifyConstraintProd PNothing = PNothing
instance c (g a) => ReifyConstraintProd Maybe c g ('Just a) where
  reifyConstraintProd (PJust x) = PJust (V.Compose (Dict x))

-- | Witness an item in a type-level @'Either' j@ by proving the 'Either'
-- is 'Right'.
data IRight :: Either j k -> k -> Type where
  IRight :: IRight ('Right a) a

deriving instance Show (IRight as a)
deriving instance Read (IRight ('Right a) a)
deriving instance Eq (IRight as a)
deriving instance Ord (IRight as a)

-- | Kind-indexed singleton for 'IRight'.
data SIRight (as :: Either j k) (a :: k) :: IRight as a -> Type where
  SIRight :: SIRight ('Right a) a 'IRight

deriving instance Show (SIRight as a i)

#if __GLASGOW_HASKELL__ >= 908
type instance forall as a. Sing = SIRight as a :: IRight as a -> Type
#else
type instance Sing = SIRight as a :: IRight as a -> Type
#endif

instance SingI 'IRight where
  sing = SIRight

instance SingKind (IRight as a) where
  type Demote (IRight as a) = IRight as a
  fromSing SIRight = IRight
  toSing IRight = SomeSing SIRight

instance SDecide (IRight as a) where
  SIRight %~ SIRight = Proved Refl

-- | A @'PEither' f ('Left e)@ contains @'Sing' e@, and a @'PMaybe' f ('Right a)@
-- contains an @f a@.
--
-- In practice this can be useful in the same situatinos that 'PMaybe' can,
-- but with an extra value in the case where value @f@ is "turned off" with
-- 'Left'.
data PEither :: (k -> Type) -> Either j k -> Type where
  PLeft :: Sing e -> PEither f ('Left e)
  PRight :: f a -> PEither f ('Right a)

instance (SShow j, ReifyConstraintProd (Either j) Show f as) => Show (PEither f as) where
  showsPrec d xs = case reifyConstraintProd @_ @Show xs of
    PLeft e -> showsUnaryWith go "PLeft" d e
    PRight (V.Compose (Dict x)) -> showsUnaryWith showsPrec "PRight" d x
    where
      go (fromIntegral -> FromSing i) x (T.pack -> FromSing str) = T.unpack . fromSing $ sShowsPrec i x str

instance FProd (Either j) where
  type Elem (Either j) = IRight
  type Prod (Either j) = PEither

  singProd = \case
    SLeft e -> PLeft e
    SRight x -> PRight x
  prodSing = \case
    PLeft e -> SLeft e
    PRight x -> SRight x
  withIndices = \case
    PLeft e -> PLeft e
    PRight x -> PRight (IRight :*: x)
  traverseProd f = \case
    PLeft e -> pure (PLeft e)
    PRight x -> PRight <$> f x
  zipWithProd f = \case
    PLeft e -> \case
      PLeft _ -> PLeft e
    PRight x -> \case
      PRight y -> PRight (f x y)
  htraverse _ f = \case
    PLeft e -> pure (PLeft e)
    PRight x -> PRight <$> f x
  ixProd = \case
    IRight -> \f -> \case
      PRight x -> PRight <$> f x
  toRec = \case
    PLeft _ -> RNil
    PRight x -> x :& RNil
  withPureProd = \case
    PLeft Sing -> id
    PRight _ -> id

instance SingI e => PureProd (Either j) ('Left e) where
  pureProd _ = PLeft sing
instance PureProd (Either j) ('Right a) where
  pureProd = PRight

instance SingI e => PureProdC (Either j) c ('Left e) where
  pureProdC _ = PLeft sing
instance c a => PureProdC (Either j) c ('Right a) where
  pureProdC = PRight

instance ReifyConstraintProd (Either j) c g ('Left e) where
  reifyConstraintProd (PLeft e) = PLeft e
instance c (g a) => ReifyConstraintProd (Either j) c g ('Right a) where
  reifyConstraintProd (PRight x) = PRight (V.Compose (Dict x))

-- | Witness an item in a type-level 'NonEmpty' by either indicating that
-- it is the "head", or by providing an index in the "tail".
data NEIndex :: NonEmpty k -> k -> Type where
  NEHead :: NEIndex (a ':| as) a
  NETail :: Index as a -> NEIndex (b ':| as) a

deriving instance Show (NEIndex as a)
deriving instance Eq (NEIndex as a)
deriving instance Ord (NEIndex as a)

-- | Kind-indexed singleton for 'NEIndex'.
data SNEIndex (as :: NonEmpty k) (a :: k) :: NEIndex as a -> Type where
  SNEHead :: SNEIndex (a ':| as) a 'NEHead
  SNETail :: SIndex as a i -> SNEIndex (b ':| as) a ('NETail i)

deriving instance Show (SNEIndex as a i)

#if __GLASGOW_HASKELL__ >= 908
type instance forall as a. Sing = SNEIndex as a :: NEIndex as a -> Type
#else
type instance Sing = SNEIndex as a :: NEIndex as a -> Type
#endif

instance SingI 'NEHead where
  sing = SNEHead

instance SingI i => SingI ('NETail i) where
  sing = SNETail sing

instance SingKind (NEIndex as a) where
  type Demote (NEIndex as a) = NEIndex as a
  fromSing = \case
    SNEHead -> NEHead
    SNETail i -> NETail $ fromSing i
  toSing = \case
    NEHead -> SomeSing SNEHead
    NETail i -> withSomeSing i $ SomeSing . SNETail

instance SDecide (NEIndex as a) where
  (%~) = \case
    SNEHead -> \case
      SNEHead -> Proved Refl
      SNETail _ -> Disproved $ \case {}
    SNETail i -> \case
      SNEHead -> Disproved $ \case {}
      SNETail j -> case i %~ j of
        Proved Refl -> Proved Refl
        Disproved v -> Disproved $ \case Refl -> v Refl

-- | A non-empty version of 'Rec'.
data NERec :: (k -> Type) -> NonEmpty k -> Type where
  (:&|) :: f a -> Rec f as -> NERec f (a ':| as)

infixr 5 :&|

deriving instance
  (Show (f a), RMap as, ReifyConstraint Show f as, RecordToList as) => Show (NERec f (a ':| as))
deriving instance (Eq (f a), Eq (Rec f as)) => Eq (NERec f (a ':| as))
deriving instance (Ord (f a), Ord (Rec f as)) => Ord (NERec f (a ':| as))

instance FProd NonEmpty where
  type Elem NonEmpty = NEIndex
  type Prod NonEmpty = NERec

  singProd (x NE.:%| xs) = x :&| singProd xs
  prodSing (x :&| xs) = x NE.:%| prodSing xs
  withIndices (x :&| xs) =
    (NEHead :*: x)
      :&| mapProd (\(i :*: y) -> NETail i :*: y) (withIndices xs)
  traverseProd f (x :&| xs) =
    (:&|) <$> f x <*> traverseProd f xs
  zipWithProd f (x :&| xs) (y :&| ys) = f x y :&| zipWithProd f xs ys
  htraverse ff f (x :&| xs) =
    (:&|) <$> f x <*> htraverse ff f xs
  ixProd = \case
    NEHead -> \f -> \case
      x :&| xs -> (:&| xs) <$> f x
    NETail i -> \f -> \case
      x :&| xs -> (x :&|) <$> ixProd i f xs
  toRec (x :&| xs) = x :& xs
  withPureProd (x :&| xs) = withPureProdNE x xs

-- | A stronger version of 'withPureProd' for 'NERec', providing
-- a 'RecApplicative' instance as well.
withPureProdNE ::
  f a ->
  Rec f as ->
  ((RecApplicative as, PureProd NonEmpty (a ':| as)) => r) ->
  r
withPureProdNE _ = withPureProdList

instance RecApplicative as => PureProd NonEmpty (a ':| as) where
  pureProd x = x :&| pureProd x

instance (c a, RPureConstrained c as) => PureProdC NonEmpty c (a ':| as) where
  pureProdC x = x :&| pureProdC @_ @c x

instance (c (g a), ReifyConstraint c g as) => ReifyConstraintProd NonEmpty c g (a ':| as) where
  reifyConstraintProd (x :&| xs) =
    V.Compose (Dict x)
      :&| reifyConstraintProd @_ @c xs

-- | Test if two indices point to the same item in a list.
--
-- We have to return a 'Maybe' here instead of a 'Decision', because it
-- might be the case that the same item might be duplicated in a list.
-- Therefore, even if two indices are different, we cannot prove that the
-- values they point to are different.
sameIndexVal ::
  Index as a ->
  Index as b ->
  Maybe (a :~: b)
sameIndexVal = \case
  IZ -> \case
    IZ -> Just Refl
    IS _ -> Nothing
  IS i -> \case
    IZ -> Nothing
    IS j -> sameIndexVal i j <&> \case Refl -> Refl

-- | Test if two indices point to the same item in a non-empty list.
--
-- We have to return a 'Maybe' here instead of a 'Decision', because it
-- might be the case that the same item might be duplicated in a list.
-- Therefore, even if two indices are different, we cannot prove that the
-- values they point to are different.
sameNEIndexVal ::
  NEIndex as a ->
  NEIndex as b ->
  Maybe (a :~: b)
sameNEIndexVal = \case
  NEHead -> \case
    NEHead -> Just Refl
    NETail _ -> Nothing
  NETail i -> \case
    NEHead -> Nothing
    NETail j -> sameIndexVal i j <&> \case Refl -> Refl

-- | Trivially witness an item in the second field of a type-level tuple.
data ISnd :: (j, k) -> k -> Type where
  ISnd :: ISnd '(a, b) b

deriving instance Show (ISnd as a)
deriving instance Read (ISnd '(a, b) b)
deriving instance Eq (ISnd as a)
deriving instance Ord (ISnd as a)

-- | Kind-indexed singleton for 'ISnd'.
data SISnd (as :: (j, k)) (a :: k) :: ISnd as a -> Type where
  SISnd :: SISnd '(a, b) b 'ISnd

deriving instance Show (SISnd as a i)

#if __GLASGOW_HASKELL__ >= 908
type instance forall as a. Sing = SISnd as a :: ISnd as a -> Type
#else
type instance Sing = SISnd as a :: ISnd as a -> Type
#endif


instance SingI 'ISnd where
  sing = SISnd

instance SingKind (ISnd as a) where
  type Demote (ISnd as a) = ISnd as a
  fromSing SISnd = ISnd
  toSing ISnd = SomeSing SISnd

instance SDecide (ISnd as a) where
  SISnd %~ SISnd = Proved Refl

-- | A 'PTup' tuples up some singleton with some value; a @'PTup' f '(w,
-- a)@ contains a @'Sing' w@ and an @f a@.
--
-- This can be useful for carrying along some witness aside a functor
-- value.
data PTup :: (k -> Type) -> (j, k) -> Type where
  PTup :: Sing w -> f a -> PTup f '(w, a)

deriving instance (Show (Sing w), Show (f a)) => Show (PTup f '(w, a))
deriving instance (Read (Sing w), Read (f a)) => Read (PTup f '(w, a))
deriving instance (Eq (Sing w), Eq (f a)) => Eq (PTup f '(w, a))
deriving instance (Ord (Sing w), Ord (f a)) => Ord (PTup f '(w, a))

instance FProd ((,) j) where
  type Elem ((,) j) = ISnd
  type Prod ((,) j) = PTup

  singProd (STuple2 w x) = PTup w x
  prodSing (PTup w x) = STuple2 w x
  withIndices (PTup w x) = PTup w (ISnd :*: x)
  traverseProd f (PTup w x) = PTup w <$> f x
  zipWithProd f (PTup w x) (PTup _ y) = PTup w (f x y)
  htraverse _ f (PTup w x) = PTup w <$> f x
  ixProd ISnd f (PTup w x) = PTup w <$> f x
  toRec (PTup _ x) = x :& RNil
  withPureProd (PTup Sing _) x = x

instance SingI w => PureProd ((,) j) '(w, a) where
  pureProd = PTup sing

instance (SingI w, c a) => PureProdC ((,) j) c '(w, a) where
  pureProdC = PTup sing

instance c (g a) => ReifyConstraintProd ((,) j) c g '(w, a) where
  reifyConstraintProd (PTup w x) = PTup w $ V.Compose (Dict x)

-- | Trivially witness the item held in an 'Identity'.
--
-- @since 0.1.3.0
data IIdentity :: Identity k -> k -> Type where
  IId :: IIdentity ('Identity x) x

deriving instance Show (IIdentity as a)
deriving instance Read (IIdentity ('Identity a) a)
deriving instance Eq (IIdentity as a)
deriving instance Ord (IIdentity as a)

-- | Kind-indexed singleton for 'IIdentity'.
--
-- @since 0.1.5.0
data SIIdentity (as :: Identity k) (a :: k) :: IIdentity as a -> Type where
  SIId :: SIIdentity ('Identity a) a 'IId

deriving instance Show (SIIdentity as a i)

#if __GLASGOW_HASKELL__ >= 908
type instance forall as a. Sing = SIIdentity as a :: IIdentity as a -> Type
#else
type instance Sing = SIIdentity as a :: IIdentity as a -> Type
#endif

instance SingI 'IId where
  sing = SIId

instance SingKind (IIdentity as a) where
  type Demote (IIdentity as a) = IIdentity as a
  fromSing SIId = IId
  toSing IId = SomeSing SIId

instance SDecide (IIdentity as a) where
  SIId %~ SIId = Proved Refl

-- | A 'PIdentity' is a trivial functor product; it is simply the functor,
-- itself, alone.  @'PIdentity' f ('Identity' a)@ is simply @f a@.  This
-- may be useful in conjunction with other combinators.
data PIdentity :: (k -> Type) -> Identity k -> Type where
  PIdentity :: f a -> PIdentity f ('Identity a)

deriving instance Show (f a) => Show (PIdentity f ('Identity a))
deriving instance Read (f a) => Read (PIdentity f ('Identity a))
deriving instance Eq (f a) => Eq (PIdentity f ('Identity a))
deriving instance Ord (f a) => Ord (PIdentity f ('Identity a))

instance FProd Identity where
  type Elem Identity = IIdentity
  type Prod Identity = PIdentity

  singProd (SIdentity x) = PIdentity x
  prodSing (PIdentity x) = SIdentity x
  withIndices (PIdentity x) = PIdentity (IId :*: x)
  traverseProd f (PIdentity x) = PIdentity <$> f x
  zipWithProd f (PIdentity x) (PIdentity y) = PIdentity (f x y)
  htraverse _ f (PIdentity x) = PIdentity <$> f x
  ixProd IId f (PIdentity x) = PIdentity <$> f x
  toRec (PIdentity x) = x :& RNil
  withPureProd (PIdentity _) x = x

instance PureProd Identity ('Identity a) where
  pureProd = PIdentity

instance c a => PureProdC Identity c ('Identity a) where
  pureProdC = PIdentity

instance c (g a) => ReifyConstraintProd Identity c g ('Identity a) where
  reifyConstraintProd (PIdentity x) = PIdentity $ V.Compose (Dict x)

-- | Produce an 'Index' from an 'RElem' constraint.
rElemIndex ::
  forall r rs i.
  (RElem r rs i, PureProd [] rs) =>
  Index rs r
rElemIndex = rgetC indices

-- | Use an 'Index' to inject an @f a@ into a 'CoRec'.
toCoRec ::
  forall k (as :: [k]) a f.
  (RecApplicative as, FoldRec as as) =>
  Index as a ->
  f a ->
  CoRec f as
toCoRec = \case
  IZ -> CoRec
  IS i -> \x -> fromJust . firstField $ mapProd (go i x) indices
  where
    go :: Index bs a -> f a -> Index (b ': bs) c -> V.Compose Maybe f c
    go i x j = case sameIndexVal (IS i) j of
      Just Refl -> V.Compose (Just x)
      Nothing -> V.Compose Nothing

-- | If we have @'Index' as a@, we should also be able to create an item
-- that would require @'RElem' a as ('V.RIndex' as a)@.  Along with
-- 'rElemIndex', this essentially converts between the indexing system in
-- this library and the indexing system of /vinyl/.
indexRElem ::
  (SDecide k, SingI (a :: k), RecApplicative as, FoldRec as as) =>
  Index as a ->
  (RElem a as (V.RIndex a as) => r) ->
  r
indexRElem i = case toCoRec i x of
  CoRec y -> case x %~ y of
    Proved Refl -> id
    Disproved _ -> \_ -> errorWithoutStackTrace "why :|"
  where
    x = sing
