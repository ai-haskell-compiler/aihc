{-# LANGUAGE
    AllowAmbiguousTypes,
    EmptyCase,
    FlexibleContexts,
    FlexibleInstances,
    InstanceSigs,
    MultiParamTypeClasses,
    PolyKinds,
    ScopedTypeVariables,
    TypeApplications,
    TypeFamilies,
    TypeOperators,
    UndecidableInstances #-}

-- | Generic cogenerators
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
-- It is made available only for debugging.
-- Otherwise, use "Test.Fun".
--
-- If something here seems useful, please open an issue to export it from an
-- external module.

module Test.Fun.Internal.Generic where

import Data.Functor.Identity (Identity(..))
import Data.Kind (Type)
import Data.Monoid (Sum(..))
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable, typeRep, typeRepTyCon, tyConName)
import Data.Void (Void)
import Data.Word (Word)
import GHC.Generics

import Test.Fun.Internal.Types
import Test.Fun.Internal.CoGen

-- * CoArbitrary

-- | Implicit, default cogenerator.
class Applicative gen => CoArbitrary gen a where
  coarbitrary :: forall r. Co gen a r

-- * Generics

-- | Cogenerator for generic types, parameterized by a list of cogenerators,
-- one for each constructor.
--
-- The list is constructed with @(':+')@ (pairs) and @()@.
--
-- === __Example__
--
-- @
-- -- Cogenerator for lists, parameterized by a cogenerator for elements.
-- 'cogenList' :: forall a. (forall r. 'Co' Gen a r) -> (forall r. 'Co' Gen [a] r)
-- 'cogenList' coa = 'cogenGeneric' gs where
--   -- gs :: GSumCo Gen [a] r  -- unfolds to --
--   gs ::
--     (gen r -> gen r)                 ':+'  -- Cogenerator for the empty list
--     (gen r -> gen (a ':->' [a] ':->' r)) ':+'  -- Cogenerator for non-empty lists
--     ()
--   gs = id ':+' (coa '.' 'cogenList' coa) ':+' ()
-- @
cogenGeneric :: forall a r gen.
  (Generic a, GCoGen a, Applicative gen) => GSumCo gen a r -> Co gen a r
cogenGeneric gs g = ToShrink <$> (Case typename normalize <$> branches g <*> g) where
  typename = shortTypeName @a
  normalize = gnormalize . from
  branches = genBranches @(Rep a) gs

-- | Heterogeneous products as nested pairs. These products must be terminated by ().
--
-- > a :+ b :+ c :+ ()  -- the product of a, b, c
data a :+ b = a :+ b

infixr 2 :+  -- more than :-> to force parentheses

-- | Cogenerator for lists.
--
-- === __Implementation note__
--
-- The cogenerator of @a@ is made monomorphic only to keep the type of
-- 'cogenList' at rank 1. But really, don't pay attention to the last type
-- argument of 'Co'.
--
-- @
-- 'cogenList' :: ... => 'Co' gen a _ -> 'Co' gen [a] _
-- @
cogenList :: forall a r gen. Applicative gen => Co gen a ([a] :-> r) -> Co gen [a] r
cogenList coa = self where
  self = cogenGeneric @[a] (id :+ (coa . self) :+ ())

-- * Internals

-- ** Generic cogenerators

-- The generic implementation is split in three parts:
--
-- - the type name, for printing;
-- - simplifying the generic representation to forget details specific to GHC.Generics;
-- - constructing the @case@ branches.

-- | Class of types with generic cogenerators.
class    (Typeable_ a, GNormalize (Rep a), GenBranches (Rep a)) => GCoGen a
instance (Typeable_ a, GNormalize (Rep a), GenBranches (Rep a)) => GCoGen a

-- *** Reify the name and arity of a type constructor

shortTypeName :: forall a. Typeable_ a => TypeName
shortTypeName = shortTypeName_ @_ @a ""

class Typeable_ (a :: k) where
  shortTypeName_ :: String -> String

instance {-# OVERLAPPING #-} Typeable_ f => Typeable_ (f a) where
  shortTypeName_ = shortTypeName_ @_ @f . (' ' :) . ('_' :)

instance Typeable a => Typeable_ a where
  shortTypeName_ = ((++) . tyConName . typeRepTyCon) (typeRep (Proxy @a))

-- *** Type-level functions on generic representations

-- | Convert a generic 'Rep' into a sum of products made of 'Either' and @(,)@,
-- where products are nested to the left (i.e., @((((), a), b), c)@).
type family Normalize (f :: Type -> Type) :: Type where
  Normalize (M1 D c f) = Normalize f
  Normalize (f :+: g) = Either (Normalize f) (Normalize g)
  Normalize V1 = Void
  Normalize (M1 C c f) = () >*> f

-- | Convert a @(:*:)@ product into a left-nested @(,)@ product.
type family (>*>) (s :: Type) (f :: Type -> Type) :: Type where
  s >*> (f :*: g) = s >*> f >*> g
  s >*> M1 S c (K1 R a) = (s, a)
  s >*> U1 = s

infixl 9 >*>

-- | The list of cogenerators for a generic type, one for each constructor.
type GSumCo gen a r = GSumCo_ gen (Rep a) r ()

type family GSumCo_ (gen :: Type -> Type) (f :: Type -> Type) (r :: Type) (t :: Type) :: Type where
  GSumCo_ gen (M1 D c f) r t = GSumCo_ gen f r t
  GSumCo_ gen (f :+: g)  r t = GSumCo_ gen f r (GSumCo_ gen g r t)
  GSumCo_ gen (M1 C c f) r t = (gen r -> gen (f >-> r)) :+ t
  GSumCo_ gen V1 r t = t

type family (>->) (f :: Type -> Type) (r :: Type) :: Type where
  (f :*: g) >-> r = f >-> g >-> r
  M1 S c (K1 R a) >-> r = a :-> r
  U1 >-> r = r

infixr 9 >->

-- *** Simplify the generic representation

-- Sums

class GNormalize f where
  gnormalize :: f p -> Normalize f

instance GNormalize f => GNormalize (M1 D c f) where
  gnormalize = gnormalize . unM1

instance (GNormalize f, GNormalize g) => GNormalize (f :+: g) where
  gnormalize (L1 x) = Left (gnormalize x)
  gnormalize (R1 y) = Right (gnormalize y)

instance GNormalize V1 where
  gnormalize x = case x of {}

instance GToList f => GNormalize (M1 C c f) where
  gnormalize = gToList () . unM1

-- Products

class GToList f where
  gToList :: y -> f p -> (y >*> f)

instance (GToList f, GToList g) => GToList (f :*: g) where
  gToList y (u :*: v) = (y `gToList` u) `gToList` v

instance GToList (M1 S c (K1 R a)) where
  gToList y (M1 (K1 a)) = (y, a)

instance GToList U1 where
  gToList y _ = y

-- *** Construct the @case@ branches

genBranches :: forall f r gen. (Applicative gen, GenBranches f) =>
  GSumCo_ gen f r () -> gen r -> gen (Branches (Normalize f) r)
genBranches gs g = genBranches_ @f g (\c () -> c) gs

-- Sums

class GenBranches f where
  genBranches_ :: forall t r y gen. Applicative gen =>
    gen r ->
    (gen (Branches (Normalize f) r) -> t -> y) ->
    GSumCo_ gen f r t -> y

instance GenBranches f => GenBranches (M1 D c f) where
  genBranches_ = genBranches_ @f

instance (GenBranches f, GenBranches g) => GenBranches (f :+: g) where
  genBranches_ gr k =
    genBranches_ @f gr (\gf ->
    genBranches_ @g gr (\gg ->
    k (Alt <$> gf <*> gg)))

instance (Constructor c, MkFields f) => GenBranches (M1 C c f) where
  genBranches_ gr k (h :+ t) = k gh t where
    gh = (Pat cn . mkFields @f . NoField) <$> h gr
    cn = conName @c undefined

instance GenBranches V1 where
  genBranches_ _ k = k (pure Fail)

-- Products

class MkFields f where
  mkFields :: Fields x (f >-> r) -> Fields (x >*> f) r

instance (MkFields f, MkFields g) => MkFields (f :*: g) where
  mkFields = mkFields @g . mkFields @f

instance MkFields (M1 S c (K1 R a)) where
  mkFields = Field

instance MkFields U1 where
  mkFields = id

-- ** Generic @CoArbitrary@

-- | Generic implementation of 'coarbitrary'.
--
-- @
-- -- Assuming MyData is a data type whose fields are all instances of CoArbitrary.
--
-- instance CoArbitrary MyData where
--   coarbitrary = coarbitraryGeneric
-- @
coarbitraryGeneric :: forall a r gen. (Generic a, GCoArbitrary gen a) => Co gen a r
coarbitraryGeneric = cogenGeneric (gsumCoarb @gen @(Rep a) (Proxy @r) ())

-- | Constraint for 'coarbitraryGeneric'.
class    (GCoGen a, Applicative gen, GSumCoArb gen (Rep a)) => GCoArbitrary gen a
instance (GCoGen a, Applicative gen, GSumCoArb gen (Rep a)) => GCoArbitrary gen a

-- Sums

class GSumCoArb gen f where
  gsumCoarb :: forall r t. Proxy r -> t -> GSumCo_ gen f r t

instance GSumCoArb gen f => GSumCoArb gen (M1 D c f) where
  gsumCoarb = gsumCoarb @gen @f

instance (GSumCoArb gen f, GSumCoArb gen g) => GSumCoArb gen (f :+: g) where
  gsumCoarb p = gsumCoarb @gen @f p . gsumCoarb @gen @g p

instance GSumCoArb gen V1 where
  gsumCoarb _ = id

instance GProdCoArb gen f => GSumCoArb gen (M1 C c f) where
  gsumCoarb _ t = gprodCoarb @gen @f :+ t

-- Products

class GProdCoArb gen f where
  gprodCoarb :: gen r -> gen (f >-> r)

instance (GProdCoArb gen f, GProdCoArb gen g) => GProdCoArb gen (f :*: g) where
  gprodCoarb = gprodCoarb @gen @f . gprodCoarb @gen @g

instance CoArbitrary gen a => GProdCoArb gen (M1 S c (K1 R a)) where
  gprodCoarb = coarbitrary

instance GProdCoArb gen U1 where
  gprodCoarb = id

-- Instances

instance Applicative gen => CoArbitrary gen () where
  coarbitrary g = Const <$> g

instance Applicative gen => CoArbitrary gen Void where
  coarbitrary _ = pure (Absurd id)

instance Applicative gen => CoArbitrary gen Integer where
  coarbitrary = cogenIntegral "Integer"

instance Applicative gen => CoArbitrary gen Int where
  coarbitrary = cogenIntegral "Int"

instance Applicative gen => CoArbitrary gen Word where
  coarbitrary = cogenIntegral "Word"

instance Applicative gen => CoArbitrary gen Bool where
  coarbitrary = coarbitraryGeneric

instance Applicative gen => CoArbitrary gen Ordering where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary gen a => CoArbitrary gen [a] where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary gen a => CoArbitrary gen (Maybe a) where
  coarbitrary = coarbitraryGeneric

instance (CoArbitrary gen a, CoArbitrary gen b) => CoArbitrary gen (a, b) where
  coarbitrary = coarbitraryGeneric

instance (CoArbitrary gen a, CoArbitrary gen b) => CoArbitrary gen (Either a b) where
  coarbitrary = coarbitraryGeneric

instance CoArbitrary gen a => CoArbitrary gen (Identity a) where
  coarbitrary = cogenEmbed "runIdentity" runIdentity coarbitrary

instance CoArbitrary gen a => CoArbitrary gen (Sum a) where
  coarbitrary = cogenEmbed "getSum" getSum coarbitrary
