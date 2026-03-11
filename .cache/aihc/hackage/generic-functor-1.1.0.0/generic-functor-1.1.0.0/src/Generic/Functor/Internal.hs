{-# LANGUAGE
  AllowAmbiguousTypes,
  ConstraintKinds,
  EmptyCase,
  FlexibleContexts,
  FlexibleInstances,
  MultiParamTypeClasses,
  QuantifiedConstraints,
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeOperators,
  UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

-- | This is an internal module. Look, don't touch.
--
-- "Generic.Functor" is the public API.

module Generic.Functor.Internal where

import ApNormalize
import Control.Applicative (liftA2)
import Data.Bifoldable (Bifoldable(..))
import Data.Bifunctor (Bifunctor(..))
import Data.Coerce (coerce, Coercible)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Const (Const(..))
import Data.Monoid (Endo(..))
import GHC.Generics hiding (S)

import Generic.Functor.Internal.Implicit

-- | Generic implementation of 'fmap'. See also 'GenericFunctor' for @DerivingVia@,
-- using 'gfmap' under the hood.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics" ('Generic')
-- import "Generic.Functor" ('gfmap')
--
-- data Twice a = Twice (Either a a)
--   deriving 'Generic'
--
-- instance 'Functor' Twice where
--   'fmap' = 'gfmap'
-- @
--
-- Unlike 'gsolomap', 'gfmap' is safe to use in all contexts.
gfmap :: forall f a b. GFunctor f => (a -> b) -> (f a -> f b)
gfmap f = with @(GFunctorRep a b f) (to . gmapRep (defaultIncoherent f) . from)

-- | Generalized generic functor.
--
-- 'gsolomap' is a generalization of 'gfmap' (generic 'fmap'),
-- where the type parameter to be \"mapped\" does not have to be the last one.
--
-- 'gsolomap' is __unsafe__: misuse will break your programs.
-- Read the <#gsolomapusage Usage> section below for details.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics" ('Generic')
-- import "Generic.Functor" ('gsolomap')
--
-- data Result a r = Error a | Ok r  -- Another name for Either
--   deriving 'Generic'
--
-- mapError :: (a -> b) -> Result a r -> Result b r
-- mapError = 'gsolomap'
--
-- mapOk :: (r -> s) -> Result a r -> Result a s
-- mapOk = 'gsolomap'
--
-- mapBoth :: (a -> b) -> Result a a -> Result b b
-- mapBoth = 'gsolomap'
-- @
--
-- === Usage #gsolomapusage#
--
-- (This also applies to 'solomap', 'gmultimap', and 'multimap'.)
--
-- 'gsolomap' should only be used to define __polymorphic__ "@fmap@-like functions".
-- It works only in contexts where @a@ and @b@ are two distinct, non-unifiable
-- type variables. This is usually the case when they are bound by universal
-- quantification (@forall a b. ...@), with no equality constraints on @a@ and
-- @b@.
--
-- The one guarantee of 'gsolomap' is that @'gsolomap' 'id' = 'id'@.
-- Under the above conditions, that law and the types should uniquely determine
-- the implementation, which 'gsolomap' seeks automatically.
--
-- The unsafety is due to the use of incoherent instances as part of the
-- definition of 'GSolomap'. Functions are safe to specialize after 'GSolomap'
-- (and 'Solomap') constraints have been discharged.
--
-- Note also that the type parameters of 'gsolomap' must all be determined by
-- the context. For instance, composing two 'gsolomap', as in
-- @'gsolomap' f . 'gsolomap' g@, is a type error because the type in the middle
-- cannot be inferred.
gsolomap :: forall a b x y. (Generic x, Generic y, GSolomap a b x y) => (a -> b) -> (x -> y)
gsolomap f = to . gmapRep (defaultIncoherent f) . from

-- | Generalized implicit functor.
--
-- Use this when @x@ and @y@ are applications of existing functors
-- ('Functor', 'Bifunctor').
--
-- This is a different use case from 'Generic.Functor.gfmap' and 'gsolomap', which make
-- functors out of freshly declared @data@ types.
--
-- 'solomap' is __unsafe__: misuse will break your programs.
--
-- See the <#gsolomapusage Usage> section of 'gsolomap' for details.
--
-- === Example
--
-- @
-- map1 :: (a -> b) -> Either e (Maybe [IO a]) -> Either e (Maybe [IO b])
-- map1 = 'solomap'
-- -- equivalent to:   fmap . fmap . fmap . fmap
--
-- map2 :: (a -> b) -> (e -> Either [a] r) -> (e -> Either [b] r)
-- map2 = 'solomap'
-- -- equivalent to:   \\f -> fmap (bimap (fmap f) id)
-- @
solomap :: forall a b x y. Solomap a b x y => (a -> b) -> (x -> y)
solomap f = multimap f

-- | Generic n-ary functor.
--
-- A generalization of 'gsolomap' to map over multiple parameters simultaneously.
-- 'gmultimap' takes a list of functions separated by @(':+')@ and terminated by @()@.
--
-- 'gmultimap' is __unsafe__: misuse will break your programs.
-- The type of every function in the list must be some @(a -> b)@
-- where @a@ and @b@ are distinct type variables.
--
-- See the <#gsolomapusage Usage> section of 'gsolomap' for details.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DeriveGeneric \#-}
--
-- import "GHC.Generics" ('Generic')
-- import "Generic.Functor" ('gmultimap')
--
-- data Three a b c = One a | Two b | Three c
--   deriving 'Generic'
--
-- mapThree :: (a -> a') -> (b -> b') -> (c -> c') -> Three a b c -> Three a' b' c'
-- mapThree f g h = 'gmultimap' (f ':+' g ':+' h ':+' ())
-- @
gmultimap :: forall arr x y. (Generic x, Generic y, GMultimap arr x y) => arr -> (x -> y)
gmultimap f = to . gmapRep (defaultIncoherent f) . from

-- | Implicit n-ary functor.
--
-- A generalization of 'solomap' to map over multiple parameters simultaneously.
-- 'multimap' takes a list of functions separated by @(':+')@ and terminated by @()@.
--
-- 'multimap' is __unsafe__: misuse will break your programs.
-- The type of every function in the list must be some @(a -> b)@
-- where @a@ and @b@ are distinct type variables.
--
-- See the <#gsolomapusage Usage> section of 'gsolomap' for details.
--
-- === Example
--
-- @
-- type F a b c = Either a (b, c)
--
-- map3 :: (a -> a') -> (b -> b') -> (c -> c') -> F a b c -> F a' b' c'
-- map3 f g h = 'multimap' (f ':+' g ':+' h ':+' ())
-- -- equivalent to:  \\f g h -> bimap f (bimap g h)
-- @
multimap :: forall arr x y. Multimap arr x y => arr -> (x -> y)
multimap f = multimapI (defaultIncoherent f)

-- | Generic implementation of 'bimap' from 'Bifunctor'. See also 'GenericBifunctor'.
gbimap :: forall f a b c d. GBimap f => (a -> b) -> (c -> d) -> f a c -> f b d
gbimap f g = with @(GBimapRep a b c d f) (to . gmapRep (defaultIncoherent (f :+ g)) . from)

-- | Generic implementation of 'first' from 'Bifunctor'. See also 'GenericBifunctor'.
gfirst :: forall f a b c. GFirst f => (a -> b) -> f a c -> f b c
gfirst f = with @(GFirstRep a b c f) (to . gmapRep (defaultIncoherent f) . from)

-- | Generic implementation of 'second' from 'Bifunctor'. See also 'GenericBifunctor'.
gsecond :: forall f a c d. GSecond f => (c -> d) -> f a c -> f a d
gsecond = gfmap

-- *** Fold

-- | Generic implementation of 'foldMap' from 'Foldable'.
gfoldMap :: forall t m a. (GFoldMap m t, Monoid m) => (a -> m) -> t a -> m
gfoldMap f =
  with @(GFoldMapRep a a m t) (gfoldMapRep (defaultIncoherent (Fold @m @a @a f)) . from)

-- | Generic implementation of 'bifoldMap' from 'Bifoldable'.
gbifoldMap :: forall t m a b. (GBifoldMap m t, Monoid m) => (a -> m) -> (b -> m) -> t a b -> m
gbifoldMap f g =
  with @(GBifoldMapRep a a b b m t) (gfoldMapRep (defaultIncoherent (Fold @m @a @a f :+ Fold @m @b @b g)) . from)

-- *** Traverse

-- | Generic implementation of 'traverse' from 'Traversable'.
gtraverse :: forall t f a b. (GTraverse f t, Applicative f) => (a -> f b) -> t a -> f (t b)
gtraverse f = with @(GTraverseRep a b f t) (lowerAps . fmap to . gtraverseRep (defaultIncoherent f) . from)

-- | Generic implementation of 'bitraverse' from 'Bitraversable'.
gbitraverse :: forall t f a b c d. (GBitraverse f t, Applicative f) => (a -> f b) -> (c -> f d) -> t a c -> f (t b d)
gbitraverse f g =
  with @(GBitraverseRep a b c d f t) (lowerAps . fmap to . gtraverseRep (defaultIncoherent (f :+ g)) . from)


-- | Explicitly require a constraint, to force the instantiation of a quantified constraint.
with :: forall c r. (c => r) -> (c => r)
with x = x

-- ** Top-level constraints

-- *** @gfmap@

-- | Generic 'Functor'. Constraint for 'gfmap'.
class    (forall a. Generic (f a), forall a b. GFunctorRep a b f) => GFunctor f
instance (forall a. Generic (f a), forall a b. GFunctorRep a b f) => GFunctor f

-- | Internal component of 'GFunctor'.
--
-- This is an example of the \"quantified constraints trick\" to encode
-- @forall a b. GMap1 a b (Rep (f a)) (Rep (f b))@ which doesn't actually
-- work as-is.
class    GMap1 (Default Incoherent (a -> b)) (Rep (f a)) (Rep (f b)) => GFunctorRep a b f
instance GMap1 (Default Incoherent (a -> b)) (Rep (f a)) (Rep (f b)) => GFunctorRep a b f

-- *** @gbimap@

-- | Constraint for 'gbimap'.
class    (forall a c. Generic (f a c), forall a b c d. GBimapRep a b c d f) => GBimap f
instance (forall a c. Generic (f a c), forall a b c d. GBimapRep a b c d f) => GBimap f

-- | Internal component of 'GBifunctor'.
class    GMap1 (Default Incoherent ((a -> b) :+ (c -> d))) (Rep (f a c)) (Rep (f b d)) => GBimapRep a b c d f
instance GMap1 (Default Incoherent ((a -> b) :+ (c -> d))) (Rep (f a c)) (Rep (f b d)) => GBimapRep a b c d f

-- *** @gfirst@

-- | Constraint for 'gfirst'.
class    (forall a c. Generic (f a c), forall a b c. GFirstRep a b c f) => GFirst f
instance (forall a c. Generic (f a c), forall a b c. GFirstRep a b c f) => GFirst f

-- | Internal component of 'GFirst'.
class    GMap1 (Default Incoherent (a -> b)) (Rep (f a c)) (Rep (f b c)) => GFirstRep a b c f
instance GMap1 (Default Incoherent (a -> b)) (Rep (f a c)) (Rep (f b c)) => GFirstRep a b c f

-- *** @gsecond@

-- | Constraint for 'gsecond'.
class    (forall a c. Generic (f a c), forall a c d. GFunctorRep c d (f a)) => GSecond f
instance (forall a c. Generic (f a c), forall a c d. GFunctorRep c d (f a)) => GSecond f

-- | Generic 'Bifunctor'.
class    (GBimap f, GFirst f, GSecond f) => GBifunctor f
instance (GBimap f, GFirst f, GSecond f) => GBifunctor f

-- *** @gtraverse@

-- | Constraint for 'gtraverse'.
class    (forall a. Generic (t a), forall a b. GTraverseRep a b f t) => GTraverse f t
instance (forall a. Generic (t a), forall a b. GTraverseRep a b f t) => GTraverse f t

class    GTraverse1 f (Default Incoherent (a -> f b)) (Rep (t a)) (Rep (t b)) => GTraverseRep a b f t
instance GTraverse1 f (Default Incoherent (a -> f b)) (Rep (t a)) (Rep (t b)) => GTraverseRep a b f t

-- | Generic 'Traversable'.
class    (forall f. Applicative f => GBitraverse f t) => GTraversable t
instance (forall f. Applicative f => GBitraverse f t) => GTraversable t

-- | Constraint for 'gtraverse'.
class    (forall a b. Generic (t a b), forall a b c d. GBitraverseRep a b c d f t) => GBitraverse f t
instance (forall a b. Generic (t a b), forall a b c d. GBitraverseRep a b c d f t) => GBitraverse f t

class    GTraverse1 f (Default Incoherent ((a -> f b) :+ (c -> f d))) (Rep (t a c)) (Rep (t b d)) => GBitraverseRep a b c d f t
instance GTraverse1 f (Default Incoherent ((a -> f b) :+ (c -> f d))) (Rep (t a c)) (Rep (t b d)) => GBitraverseRep a b c d f t

-- | Generic 'Bitraversable'.
class    (forall f. Applicative f => GBitraverse f t) => GBitraversable t
instance (forall f. Applicative f => GBitraverse f t) => GBitraversable t

-- *** @foldMap@

-- | Constraint for 'gfoldMap'.
class    (forall a. Generic (t a), forall a b. GFoldMapRep a b m t) => GFoldMap m t
instance (forall a. Generic (t a), forall a b. GFoldMapRep a b m t) => GFoldMap m t

class    GFoldMap1 m (Default Incoherent (Fold m a b)) (Rep (t a)) (Rep (t b)) => GFoldMapRep a b m t
instance GFoldMap1 m (Default Incoherent (Fold m a b)) (Rep (t a)) (Rep (t b)) => GFoldMapRep a b m t

-- | Generic 'Foldable'. Constraint for 'GenericFunctor' (deriving-via 'Foldable').
class    (forall m. Monoid m => GFoldMap m t) => GFoldable t
instance (forall m. Monoid m => GFoldMap m t) => GFoldable t

-- | Constraint for 'gbifoldMap'.
class    (forall a b. Generic (t a b), forall a b c d. GBifoldMapRep a b c d m t) => GBifoldMap m t
instance (forall a b. Generic (t a b), forall a b c d. GBifoldMapRep a b c d m t) => GBifoldMap m t

class    GFoldMap1 m (Default Incoherent (Fold m a b :+ Fold m c d)) (Rep (t a c)) (Rep (t b d)) => GBifoldMapRep a b c d m t
instance GFoldMap1 m (Default Incoherent (Fold m a b :+ Fold m c d)) (Rep (t a c)) (Rep (t b d)) => GBifoldMapRep a b c d m t

-- | Generic 'Foldable'. Constraint for 'GenericFunctor' (deriving-via 'Foldable').
class    (forall m. Monoid m => GBifoldMap m t) => GBifoldable t
instance (forall m. Monoid m => GBifoldMap m t) => GBifoldable t

-- *** Others

-- | Constraint for 'gsolomap'.
class    GMultimap (a -> b) x y => GSolomap a b x y
instance GMultimap (a -> b) x y => GSolomap a b x y

-- | Constraint for 'solomap'.
class    Multimap (a -> b) x y => Solomap a b x y
instance Multimap (a -> b) x y => Solomap a b x y

-- | Constraint for 'gmultimap'.
class    GMap1 (Default Incoherent arr) (Rep x) (Rep y) => GMultimap arr x y
instance GMap1 (Default Incoherent arr) (Rep x) (Rep y) => GMultimap arr x y

-- | Constraint for 'multimap'.
class    MultimapI (Default Incoherent arr) x y => Multimap arr x y
instance MultimapI (Default Incoherent arr) x y => Multimap arr x y

-- * Deriving Via

-- ** Functor

-- | @newtype@ for @DerivingVia@ of 'Functor' and 'Foldable' instances.
--
-- Note: the GHC extensions @DeriveFunctor@, @DeriveFoldable@, and @DeriveTraversable@
-- (which implies all three) already works out-of-the-box in most cases.
-- There are exceptions, such as the following example.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DeriveGeneric, DerivingVia \#-}
--
-- import "GHC.Generics" ('Generic')
-- import "Generic.Functor" ('GenericFunctor'(..))
--
-- data Twice a = Twice (Either a a)
--   deriving 'Generic'
--   deriving ('Functor', 'Foldable') via ('GenericFunctor' Twice)
-- @
newtype GenericFunctor f a = GenericFunctor (f a)

instance GFunctor f => Functor (GenericFunctor f) where
  fmap = coerce1 (gfmap @f)

instance GFoldable f => Foldable (GenericFunctor f) where
  foldMap = coerceFoldMap (gfoldMap @f)

-- ** Bifunctor

-- | @newtype@ for @DerivingVia@ of 'Bifunctor' and 'Bifoldable' instances.
--
-- Note: although 'GenericBifunctor' has 'Functor' and 'Foldable' instances,
-- it is recommended to use 'GenericFunctor' instead for those two classes.
-- They have to use a separate deriving clause from 'Bifunctor' and 'Bifoldable' anyway.
-- Those instances exist because they are to become superclasses of 'Bifunctor'
-- and 'Bifoldable'. The 'Foldable' instance of 'GenericBifunctor' is also less
-- efficient than 'GenericFunctor' unless the extra @const mempty@ gets optimized away.
--
-- === Example
--
-- @
-- {-\# LANGUAGE DeriveGeneric, DerivingVia \#-}
--
-- import "Data.Bifoldable" ('Bifoldable')
-- import "Data.Bifunctor" ('Bifunctor')
-- import "GHC.Generics" ('Generic')
-- import "Generic.Functor" ('GenericFunctor'(..), 'GenericBifunctor'(..))
--
-- data Tree a b = Node a (Tree a b) (Tree a b) | Leaf b
--   deriving 'Generic'
--   deriving ('Functor', 'Foldable') via ('GenericFunctor' (Tree a))
--   deriving ('Bifunctor', 'Bifoldable') via ('GenericBifunctor' Tree)
--
-- data CofreeF f a b = a :< f b
--   deriving 'Generic'
--   deriving ('Bifunctor', 'Bifoldable') via ('GenericBifunctor' (CofreeF f))
-- @
newtype GenericBifunctor f a b = GenericBifunctor (f a b)

instance GBifunctor f => Bifunctor (GenericBifunctor f) where
  bimap = coerce2 (gbimap @f)
  first = coerce3 (gfirst @f)
  second = fmap

instance GBifoldable f => Bifoldable (GenericBifunctor f) where
  bifoldMap = coerceBifoldMap (gbifoldMap @f)

instance GSecond f => Functor (GenericBifunctor f a) where
  fmap = coerce3 (gsecond @f)

instance GBifoldable f => Foldable (GenericBifunctor f a) where
  foldMap = bifoldMap (const mempty)

-- ** Internal coercions

coerce1 :: Coercible s t => (r -> s) -> (r -> t)
coerce1 = coerce

coerce2 :: Coercible t u => (r -> s -> t) -> (r -> s -> u)
coerce2 = coerce

coerce3 :: (Coercible w v, Coercible (f b d) (g b d)) => (r -> w -> f b d) -> (r -> v -> g b d)
coerce3 = coerce

coerceFoldMap :: Coercible t u => (am -> t -> m) -> (am -> u -> m)
coerceFoldMap = coerce

coerceBifoldMap :: Coercible t u => (am -> bm -> t -> m) -> (am -> bm -> u -> m)
coerceBifoldMap = coerce

-- ** @GMultimapK@

-- | We use the same class to implement all of 'fmap', 'foldMap', 'traverse',
-- instantiating @m@ as 'Identity', 'Const (EndoM mm)' and 'Aps n' respectively.
-- Those three cases differ in their instances for 'K1'.
--
-- (the K stands for @Kleisli@, because the result is @Kleisli m (f ()) (g ())@
class GMultimapK m arr f g where
  gmultimapK :: arr -> f () -> m (g ())

-- *** Instance for @fmap@

instance MultimapI arr x y => GMultimapK Identity arr (K1 i x) (K1 i' y) where
  gmultimapK = coerce (multimapI @arr @x @y)

class    GMultimapK Identity arr f g => GMap1 arr f g
instance GMultimapK Identity arr f g => GMap1 arr f g

gmapRep :: GMap1 arr f g => arr -> f () -> g ()
gmapRep f x = runIdentity (gmultimapK f x)

-- *** Instance for @foldMap@

instance (Multifold_ m arr x y, Monoid m) => GMultimapK (Const (EndoM m)) arr (K1 i x) (K1 i' y) where
  gmultimapK f (K1 x) = K1 <$> foldToConst (multifold_ f) x

-- Spooky instance. It makes sense only because @GMultimapK (Const (EndoM m))@
-- occurs exclusively under a quantified constraint (in the definition of GFoldMap).
instance {-# INCOHERENT #-} GMultimapK (Const (EndoM m)) arr (K1 i x) (K1 i x) where
  gmultimapK _ (K1 _) = Const (Endo id)

-- An extra wrapper to simplify away @(mempty <> _)@ and @(_ <> mempty)@.
type EndoM m = Endo (Maybe m)

unEndoM :: Monoid m => EndoM m -> m
unEndoM (Endo f) = case f Nothing of
  Nothing -> mempty
  Just y -> y

liftEndoM :: Monoid m => m -> EndoM m
liftEndoM y = Endo (Just . app)
  where
    app Nothing = y
    app (Just y') = y `mappend` y'

foldToConst :: Monoid m => Fold m x y -> x -> Const (EndoM m) y
foldToConst (Fold f) x = Const (liftEndoM (f x))

class    GMultimapK (Const (EndoM m)) arr f g => GFoldMap1 m arr f g
instance GMultimapK (Const (EndoM m)) arr f g => GFoldMap1 m arr f g

-- | Danger! @GFoldMap1 m arr f f@ MUST come from a quantified constraint (see use in 'gfoldMap').
gfoldMapRep :: forall m arr f. (GFoldMap1 m arr f f, Monoid m) => arr -> f () -> m
gfoldMapRep f x = unEndoM (getConst (gmultimapK f x :: Const (EndoM m) (f ())))

-- *** Instance for @traverse@

instance (Multitraverse m arr x y) => GMultimapK (Aps m) arr (K1 i x) (K1 i' y) where
  gmultimapK f (K1 x) = K1 <$>^ multitraverse f x

class    GMultimapK (Aps m) arr f g => GTraverse1 m arr f g
instance GMultimapK (Aps m) arr f g => GTraverse1 m arr f g

gtraverseRep :: GTraverse1 m arr f g => arr -> f () -> Aps m (g ())
gtraverseRep = gmultimapK

-- *** Common instances

instance (GMultimapK m arr f g, Functor m) => GMultimapK m arr (M1 i c f) (M1 i' c' g) where
  gmultimapK f (M1 x) = M1 <$> gmultimapK f x

instance
  (GMultimapK m arr f1 g1, GMultimapK m arr f2 g2, Applicative m) =>
  GMultimapK m arr (f1 :+: f2) (g1 :+: g2)
  where
  gmultimapK f (L1 x) = L1 <$> gmultimapK f x
  gmultimapK f (R1 y) = R1 <$> gmultimapK f y

instance
  (GMultimapK m arr f1 g1, GMultimapK m arr f2 g2, Applicative m) =>
  GMultimapK m arr (f1 :*: f2) (g1 :*: g2)
  where
  gmultimapK f (x :*: y) = liftA2 (:*:) (gmultimapK f x) (gmultimapK f y)

instance Applicative m => GMultimapK m arr U1 U1 where
  gmultimapK _ U1 = pure U1

instance GMultimapK m arr V1 V1 where
  gmultimapK _ v = case v of
