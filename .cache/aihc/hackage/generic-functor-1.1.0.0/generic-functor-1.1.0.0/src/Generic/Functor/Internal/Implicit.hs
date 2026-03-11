{-# LANGUAGE
  ConstraintKinds,
  FlexibleContexts,
  FlexibleInstances,
  InstanceSigs,
  MultiParamTypeClasses,
  RankNTypes,
  ScopedTypeVariables,
  TypeApplications,
  TypeFamilies,
  TypeOperators,
  UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Generic.Functor.Internal.Implicit where

import Control.Arrow (Kleisli (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

-- | Core of 'multimap'
multimapI :: forall arr x y. MultimapI arr x y => arr -> (x -> y)
multimapI = multimapOf

multitraverse :: forall f arr x y. Multitraverse f arr x y => arr -> (x -> f y)
multitraverse f = runKleisli (multimapOf (coerce f :: WrapKleisli f arr))

-- | This is kept internal because of the 'Fold' wrapping.
multifold_ :: forall m arr x y. Multifold_ m arr x y => arr -> Fold m x y
multifold_ = multimapOf

multimapOf :: forall cat arr x y. MultimapOf cat arr x y => arr -> cat x y
multimapOf f = multimap_ (s2 f)

-- | Core of 'multimap'.
class    MultimapOf (->) arr x y => MultimapI arr x y
instance MultimapOf (->) arr x y => MultimapI arr x y

-- | Constraint for 'multifold_'.
class    MultimapOf (Fold m) arr x y => Multifold_ m arr x y
instance MultimapOf (Fold m) arr x y => Multifold_ m arr x y

-- | Constraint for 'multitraverse'.
class    Multitraverse_ f arr x y => Multitraverse f arr x y
instance Multitraverse_ f arr x y => Multitraverse f arr x y

-- | Internal definition of 'Multitraverse'
type Multitraverse_ f arr x y =
  ( MultimapOf (Kleisli f) (WrapKleisli f arr) x y,
    CoercibleKleisli f (WrapKleisli f arr) arr
  )

type family WrapKleisli (f :: Type -> Type) (arr :: Type)
type instance WrapKleisli _f NilArr = NilArr
type instance WrapKleisli _f (Rule rule mode)= Rule rule mode
type instance WrapKleisli f (a :+ arr) = WrapKleisli f a :+ WrapKleisli f arr
type instance WrapKleisli f (a -> f b) = Kleisli f a b

-- | Auxiliary constraint for 'Multitraverse'
class Coercible warr arr => CoercibleKleisli (f :: Type -> Type) warr arr
instance (d ~ NilArr) => CoercibleKleisli f d NilArr
instance (d ~ Rule rule mode) => CoercibleKleisli f d (Rule rule mode)
instance (CoercibleKleisli f a b, CoercibleKleisli f arr arr') => CoercibleKleisli f (a :+ arr) (b :+ arr')
instance (b2 ~ f c, a ~ Kleisli f b1 c) => CoercibleKleisli f a (b1 -> b2)

class    Multimap_ cat (S2 arr) x y => MultimapOf cat arr x y
instance Multimap_ cat (S2 arr) x y => MultimapOf cat arr x y

-- | @Fold m@ is like @Kleisli (Const m)@, but it has a different @FunctorOf@ instance,
-- with 'Foldable' instead of 'Traversable'.
newtype Fold m x y = Fold { unFold :: x -> m }

instance Monoid m => CatLike (Fold m) where
  catid = Fold (\_ -> mempty)

instance (Foldable t, Monoid m) => FunctorOf (Fold m) t where
  catmap (Fold f) = Fold (foldMap f)

instance (Bifoldable t, Monoid m) => BifunctorOf (Fold m) t where
  catbimap (Fold f) (Fold g) = Fold (bifoldMap f g)

-- * Internal

class CatLike cat where
  catid :: cat x x

instance CatLike (->) where
  catid = id

instance Applicative f => CatLike (Kleisli f) where
  catid = Kleisli pure

class FunctorOf cat t where
  catmap :: cat a b -> cat (t a) (t b)

instance Functor t => FunctorOf (->) t where
  catmap = fmap

instance (Applicative f, Traversable t) => FunctorOf (Kleisli f) t where
  catmap :: forall a b. Kleisli f a b -> Kleisli f (t a) (t b)
  catmap = coerce (traverse @t @f @a @b)

class BifunctorOf cat t where
  catbimap :: cat a b -> cat c d -> cat (t a c) (t b d)

instance Bifunctor t => BifunctorOf (->) t where
  catbimap = bimap

instance (Applicative f, Bitraversable t) => BifunctorOf (Kleisli f) t where
  catbimap :: forall a b c d. Kleisli f a b -> Kleisli f c d -> Kleisli f (t a c) (t b d)
  catbimap = coerce (bitraverse @t @f @a @b @c @d)

-- | Internal implementation of 'MultimapOf'.
class Multimap_ cat arr x y where
  multimap_ :: arr -> cat x y

-- | Heterogeneous lists of arrows are constructed as lists separated by
-- @(':+')@ and terminated by @()@.
--
-- === Example
--
-- Given @f :: a -> a'@ and @g :: b -> b'@,
-- @(f ':+' g ':+' ())@ is a list with the two elements @f@ and @g@.
--
-- @
-- if
--   f :: a -> a'
--   g :: b -> b'
--
-- then
--   f ':+' g ':+' ()  ::  (a -> a') ':+' (b -> b') ':+' ()
-- @
--
-- Those lists are used by 'gmultimap' and 'multimap'.
--
-- @
-- bimap_ :: (a -> a') -> (b -> b') -> (Maybe a, [Either b a]) -> (Maybe a', [Either b' a'])
-- bimap_ f g = 'multimap' (f ':+' g ':+' ())
-- @
data a :+ b = a :+ b

infixr 1 :+

-- Special arrows

data Rule rule mode = Rule rule mode
data AnyId = AnyId
data AnyFunctor = AnyFunctor
data AnyBifunctor = AnyBifunctor
data NilArr = NilArr

data Incoherent = Incoherent

type Default mode arr = arr :+ Rule AnyId mode :+ Rule AnyFunctor mode :+ Rule AnyBifunctor mode :+ NilArr

defaultIncoherent :: arr -> Default Incoherent arr
defaultIncoherent = def Incoherent

def :: mode -> arr -> Default mode arr
def mode arr = arr :+ Rule AnyId mode :+ Rule AnyFunctor mode :+ Rule AnyBifunctor mode :+ NilArr

-- | @arr@ is the list of arrows provided by the user. It is constant.
-- When testing whether any arrow matches, @arr'@ is the remaining list of
-- arrows to be tested.
data S arr arr' = S arr arr'

type S2 arr = S arr arr

s2 :: arr -> S2 arr
s2 f = S f f

-- The head doesn't match anything, go to the next thing.
instance {-# OVERLAPPABLE #-} Multimap_ cat (S arr arr') x y => Multimap_ cat (S arr (arr0 :+ arr')) x y where
  multimap_ (S f (_ :+ g')) = multimap_ (S f g')

-- Reassociate to even handle tree-shaped parameters.
instance Multimap_ cat (S arr (arr0 :+ arr1 :+ arr2)) x y => Multimap_ cat (S arr ((arr0 :+ arr1) :+ arr2)) x y where
  multimap_ (S f ((f0 :+ f1) :+ f2)) = multimap_ (S f (f0 :+ f1 :+ f2))

instance Multimap_ cat (S arr arr') x y => Multimap_ cat (S arr (NilArr :+ arr')) x y where
  multimap_ (S f (NilArr :+ f')) = multimap_ (S f f')

instance Multimap_ cat (S arr arr') x y => Multimap_ cat (S arr (() :+ arr')) x y where
  multimap_ (S f (() :+ f')) = multimap_ (S f f')

instance {-# INCOHERENT #-} Multimap_ cat (S arr (cat a b :+ arr')) a b where
  multimap_ (S _ (f :+ _)) = f

-- "id" instance
instance {-# INCOHERENT #-} CatLike cat => Multimap_ cat (S arr (Rule AnyId Incoherent :+ arr')) x x where
  multimap_ _ = catid

-- "Functor" instance
-- Note: if x ~ y, then @AnyId Incoherent@ should already have matched earlier
-- (if you remembered to put it in the list),
-- so we don't need another overlapping instance here.
instance
  {-# INCOHERENT #-}
  (FunctorOf cat f, MultimapOf cat arr x y) =>
  Multimap_ cat (S arr (Rule AnyFunctor Incoherent :+ arr')) (f x) (f y)
  where
  multimap_ (S f (Rule AnyFunctor Incoherent :+ _)) = catmap (multimapOf f)

-- "Bifunctor" instance.
-- Note: the overlap with AnyId (where x1 ~ y1, x2 ~ y2) and AnyFunctor (where x1 ~ x2)
-- is handled by putting those rules before AnyBifunctor in the list.
instance
  {-# INCOHERENT #-}
  (BifunctorOf cat f, MultimapOf cat arr x1 y1, MultimapOf cat arr x2 y2) =>
  Multimap_ cat (S arr (Rule AnyBifunctor Incoherent :+ arr')) (f x1 x2) (f y1 y2)
  where
  multimap_ (S f (Rule AnyBifunctor Incoherent :+ _)) = catbimap (multimapOf f) (multimapOf f)

-- Hardcoded special case for (->)
-- We know how to "Multimap_ (->)" over (->),
-- not "Multimap_ (Kleisli f)".
instance
  {-# INCOHERENT #-}
  (MultimapOf (->) arr y1 x1, MultimapOf (->) arr x2 y2) =>
  Multimap_ (->) (S arr (Rule AnyBifunctor Incoherent :+ arr')) (x1 -> x2) (y1 -> y2)
  where
  multimap_ (S f (Rule AnyBifunctor Incoherent :+ _)) u = multimapOf f . u . multimapOf f