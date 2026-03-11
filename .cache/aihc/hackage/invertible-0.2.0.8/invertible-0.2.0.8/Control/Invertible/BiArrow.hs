-- |
-- Bidirectional arrows.
-- Taken directly from
--
--  * Artem Alimarine, et al. /There and Back Again: Arrows for Invertible Programming/. Haskell '05. <http://citeseer.ist.psu.edu/alimarine05there.html>
--
{-# LANGUAGE CPP, Trustworthy, TypeOperators #-}
module Control.Invertible.BiArrow
  ( BiArrow(..)
  , BiArrow'
  , biarr
  , involve
  , (^^>>)
  , (>>^^)
  , (<<^^)
  , (^^<<)
  , BiKleisli
  ) where

import Prelude hiding ((.))

import Control.Arrow
import Control.Category

import Data.Invertible.Bijection
#ifdef VERSION_semigroupoids
import Data.Semigroupoid (Semigroupoid(..))
import Data.Groupoid (Groupoid(..))
import qualified Data.Isomorphism as Semigroupoid
#define SemigroupoidArrowA (Semigroupoid a, Arrow a)
#else
#define SemigroupoidArrowA Arrow a
#endif
#ifdef VERSION_TypeCompose
import qualified Data.Bijection as TypeCompose
#endif
#ifdef VERSION_partial_isomorphisms
import qualified Control.Isomorphism.Partial as Partial
import qualified Control.Isomorphism.Partial.Unsafe as Partial
#endif
#ifdef VERSION_arrows
import qualified Control.Arrow.Transformer.All as T
#endif

infix 2 <->

-- |The bidirectional arrow class.
--
-- Instances should satisfy the following laws:
--
--  * @f1 \<-\> g2 >>> g1 \<-\> f2 = (f1 >>> g1) \<-\> (f2 >>> g2)@
--  * @invert (invert f) = f@
--  * @invert (f \<-\> g) = g \<-\> f@
--  * @first (f \<-\> g) = f *** id \<-\> g *** id@
--  * @first h >>> id *** f \<-\> id *** g = id *** f \<-\> id *** g >>> first h@
--  * @first (first f) >>> assoc = assoc >>> first f@
--
-- where @assoc = ['Data.Invertible.TH.biCase'|((x,y),z) \<-\> (x,(y,z))|]@
class (
#ifdef VERSION_semigroupoids
    Groupoid a,
#endif
    Category a) => BiArrow a where
  -- |Take two functions and lift them into a bidirectional arrow.
  -- The intention is that these functions are each other's inverse.
  (<->) :: (b -> c) -> (c -> b) -> a b c
  -- |Inverse: reverse the direction of a bidirectional arrow.
  invert :: a b c -> a c b
#ifdef VERSION_semigroupoids
  invert = inv
#endif

-- |Bidirectional arrows under 'Arrow'.
--
-- Although 'BiArrow' should not, strictly speaking, be a subclass of 'Arrow' (as it is often impossible to define 'arr'), this is done because (as the paper says) \"conceptually bi-arrows form an extension of the arrow class. Moreover, it allows us to use bi-arrows as normal arrows.\"  This class exists to register this confound.
class (BiArrow a, Arrow a) => BiArrow' a

-- |Lift a bidirectional function to an arbitrary arrow using '<->'.
biarr :: BiArrow a => (b <-> c) -> a b c
biarr (f :<->: g) = f <-> g

-- |Construct an involution (a biarrow where the function and inverse are the same).
involve :: BiArrow a => (b -> b) -> a b b
involve f = f <-> f

infixr 1 ^^>>, >>^^
infixr 1 ^^<<, <<^^

-- | Precomposition with a pure bijection.
(^^>>) :: BiArrow a => (b <-> c) -> a c d -> a b d
f ^^>> a = biarr f >>> a

-- | Postcomposition with a pure bijection.
(>>^^) :: BiArrow a => a b c -> (c <-> d) -> a b d
a >>^^ f = a >>> biarr f

-- | Precomposition with a pure bijection (right-to-left variant).
(<<^^) :: BiArrow a => a c d -> (b <-> c) -> a b d
a <<^^ f = a <<< biarr f

-- | Postcomposition with a pure bijection (right-to-left variant).
(^^<<) :: BiArrow a => (c <-> d) -> a b c -> a b d
f ^^<< a = biarr f <<< a

instance SemigroupoidArrowA => BiArrow (Bijection a) where
  f <-> g = arr f :<->: arr g
  invert (f :<->: g) = g :<->: f

instance SemigroupoidArrowA => BiArrow' (Bijection a)

-- |Bidirectional 'Control.Arrow.Kleisli' monad arrow transformer.
type BiKleisli m = Bijection (Kleisli m)

#ifdef VERSION_semigroupoids
instance (Semigroupoid a, Arrow a) => BiArrow (Semigroupoid.Iso a) where
  f <-> g = Semigroupoid.Iso (arr f) (arr g)
#endif

#ifdef VERSION_TypeCompose
#ifdef VERSION_semigroupoids
-- |Poor orphans.  Please will someone adopt us?
instance Semigroupoid a => Semigroupoid (TypeCompose.Bijection a) where
  TypeCompose.Bi f1 g1 `o` TypeCompose.Bi f2 g2 = TypeCompose.Bi (o f1 f2) (o g2 g1)
-- |Poor orphans.  Please will someone adopt us?
instance Semigroupoid a => Groupoid (TypeCompose.Bijection a) where
  inv = TypeCompose.inverse
#endif
instance SemigroupoidArrowA => BiArrow (TypeCompose.Bijection a) where
  f <-> g = TypeCompose.Bi (arr f) (arr g)
  invert = TypeCompose.inverse
instance SemigroupoidArrowA => BiArrow' (TypeCompose.Bijection a)
#endif

#ifdef VERSION_partial_isomorphisms
#ifdef VERSION_semigroupoids
-- |Poor orphans.  Please will someone adopt us?
instance Semigroupoid Partial.Iso where
  o = (.)
-- |Poor orphans.  Please will someone adopt us?
instance Groupoid Partial.Iso where
  inv = Partial.inverse
#endif
instance BiArrow Partial.Iso where
  f <-> g = Partial.Iso (Just . f) (Just . g)
  invert = Partial.inverse
#endif

#ifdef VERSION_arrows
#ifdef VERSION_semigroupoids
-- |Poor orphans.  Please will someone adopt us?
instance Semigroupoid a => Semigroupoid (T.StateArrow s a) where
  T.StateArrow f `o` T.StateArrow g = T.StateArrow (f `o` g)
-- |Poor orphans.  Please will someone adopt us?
instance Groupoid a => Groupoid (T.StateArrow s a) where
  inv (T.StateArrow f) = T.StateArrow (inv f)
-- |Poor orphans.  Please will someone adopt us?
instance Semigroupoid a => Semigroupoid (T.CoStateArrow s a) where
  T.CoStateArrow f `o` T.CoStateArrow g = T.CoStateArrow (f `o` g)
-- |Poor orphans.  Please will someone adopt us?
instance Groupoid a => Groupoid (T.CoStateArrow s a) where
  inv (T.CoStateArrow f) = T.CoStateArrow (inv f)
-- |Poor orphans.  Please will someone adopt us?
instance Semigroupoid a => Semigroupoid (T.StreamArrow a) where
  T.StreamArrow f `o` T.StreamArrow g = T.StreamArrow (f `o` g)
-- |Poor orphans.  Please will someone adopt us?
instance Groupoid a => Groupoid (T.StreamArrow a) where
  inv (T.StreamArrow f) = T.StreamArrow (inv f)
#endif
instance (Arrow a, BiArrow a) => BiArrow (T.StateArrow s a) where
  f <-> g = T.StateArrow (first $ f <-> g)
  invert (T.StateArrow f) = T.StateArrow (invert f)
instance BiArrow' a => BiArrow' (T.StateArrow s a)
instance BiArrow a => BiArrow (T.CoStateArrow s a) where
  f <-> g = T.CoStateArrow ((f .) <-> (g .))
  invert (T.CoStateArrow f) = T.CoStateArrow (invert f)
instance BiArrow' a => BiArrow' (T.CoStateArrow s a)
instance BiArrow a => BiArrow (T.StreamArrow a) where
  f <-> g = T.StreamArrow (fmap f <-> fmap g)
  invert (T.StreamArrow f) = T.StreamArrow (invert f)
instance BiArrow' a => BiArrow' (T.StreamArrow a)
#endif
