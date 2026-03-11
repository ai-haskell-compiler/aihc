{-# LANGUAGE UndecidableInstances #-}

module Bowtie.Jot
  ( JotF (.., JotFP)
  , jotFKey
  , jotFVal
  , Jot (.., JotP)
  , mkJot
  , unMkJot
  , annoJot
  , transJot
  , jotKey
  , jotVal
  , jotCata
  , jotCataM
  , jotRight
  , jotRightM
  , jotExtend
  )
where

import Bowtie.Anno (Anno (..), annoRight, annoRightM)
import Bowtie.Foldable (Base1, Corecursive1 (..), Recursive1 (..), cata1, fmapViaBi, foldrViaBi, traverseViaBi)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Prettyprinter (Pretty (..))

-- | The base functor for a 'Jot'
newtype JotF g k a r = JotF {unJotF :: Anno k (g a r)}
  deriving stock (Show, Functor)
  deriving newtype (Eq, Ord)

pattern JotFP :: k -> g a r -> JotF g k a r
pattern JotFP k v = JotF (Anno k v)

{-# COMPLETE JotFP #-}

deriving newtype instance (Monoid k, IsString (g a r)) => IsString (JotF g k a r)

deriving newtype instance (Pretty (g a r)) => Pretty (JotF g k a r)

instance (Bifunctor g) => Bifunctor (JotF g k) where
  bimap f g = go where go = JotF . fmap (bimap f g) . unJotF

instance (Bifoldable g) => Bifoldable (JotF g k) where
  bifoldr f g = go where go z = bifoldr f g z . annoVal . unJotF

instance (Bitraversable g) => Bitraversable (JotF g k) where
  bitraverse f g = go where go = fmap JotF . traverse (bitraverse f g) . unJotF

jotFKey :: JotF g k a r -> k
jotFKey (JotFP k _) = k

jotFVal :: JotF g k a r -> g a r
jotFVal (JotFP _ v) = v

-- | An annotated 'Knot'
type Jot :: (Type -> Type -> Type) -> Type -> Type -> Type
newtype Jot g k a = Jot {unJot :: JotF g k a (Jot g k a)}

pattern JotP :: k -> g a (Jot g k a) -> Jot g k a
pattern JotP k v = Jot (JotF (Anno k v))

{-# COMPLETE JotP #-}

deriving newtype instance (Eq k, Eq (g a (Jot g k a))) => Eq (Jot g k a)

deriving newtype instance (Ord k, Ord (g a (Jot g k a))) => Ord (Jot g k a)

deriving stock instance (Show k, Show (g a (Jot g k a))) => Show (Jot g k a)

deriving newtype instance (Monoid k, IsString (g a (Jot g k a))) => IsString (Jot g k a)

deriving newtype instance (Pretty (g a (Jot g k a))) => Pretty (Jot g k a)

type instance Base1 (Jot g k) = JotF g k

instance (Bifunctor g) => Recursive1 (Jot g k) where project1 = unJot

instance (Bifunctor g) => Corecursive1 (Jot g k) where embed1 = Jot

instance (Bifunctor g) => Functor (Jot g k) where fmap = fmapViaBi

instance (Bifunctor g, Bifoldable g) => Foldable (Jot g k) where foldr = foldrViaBi

instance (Bitraversable g) => Traversable (Jot g k) where traverse = traverseViaBi

instance (Bifunctor g) => Bifunctor (Jot g) where
  bimap f g = go where go (JotP k v) = JotP (f k) (bimap g go v)

instance (Bifoldable g) => Bifoldable (Jot g) where
  bifoldr f g = flip go where go (JotP k v) z = f k (bifoldr g go z v)

instance (Bitraversable g) => Bitraversable (Jot g) where
  bitraverse f g = go where go (JotP k v) = liftA2 JotP (f k) (bitraverse g go v)

-- | Pull a recursive structure apart and retie as a 'Jot', using the given
-- function to calculate a key for every level.
mkJot :: (Recursive1 t, Base1 t ~ g) => (g a k -> k) -> t a -> Jot g k a
mkJot f = cata1 (\v -> JotP (f (fmap jotKey v)) v)

-- | Forget keys at every level and convert back to a plain structure.
unMkJot :: (Corecursive1 t, Base1 t ~ g) => Jot g k a -> t a
unMkJot (JotP _ v) = embed1 (fmap unMkJot v)

-- | Quick conversion from annotated functor.
annoJot :: Anno b (g a (Jot g b a)) -> Jot g b a
annoJot = Jot . JotF

-- | Transform the base functor.
transJot :: (Bifunctor g) => (forall x. g a x -> h a x) -> Jot g k a -> Jot h k a
transJot nat = go
 where
  go (JotP k v) = JotP k (nat (second go v))

jotKey :: Jot g k a -> k
jotKey (JotP k _) = k

jotVal :: Jot g k a -> g a (Jot g k a)
jotVal (JotP _ v) = v

-- | 'cata' but nicer
jotCata :: (Bifunctor g) => (g a x -> Reader k x) -> Jot g k a -> x
jotCata f = go
 where
  go (JotP k v) = runReader (f (fmap go v)) k

-- | 'cataM' but nicer
jotCataM :: (Bifunctor g) => (g a (m x) -> ReaderT k m x) -> Jot g k a -> m x
jotCataM f = go
 where
  go (JotP k v) = runReaderT (f (fmap go v)) k

-- | Peek at the top value like 'annoRight'
jotRight :: (g a (Jot g k a) -> Reader k x) -> Jot g k a -> x
jotRight f = annoRight f . unJotF . unJot

-- | Peek at the top value like 'annoRightM'
jotRightM :: (g a (Jot g k a) -> ReaderT k m x) -> Jot g k a -> m x
jotRightM f = annoRightM f . unJotF . unJot

-- | Re-annotate top-down
jotExtend :: (Bifunctor g) => (Jot g k a -> x) -> Jot g k a -> Jot g x a
jotExtend w = go where go j@(JotP _ v) = JotP (w j) (fmap go v)
