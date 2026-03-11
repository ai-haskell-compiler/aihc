module Bowtie.Anno
  ( Anno (..)
  , annoUnit
  , annoUnitM
  , annoCounit
  , annoCounitM
  , annoLeft
  , annoLeftM
  , annoRight
  , annoRightM
  )
where

import Control.Comonad (Comonad (..))
import Control.Exception (Exception)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Functor.Apply (Apply (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Prettyprinter (Pretty (..))

-- | An "annotation" with associated value.
type Anno :: Type -> Type -> Type
data Anno k v = Anno {annoKey :: !k, annoVal :: !v}
  deriving stock (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor Anno where
  bimap f g (Anno k v) = Anno (f k) (g v)

instance Bifoldable Anno where
  bifoldr f g z (Anno k v) = f k (g v z)

instance Bitraversable Anno where
  bitraverse f g (Anno k v) = liftA2 Anno (f k) (g v)

instance (Semigroup k) => Apply (Anno k) where
  liftF2 f (Anno k1 v1) (Anno k2 v2) = Anno (k1 <> k2) (f v1 v2)

instance (Monoid k) => Applicative (Anno k) where
  pure = Anno mempty
  liftA2 = liftF2

instance Comonad (Anno k) where
  extract (Anno _ v) = v
  duplicate an@(Anno k _) = Anno k an
  extend f an@(Anno k _) = Anno k (f an)

instance (Pretty v) => Pretty (Anno k v) where
  pretty = pretty . annoVal

instance (Monoid k, IsString v) => IsString (Anno k v) where
  fromString = Anno mempty . fromString

instance (Show k, Typeable k, Exception v) => Exception (Anno k v)

-- | 'unit' from 'Adjunction'
annoUnit :: v -> Reader k (Anno k v)
annoUnit v = ReaderT (Identity . (`Anno` v))

annoUnitM :: (Applicative m) => v -> ReaderT k m (Anno k v)
annoUnitM v = ReaderT (pure . (`Anno` v))

-- | 'counit' from 'Adjunction'
annoCounit :: Anno k (Reader k v) -> v
annoCounit (Anno k m) = runReader m k

annoCounitM :: Anno k (ReaderT k m v) -> m v
annoCounitM (Anno k m) = runReaderT m k

-- | 'leftAdjunct' from 'Adjunction'
annoLeft :: (Anno k v -> x) -> v -> Reader k x
annoLeft f v = ReaderT (Identity . f . (`Anno` v))

annoLeftM :: (Anno k v -> m x) -> v -> ReaderT k m x
annoLeftM f v = ReaderT (f . (`Anno` v))

-- | 'rightAdjunct' from 'Adjunction'
annoRight :: (v -> Reader k x) -> Anno k v -> x
annoRight f (Anno k v) = runReader (f v) k

annoRightM :: (v -> ReaderT k m x) -> Anno k v -> m x
annoRightM f (Anno k v) = runReaderT (f v) k
