{-# LANGUAGE UndecidableInstances #-}

module Bowtie.Memo
  ( MemoF (.., MemoFP)
  , memoFKey
  , memoFVal
  , Memo (.., MemoP)
  , mkMemo
  , mkMemoM
  , reMkMemo
  , reMkMemoM
  , unMkMemo
  , transMemo
  , memoKey
  , memoVal
  , memoCata
  , memoCataM
  , memoRight
  , memoRightM
  , memoExtend
  , memoFix
  , memoStructEq
  )
where

import Bowtie.Anno (Anno (..), annoRight, annoRightM)
import Bowtie.Fix (Fix)
import Bowtie.Foldable (cataM)
import Control.Monad.Reader (Reader, ReaderT (..), runReader)
import Data.Functor.Apply (Apply (..))
import Data.Functor.Foldable (Base, Corecursive (..), Recursive (..))
import Data.Kind (Type)
import Data.String (IsString (..))
import Prettyprinter (Pretty (..))

-- | The base functor for a 'Memo'
newtype MemoF f k r = MemoF {unMemoF :: Anno k (f r)}
  deriving stock (Show, Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord)

pattern MemoFP :: k -> f r -> MemoF f k r
pattern MemoFP k v = MemoF (Anno k v)

{-# COMPLETE MemoFP #-}

deriving newtype instance (Monoid k, IsString (f r)) => IsString (MemoF f k r)

deriving newtype instance (Pretty (f r)) => Pretty (MemoF f k r)

instance (Apply f, Semigroup k) => Apply (MemoF f k) where
  liftF2 f (MemoF (Anno k1 v1)) (MemoF (Anno k2 v2)) = MemoF (Anno (k1 <> k2) (liftF2 f v1 v2))

instance (Applicative f, Monoid k) => Applicative (MemoF f k) where
  pure = MemoF . Anno mempty . pure
  liftA2 f (MemoF (Anno k1 v1)) (MemoF (Anno k2 v2)) = MemoF (Anno (k1 <> k2) (liftA2 f v1 v2))

memoFKey :: MemoF f k r -> k
memoFKey (MemoFP k _) = k

memoFVal :: MemoF f k r -> f r
memoFVal (MemoFP _ v) = v

-- | An annotated 'Fix'
type Memo :: (Type -> Type) -> Type -> Type
newtype Memo f k = Memo {unMemo :: MemoF f k (Memo f k)}

pattern MemoP :: k -> f (Memo f k) -> Memo f k
pattern MemoP k v = Memo (MemoF (Anno k v))

{-# COMPLETE MemoP #-}

deriving newtype instance (Eq k, Eq (f (Memo f k))) => Eq (Memo f k)

deriving newtype instance (Ord k, Ord (f (Memo f k))) => Ord (Memo f k)

deriving stock instance (Show k, Show (f (Memo f k))) => Show (Memo f k)

deriving newtype instance (Monoid k, IsString (f (Memo f k))) => IsString (Memo f k)

deriving newtype instance (Pretty (f (Memo f k))) => Pretty (Memo f k)

instance (Functor f) => Functor (Memo f) where
  fmap f = go where go (MemoP k v) = MemoP (f k) (fmap go v)

instance (Foldable f) => Foldable (Memo f) where
  foldr f = flip go where go (MemoP k v) z = foldr go (f k z) v

instance (Traversable f) => Traversable (Memo f) where
  traverse f = go where go (MemoP k v) = liftA2 MemoP (f k) (traverse go v)

type instance Base (Memo f k) = MemoF f k

instance (Functor f) => Recursive (Memo f k) where project = unMemo

instance (Functor f) => Corecursive (Memo f k) where embed = Memo

-- | Pull a recursive structure apart and retie as a 'Memo', using the given
-- function to calculate a key for every level.
mkMemo :: (Recursive t, Base t ~ f) => (f k -> k) -> t -> Memo f k
mkMemo f = cata (\v -> MemoP (f (fmap memoKey v)) v)

mkMemoM :: (Monad m, Recursive t, Base t ~ f, Traversable f) => (f k -> m k) -> t -> m (Memo f k)
mkMemoM f = cataM (\v -> fmap (`MemoP` v) (f (fmap memoKey v)))

-- | Rebuild a memo with a new annotation.
reMkMemo :: (Functor f) => (MemoF f j (Memo f k) -> k) -> Memo f j -> Memo f k
reMkMemo f = go
 where
  go (MemoP j fmj) =
    let fmk = fmap go fmj
        k = f (MemoFP j fmk)
    in  MemoP k fmk

-- | Rebuild a memo with a new annotation, effectfully.
reMkMemoM :: (Traversable f, Monad m) => (MemoF f j (Memo f k) -> m k) -> Memo f j -> m (Memo f k)
reMkMemoM f = go
 where
  go (MemoP j fmj) = do
    fmk <- traverse go fmj
    k <- f (MemoFP j fmk)
    pure (MemoP k fmk)

-- | Forget keys at every level and convert back to a plain structure.
unMkMemo :: (Corecursive t, Base t ~ f) => Memo f k -> t
unMkMemo (MemoP _ v) = embed (fmap unMkMemo v)

-- | Transform the base functor.
transMemo :: (Functor f) => (forall x. f x -> g x) -> Memo f k -> Memo g k
transMemo nat = go
 where
  go (MemoP k v) = MemoP k (nat (fmap go v))

memoKey :: Memo f k -> k
memoKey (MemoP k _) = k

memoVal :: Memo f k -> f (Memo f k)
memoVal (MemoP _ v) = v

-- | 'cata' but nicer
memoCata :: (Functor f) => (f x -> Reader k x) -> Memo f k -> x
memoCata f = go
 where
  go (MemoP k v) = runReader (f (fmap go v)) k

-- | 'cataM' but nicer
memoCataM :: (Monad m, Traversable f) => (f x -> ReaderT k m x) -> Memo f k -> m x
memoCataM f = go
 where
  go (MemoP k v) = traverse go v >>= \x -> runReaderT (f x) k

-- | Peek at the top value like 'annoRight'
memoRight :: (f (Memo f k) -> Reader k x) -> Memo f k -> x
memoRight f = annoRight f . unMemoF . unMemo

-- | Peek at the top value like 'annoRightM'
memoRightM :: (f (Memo f k) -> ReaderT k m x) -> Memo f k -> m x
memoRightM f = annoRightM f . unMemoF . unMemo

-- | Re-annotate top-down
memoExtend :: (Functor f) => (Memo f k -> x) -> Memo f k -> Memo f x
memoExtend w = go where go m@(MemoP _ v) = MemoP (w m) (fmap go v)

-- | Forget annotations (useful because the type annotation is tricky)
memoFix :: (Functor f) => Memo f k -> Fix f
memoFix = unMkMemo

-- | Structural equality, ignoring annotations
memoStructEq :: (Functor f, Eq (f (Fix f))) => Memo f k -> Memo f k -> Bool
memoStructEq m1 m2 = memoFix m1 == memoFix m2
