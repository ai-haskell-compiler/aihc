{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-dodgy-imports #-}

-- |
-- Module      :  Control.Monad.Trans.Identity.Tagged
-- Copyright   :  © 2016–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The library provides a monad transformer that works just like
-- 'IdentityT', but can be tagged at the type level. This allows us to work
-- with monad stacks as usual, except we can make two identical monad stacks
-- to have different types. The main application for this is, of course, the
-- ability to have different instances for otherwise identical stacks
-- without having to do @newtype@ wrapping.
module Control.Monad.Trans.Identity.Tagged
  ( -- * The tagged identity monad transformer
    TaggedT (..),
    mapTaggedT,

    -- * Lifting other operations
    liftCallCC,
    liftCatch,
  )
where

import Control.Applicative
import Control.Monad (MonadPlus (..))
import Control.Monad.Cont.Class hiding (liftCallCC)
import Control.Monad.Error.Class
import Control.Monad.Fail qualified as Fail
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Zip (MonadZip (..))
import Data.Functor.Classes

-- | Identity monad transformer with a type-level tag.
newtype TaggedT tag f a = TaggedT {runTaggedT :: f a}

----------------------------------------------------------------------------
-- Standard instances

instance (Eq1 f) => Eq1 (TaggedT tag f) where
  liftEq eq (TaggedT x) (TaggedT y) = liftEq eq x y
  {-# INLINE liftEq #-}

instance (Ord1 f) => Ord1 (TaggedT tag f) where
  liftCompare comp (TaggedT x) (TaggedT y) = liftCompare comp x y
  {-# INLINE liftCompare #-}

instance (Read1 f) => Read1 (TaggedT tag f) where
  liftReadsPrec rp rl =
    readsData $
      readsUnaryWith (liftReadsPrec rp rl) "TaggedT" TaggedT

instance (Show1 f) => Show1 (TaggedT tag f) where
  liftShowsPrec sp sl d (TaggedT m) =
    showsUnaryWith (liftShowsPrec sp sl) "TaggedT" d m

instance (Eq1 f, Eq a) => Eq (TaggedT tag f a) where
  (==) = eq1

instance (Ord1 f, Ord a) => Ord (TaggedT tag f a) where
  compare = compare1

instance (Read1 f, Read a) => Read (TaggedT tag f a) where
  readsPrec = readsPrec1

instance (Show1 f, Show a) => Show (TaggedT tag f a) where
  showsPrec = showsPrec1

instance (Functor m) => Functor (TaggedT tag m) where
  fmap f = mapTaggedT (fmap f)
  {-# INLINE fmap #-}

instance (Foldable f) => Foldable (TaggedT tag f) where
  foldMap f (TaggedT a) = foldMap f a
  {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (TaggedT tag f) where
  traverse f (TaggedT a) = TaggedT <$> traverse f a
  {-# INLINE traverse #-}

instance (Applicative m) => Applicative (TaggedT tag m) where
  pure x = TaggedT (pure x)
  {-# INLINE pure #-}
  (<*>) = lift2TaggedT (<*>)
  {-# INLINE (<*>) #-}
  (*>) = lift2TaggedT (*>)
  {-# INLINE (*>) #-}

instance (Alternative m) => Alternative (TaggedT tag m) where
  empty = TaggedT empty
  {-# INLINE empty #-}
  (<|>) = lift2TaggedT (<|>)
  {-# INLINE (<|>) #-}

instance (Monad m) => Monad (TaggedT tag m) where
  m >>= k = TaggedT $ runTaggedT . k =<< runTaggedT m
  {-# INLINE (>>=) #-}

instance (Fail.MonadFail m) => Fail.MonadFail (TaggedT tag m) where
  fail msg = TaggedT $ Fail.fail msg
  {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (TaggedT tag m) where
  mzero = TaggedT mzero
  {-# INLINE mzero #-}
  mplus = lift2TaggedT mplus
  {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (TaggedT tag m) where
  mfix f = TaggedT (mfix (runTaggedT . f))
  {-# INLINE mfix #-}

instance (MonadIO m) => MonadIO (TaggedT tag m) where
  liftIO = TaggedT . liftIO
  {-# INLINE liftIO #-}

instance (MonadZip m) => MonadZip (TaggedT tag m) where
  mzipWith f = lift2TaggedT (mzipWith f)
  {-# INLINE mzipWith #-}

instance MonadTrans (TaggedT tag) where
  lift = TaggedT
  {-# INLINE lift #-}

-- | Lift a unary operation to the new monad.
mapTaggedT :: (m a -> n b) -> TaggedT tag m a -> TaggedT tag n b
mapTaggedT f = TaggedT . f . runTaggedT
{-# INLINE mapTaggedT #-}

-- | Lift a binary operation to the new monad.
lift2TaggedT :: (m a -> n b -> p c) -> TaggedT tag m a -> TaggedT tag n b -> TaggedT tag p c
lift2TaggedT f a b = TaggedT (f (runTaggedT a) (runTaggedT b))
{-# INLINE lift2TaggedT #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m a b -> CallCC (TaggedT tag m) a b
liftCallCC callCC' f =
  TaggedT $ callCC' $ \c -> runTaggedT (f (TaggedT . c))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m a -> Catch e (TaggedT tag m) a
liftCatch f m h = TaggedT $ f (runTaggedT m) (runTaggedT . h)
{-# INLINE liftCatch #-}

----------------------------------------------------------------------------
-- MTL instances

instance (MonadCont m) => MonadCont (TaggedT tag m) where
  callCC = liftCallCC callCC

instance (MonadError e m) => MonadError e (TaggedT tag m) where
  throwError = lift . throwError
  catchError = liftCatch catchError

instance (MonadRWS r w s m) => MonadRWS r w s (TaggedT tag m)

instance (MonadReader r m) => MonadReader r (TaggedT tag m) where
  ask = lift ask
  local = mapTaggedT . local
  reader = lift . reader

instance (MonadState s m) => MonadState s (TaggedT tag m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadWriter w m) => MonadWriter w (TaggedT tag m) where
  writer = lift . writer
  tell = lift . tell
  listen = mapTaggedT listen
  pass = mapTaggedT pass
