{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delegate where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

class Monad m => MonadDelegate r m | m -> r where
    -- | The inverse of 'delegate' is 'bind'
    --
    -- @
    -- (>>=) :: Monad m => m a -> (a -> m r) -> m r
    -- @
    delegate :: ((a -> m r) -> m r) -> m a

-- | Instance that does real work using continuations
instance Monad m => MonadDelegate r (ContT r m) where
    delegate f = ContT $ \k -> evalContT $ f (lift . k)

-- | Passthrough instance
instance (MonadDelegate r m) => MonadDelegate r (IdentityT m) where
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)

-- | Passthrough instance
instance (MonadDelegate r m) => MonadDelegate r (ReaderT env m) where
    delegate f = ReaderT $ \r -> delegate $ \k -> (`runReaderT` r) $ f (lift . k)

-- | Passthrough instance
instance (MonadDelegate r m) => MonadDelegate r (MaybeT m) where
    delegate f = MaybeT . delegate $ \k -> do
        a <- runMaybeT . f $ lift . k . Just
        case a of
            Nothing -> k Nothing
            Just a' -> pure a'

-- | Passthrough instance
instance (MonadDelegate r m) => MonadDelegate r (ExceptT e m) where
    delegate f = ExceptT . delegate $ \kea -> do
        e <- runExceptT . f $ lift . kea . Right -- m (Either e a)
        case e of
            Left e' -> kea (Left e')
            Right r -> pure r

-- | Only handle with given monad, and ignore anything else.
-- This means subseqent fmap, aps, binds are always ignored.
-- @forall@ so @TypeApplications@ can be used to specify the type of @a@
finish :: forall a r m. MonadDelegate r m => m r -> m a
finish = delegate . const

-- | Convert two handler to a monad that may fire two possibilities
-- The inverse is 'bind2'.
multitask :: MonadDelegate r m => ((a -> m r) -> (b -> m r) -> m r) -> m (Either a b)
multitask g = delegate $ \fab -> g (fab . Left) (fab . Right)

-- | Convert a monad that fires two possibilites to a two handlers.
bind2 :: Monad m => m (Either a b) -> (a -> m r) -> (b -> m r) -> m r
bind2 m fa fb = m >>= either fa fb

-- | 'bind' only the 'Right' possibility.
bindRight :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a c)
bindRight m k = bind2 m (pure . Left) (fmap Right . k)

-- | 'bind' only the 'Left' possibility.
bindLeft :: Monad m => m (Either a b) -> (a -> m c) -> m (Either c b)
bindLeft m k = bind2 m (fmap Left . k) (pure . Right)

-- | finish the 'Left' possibility
finishLeft :: MonadDelegate r m => m (Either r b) -> m b
finishLeft m = m >>= either (finish . pure) pure

-- | finish the 'Right' possibility
finishRight :: MonadDelegate r m => m (Either a r) -> m a
finishRight m = m >>= either pure (finish . pure)

-- | maybe 'delegate' the Just value, or just use the @r@.
maybeDelegate :: MonadDelegate r m => r -> m (Maybe a) -> m a
maybeDelegate r m = delegate $ \fire -> do
    ma <- m
    case ma of
        Nothing -> pure r
        Just a -> fire a
