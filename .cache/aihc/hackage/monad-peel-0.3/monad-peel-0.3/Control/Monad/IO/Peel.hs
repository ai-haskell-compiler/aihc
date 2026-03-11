{-# LANGUAGE CPP #-}
{- |
Module      :  Control.Monad.IO.Peel
Copyright   :  © Anders Kaseorg, 2010
License     :  BSD-style

Maintainer  :  Anders Kaseorg <andersk@mit.edu>
Stability   :  experimental
Portability :  portable

This module defines the class 'MonadPeelIO' of 'IO'-based monads into
which control operations on 'IO' (such as exception catching; see
"Control.Exception.Peel") can be lifted.

'liftIOOp' and 'liftIOOp_' enable convenient lifting of two common
special cases of control operation types.
-}

module Control.Monad.IO.Peel (
  MonadPeelIO(..),
  liftIOOp,
  liftIOOp_,
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Peel
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
#if MIN_VERSION_transformers(0,4,0)
import qualified Control.Monad.Trans.Except as Except
#endif
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer
import qualified Control.Monad.Trans.Writer.Strict as Strict
import qualified Control.Monad.Trans.RWS as RWS
import qualified Control.Monad.Trans.RWS.Strict as RWS.Strict
import Data.Monoid

-- |@MonadPeelIO@ is the class of 'IO'-based monads supporting an
-- extra operation 'peelIO', enabling control operations on 'IO' to be
-- lifted into the monad.
class MonadIO m => MonadPeelIO m where
  -- |@peelIO@ is a version of 'peel' that operates through an
  -- arbitrary stack of monad transformers directly to an inner 'IO'
  -- (analagously to how 'liftIO' is a version of @lift@).  So it can
  -- be used with 'liftIO' to lift control operations on 'IO' into any
  -- monad in 'MonadPeelIO'.  For example:
  --
  -- @
  --    foo :: 'IO' a -> 'IO' a
  --    foo' :: 'MonadPeelIO' m => m a -> m a
  --    foo' a = do
  --      k \<- 'peelIO'  -- k :: m a -> IO (m a)
  --      'join' $ 'liftIO' $ foo (k a)  -- uses foo :: 'IO' (m a) -> 'IO' (m a)
  -- @
  --
  -- Note that the \"obvious\" term of this type (@peelIO = 'return'
  -- 'return'@) /does not/ work correctly.  Instances of 'MonadPeelIO'
  -- should be constructed via 'MonadTransPeel', using @peelIO =
  -- 'liftPeel' peelIO@.
  peelIO :: m (m a -> IO (m a))

instance MonadPeelIO IO where
  peelIO = idPeel

instance MonadPeelIO m => MonadPeelIO (IdentityT m) where
  peelIO = liftPeel peelIO
instance MonadPeelIO m => MonadPeelIO (MaybeT m) where
  peelIO = liftPeel peelIO
#if MIN_VERSION_transformers(0,4,0)
instance MonadPeelIO m => MonadPeelIO (Except.ExceptT e m) where
  peelIO = liftPeel peelIO
#endif
instance MonadPeelIO m => MonadPeelIO (ReaderT r m) where
  peelIO = liftPeel peelIO
instance MonadPeelIO m => MonadPeelIO (StateT s m) where
  peelIO = liftPeel peelIO
instance MonadPeelIO m => MonadPeelIO (Strict.StateT s m) where
  peelIO = liftPeel peelIO
instance (Monoid w, MonadPeelIO m) => MonadPeelIO (WriterT w m) where
  peelIO = liftPeel peelIO
instance (Monoid w, MonadPeelIO m) => MonadPeelIO (Strict.WriterT w m) where
  peelIO = liftPeel peelIO
instance (Monoid w, MonadPeelIO m) => MonadPeelIO (RWS.RWST r w s m) where
  peelIO = liftPeel peelIO
instance (Monoid w, MonadPeelIO m) =>
         MonadPeelIO (RWS.Strict.RWST r w s m) where
  peelIO = liftPeel peelIO


-- |@liftIOOp@ is a particular application of 'peelIO' that allows
-- lifting control operations of type @(a -> 'IO' b) -> 'IO' b@
-- (e.g. @alloca@, @withMVar v@) to @'MonadPeelIO' m => (a -> m b) ->
-- m b@.
--
-- @
--    'liftIOOp' f g = do
--      k \<- 'peelIO'
--      'join' $ 'liftIO' $ f (k . g)
-- @
liftIOOp :: MonadPeelIO m => ((a -> IO (m b)) -> IO (m c)) -> (a -> m b) -> m c
liftIOOp f g = do
  k <- peelIO
  join $ liftIO $ f (k . g)

-- |@liftIOOp_@ is a particular application of 'peelIO' that allows
-- lifting control operations of type @'IO' a -> 'IO' a@
-- (e.g. @block@) to @'MonadPeelIO' m => m a -> m a@.
--
-- @
--    'liftIOOp_' f m = do
--      k \<- 'peelIO'
--      'join' $ 'liftIO' $ f (k m)
-- @
liftIOOp_ :: MonadPeelIO m => (IO (m a) -> IO (m b)) -> m a -> m b
liftIOOp_ f m = do
  k <- peelIO
  join $ liftIO $ f (k m)
