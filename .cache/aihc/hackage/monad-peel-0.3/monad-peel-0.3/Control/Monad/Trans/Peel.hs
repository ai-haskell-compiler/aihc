{-# LANGUAGE CPP #-}
{- |
Module      :  Control.Monad.Trans.Peel
Copyright   :  © Anders Kaseorg, 2010
License     :  BSD-style

Maintainer  :  Anders Kaseorg <andersk@mit.edu>
Stability   :  experimental
Portability :  portable

This module defines the class 'MonadTransPeel' of monad transformers
through which control operations can be lifted.  Instances are
included for all the standard monad transformers from the
@transformers@ library except @ContT@.

'idPeel' and 'liftPeel' are provided to assist creation of
@MonadPeelIO@-like classes (see "Control.Monad.IO.Peel") based on core
monads other than 'IO'.

'liftOp' and 'liftOp_' enable convenient lifting of two common special
cases of control operation types.
-}

module Control.Monad.Trans.Peel (
  MonadTransPeel(..),
  idPeel,
  liftPeel,
  liftOp,
  liftOp_,
  ) where

import Prelude hiding (catch)
import Control.Monad
import Control.Monad.Trans.Class
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


-- |@MonadTransPeel@ is the class of monad transformers supporting an
-- extra operation 'peel', enabling control operations (functions that
-- use monadic actions as input instead of just output) to be lifted
-- through the transformer.
class MonadTrans t => MonadTransPeel t where
  -- |@peel@ is used to peel off the outer layer of a transformed
  -- monadic action, allowing an transformed action @t m a@ to be
  -- treated as a base action @m b@.
  --
  -- More precisely, @peel@ captures the monadic state of @t@ at the
  -- point where it is bound (in @t n@), yielding a function @t m a ->
  -- m (t o a)@; this function runs a transformed monadic action @t m
  -- a@ in the base monad @m@ using the captured state, and leaves the
  -- result @t o a@ in the monad @m@ after all side effects in @m@
  -- have occurred.
  --
  -- This can be used together with 'lift' to lift control operations
  -- with types such as @M a -> M a@ into the transformed monad @t M@:
  --
  -- @
  --    instance Monad M
  --    foo :: M a -> M a
  --    foo' :: ('MonadTransPeel' t, 'Monad' (t M)) => t M a -> t M a
  --    foo' a = do
  --      k \<- 'peel'  -- k :: t M a -> M (t M a)
  --      'join' $ 'lift' $ foo (k a)  -- uses foo :: M (t M a) -> M (t M a)
  -- @
  --
  -- @peel@ is typically used with @m == n == o@, but is required to
  -- be polymorphic for greater type safety: for example, this type
  -- ensures that the result of running the action in @m@ has no
  -- remaining side effects in @m@.
  peel :: (Monad m, Monad n, Monad o) => t n (t m a -> m (t o a))

instance MonadTransPeel IdentityT where
  peel = return $ \m -> do
    x <- runIdentityT m
    return $ return x

instance MonadTransPeel MaybeT where
  peel = return $ \m -> do
    xm <- runMaybeT m
    return $ maybe mzero return xm

#if MIN_VERSION_transformers(0,4,0)
instance MonadTransPeel (Except.ExceptT e) where
  peel = return $ \m -> do
    xe <- Except.runExceptT m
    return $ either Except.throwE return xe
#endif

instance MonadTransPeel (ReaderT r) where
  peel = asks $ \r m -> do
    x <- runReaderT m r
    return $ return x

instance MonadTransPeel (StateT s) where
  peel = gets $ \s m -> do
    (x, s') <- runStateT m s
    return $ do
      put s'
      return x
instance MonadTransPeel (Strict.StateT s) where
  peel = Strict.gets $ \s m -> do
    (x, s') <- Strict.runStateT m s
    return $ do
      Strict.put s'
      return x

instance Monoid w => MonadTransPeel (WriterT w) where
  peel = return $ \m -> do
    (x, w) <- runWriterT m
    return $ do
      tell w
      return x
instance Monoid w => MonadTransPeel (Strict.WriterT w) where
  peel = return $ \m -> do
    (x, w) <- Strict.runWriterT m
    return $ do
      Strict.tell w
      return x

instance Monoid w => MonadTransPeel (RWS.RWST r w s) where
  peel = do
    r <- RWS.ask
    s <- RWS.get
    return $ \m -> do
      (x, s', w) <- RWS.runRWST m r s
      return $ do
        RWS.put s'
        RWS.tell w
        return x
instance Monoid w => MonadTransPeel (RWS.Strict.RWST r w s) where
  peel = do
    r <- RWS.Strict.ask
    s <- RWS.Strict.get
    return $ \m -> do
      (x, s', w) <- RWS.Strict.runRWST m r s
      return $ do
        RWS.Strict.put s'
        RWS.Strict.tell w
        return x


-- |@idPeel@ acts as the \"identity\" 'peel' operation from a monad
-- @m@ to itself.
--
-- @
--    'idPeel' = 'return' $ 'liftM' 'return'
-- @
--
-- It serves as the base case for a class like @MonadPeelIO@, which
-- allows control operations in some base monad (here @IO@) to be
-- lifted through arbitrary stacks of zero or more monad transformers
-- in one call.  For example, "Control.Monad.IO.Peel" defines
--
-- @
--    class 'MonadIO' m => MonadPeelIO m where
--      peelIO :: m (m a -> 'IO' (m a))
--    instance MonadPeelIO 'IO' where
--      peelIO = 'idPeel'
-- @
idPeel :: (Monad m, Monad n, Monad o) => n (m a -> m (o a))
idPeel = return $ liftM return

-- |@liftPeel@ is used to compose two 'peel' operations: the outer
-- provided by a 'MonadTransPeel' instance, and the inner provided as
-- the argument.
--
-- It satisfies @'liftPeel' 'idPeel' == 'peel'@.
--
-- It serves as the induction step of a @MonadPeelIO@-like class.  For
-- example, "Control.Monad.IO.Peel" defines
--
-- @
--    instance MonadPeelIO m => MonadPeelIO ('StateT' s m) where
--      peelIO = 'liftPeel' peelIO
-- @
--
-- using the 'MonadTransPeel' instance of @'StateT' s@.
liftPeel :: (MonadTransPeel t, Monad m, Monad m', Monad n', Monad (t n'),
             Monad o', Monad (t o')) =>
            n' (m' (t o' a) -> m (o' (t o' a))) -> t n' (t m' a -> m (t o' a))
liftPeel p = do
  k <- peel
  lift $ do
    k' <- p
    return $ \m -> do
      m' <- k' $ k m
      return $ join $ lift m'

-- |@liftOp@ is a particular application of 'peel' that allows lifting
-- control operations of type @(a -> m b) -> m b@ to @'MonadTransPeel'
-- t => (a -> t m b) -> t m b@.
--
-- @
--    'liftOp' f g = do
--      k \<- 'peel'
--      'join' $ 'lift' $ f (k . g)
-- @
liftOp :: (MonadTransPeel t, Monad m, Monad n, Monad o, Monad (t n)) =>
          ((a -> m (t o b)) -> n (t n c)) -> (a -> t m b) -> t n c
liftOp f g = do
  k <- peel
  join $ lift $ f (k . g)

-- |@liftOp_@ is a particular application of 'peel' that allows
-- lifting control operations of type @m a -> m a@ to
-- @'MonadTransPeel' m => t m a -> t m a@.
--
-- It can be thought of as a generalization of @mapReaderT@,
-- @mapStateT@, etc.
--
-- @
--    'liftOp_' f m = do
--      k \<- 'peel'
--      'join' $ 'lift' $ f (k m)
-- @
liftOp_ :: (MonadTransPeel t, Monad m, Monad n, Monad o, Monad (t n)) =>
           (m (t o a) -> n (t n b)) -> t m a -> t n b
liftOp_ f m = do
  k <- peel
  join $ lift $ f (k m)
