{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

-- |
-- Module      :  Control.Monad.Exception
-- Copyright   :  (c) Harvard University 2008-2011
--                (c) Geoffrey Mainland 2011-2021
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Control.Monad.Exception (
    E.Exception(..),
    E.SomeException,

    MonadException(..),
    onException,

    MonadAsyncException(..),
    bracket,
    bracket_,

    ExceptionT(..),
    mapExceptionT,
    liftException
  ) where

#if !MIN_VERSION_base(4,6,0)
import Prelude hiding (catch)
#endif /*!MIN_VERSION_base(4,6,0) */

import Control.Applicative
import qualified Control.Exception as E (Exception(..),
                                         SomeException,
                                         catch,
                                         throw,
                                         finally)
import qualified Control.Exception as E (mask)
import Control.Monad (MonadPlus(..))
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail
#endif /* !MIN_VERSION_base(4,13,0) */
#if !MIN_VERSION_base(4,11,0)
import qualified Control.Monad.Fail as Fail
#endif /* !MIN_VERSION_base(4,11,0) */
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.Error (Error(..),
                                  ErrorT(..),
                                  mapErrorT,
                                  runErrorT)
#endif /* !MIN_VERSION_transformers(0,6,0) */
import Control.Monad.Trans.Except (ExceptT(..),
                                   mapExceptT,
                                   runExceptT)
import Control.Monad.Trans.Identity (IdentityT(..),
                                     mapIdentityT,
                                     runIdentityT)
#if !MIN_VERSION_transformers(0,6,0)
import Control.Monad.Trans.List (ListT(..),
                                 mapListT,
                                 runListT)
#endif /* !MIN_VERSION_transformers(0,6,0) */
import Control.Monad.Trans.Maybe (MaybeT(..),
                                  mapMaybeT,
                                  runMaybeT)
import Control.Monad.Trans.RWS.Lazy as Lazy (RWST(..),
                                             mapRWST,
                                             runRWST)
import Control.Monad.Trans.RWS.Strict as Strict (RWST(..),
                                                 mapRWST,
                                                 runRWST)
import Control.Monad.Trans.Reader (ReaderT(..),
                                   mapReaderT)
import Control.Monad.Trans.State.Lazy as Lazy (StateT(..),
                                               mapStateT,
                                               runStateT)
import Control.Monad.Trans.State.Strict as Strict (StateT(..),
                                                   mapStateT,
                                                   runStateT)
import Control.Monad.Trans.Writer.Lazy as Lazy (WriterT(..),
                                                mapWriterT,
                                                runWriterT)
import Control.Monad.Trans.Writer.Strict as Strict (WriterT(..),
                                                    mapWriterT,
                                                    runWriterT)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid)
#endif /* !MIN_VERSION_base(4,8,0) */
import GHC.Conc.Sync (STM(..),
                      catchSTM,
                      throwSTM)

class (Monad m) => MonadException m where
    -- | Throw an exception.
    throw :: E.Exception e => e -> m a
    -- | Catch an exception.
    catch :: E.Exception e
          => m a        -- ^ The computation to run
          -> (e -> m a) -- ^ Handler to invoke if an exception is raised
          -> m a
    -- | Run a computation and always perform a second, final computation even
    -- if an exception is raised. If a short-circuiting monad transformer such
    -- as ErrorT or MaybeT is used to transform a MonadException monad, then the
    -- implementation of @finally@ for the transformed monad must guarantee that
    -- the final action is also always performed when any short-circuiting
    -- occurs.
    finally :: m a  -- ^ The computation to run
            -> m b  -- ^ Computation to run afterward (even if an exception was
                    -- raised)
            -> m a
    act `finally` sequel = do
        a <- act `onException` sequel
        _ <- sequel
        return a

-- | If an exception is raised by the computation, then perform a final action
-- and re-raise the exception.
onException :: MonadException m
            => m a -- ^ The computation to run
            -> m b -- ^ Computation to run if an exception is raised
            -> m a
onException act what =
    act `catch` \(e :: E.SomeException) -> what >> throw e

class (MonadIO m, MonadException m) => MonadAsyncException m where
    -- | Executes a computation with asynchronous exceptions /masked/. The
    -- argument passed to 'mask' is a function that takes as its argument
    -- another function, which can be used to restore the prevailing masking
    -- state within the context of the masked computation.
    mask :: ((forall a. m a -> m a) -> m b) -> m b

-- | When you want to acquire a resource, do some work with it, and then release
-- the resource, it is a good idea to use 'bracket', because 'bracket' will
-- install the necessary exception handler to release the resource in the event
-- that an exception is raised during the computation.  If an exception is
-- raised, then 'bracket' will re-raise the exception (after performing the
-- release).
bracket :: MonadAsyncException m
        => m a         -- ^ computation to run first (\"acquire resource\")
        -> (a -> m b)  -- ^ computation to run last (\"release resource\")
        -> (a -> m c)  -- ^ computation to run in-between
        -> m c         -- returns the value from the in-between computation
bracket before after thing =
    mask $ \restore -> do
        a <- before
        restore (thing a) `finally` after a

-- | A variant of 'bracket' where the return value from the first computation is
-- not required.
bracket_ :: MonadAsyncException m
         => m a
         -> m b
         -> m c
         -> m c
bracket_ before after thing =
    bracket before (const after) (const thing)

--
-- The ExceptionT monad transformer.
--

newtype ExceptionT m a =
    ExceptionT { runExceptionT :: m (Either E.SomeException a) }

mapExceptionT :: (m (Either E.SomeException a) -> n (Either E.SomeException b))
              -> ExceptionT m a
              -> ExceptionT n b
mapExceptionT f = ExceptionT . f . runExceptionT

-- | Lift the result of running a computation in a monad transformed by
-- 'ExceptionT' into another monad that supports exceptions.
liftException :: MonadException m => Either E.SomeException a -> m a
liftException (Left e)  = throw e
liftException (Right a) = return a

instance MonadTrans ExceptionT where
    lift m = ExceptionT $ do
        a <- m
        return (Right a)

instance (Functor m, Monad m) => Applicative (ExceptionT m) where
    pure a = ExceptionT $ return (Right a)

    f <*> v = ExceptionT $ do
        mf <- runExceptionT f
        case mf of
            Left  e -> return (Left e)
            Right k -> do
                mv <- runExceptionT v
                case mv of
                    Left  e -> return (Left e)
                    Right x -> return (Right (k x))

instance (Functor m) => Functor (ExceptionT m) where
    fmap f = ExceptionT . fmap (fmap f) . runExceptionT

instance (Monad m) => Monad (ExceptionT m) where
#if MIN_VERSION_base(4,8,0)
    return = pure
#else /* !MIN_VERSION_base(4,8,0) */
    return a = ExceptionT $ return (Right a)
#endif /* !MIN_VERSION_base(4,8,0) */

    m >>= k = ExceptionT $ do
        a <- runExceptionT m
        case a of
          Left l  -> return (Left l)
          Right r -> runExceptionT (k r)

#if !MIN_VERSION_base(4,11,0)
    fail = Fail.fail
#endif /* !MIN_VERSION_base(4,11,0) */

instance (Monad m) => MonadFail (ExceptionT m) where
    fail msg = ExceptionT $ return (Left (E.toException (userError msg)))

instance (Monad m) => MonadPlus (ExceptionT m) where
    mzero       = ExceptionT $ return (Left (E.toException (userError "")))
    m `mplus` n = ExceptionT $ do
        a <- runExceptionT m
        case a of
          Left _  -> runExceptionT n
          Right r -> return (Right r)

instance (Functor m, Monad m) => Alternative (ExceptionT m) where
    empty = mzero
    (<|>) = mplus

instance (MonadFix m) => MonadFix (ExceptionT m) where
    mfix f = ExceptionT $ mfix $ \a -> runExceptionT $ f $ case a of
        Right r -> r
        _       -> error "empty mfix argument"

instance (Monad m) => MonadException (ExceptionT m) where
    throw e     = ExceptionT $ return (Left (E.toException e))
    m `catch` h = ExceptionT $ do
        a <- runExceptionT m
        case a of
          Left l  ->  case E.fromException l of
                        Just e  -> runExceptionT (h e)
                        Nothing -> return (Left l)
          Right r -> return (Right r)

instance (MonadIO m) => MonadIO (ExceptionT m) where
    liftIO m = ExceptionT $ liftIO $
        fmap Right m `E.catch` \(e :: E.SomeException) -> return (Left e)

instance (MonadAsyncException m) => MonadAsyncException (ExceptionT m) where
    mask act = ExceptionT $ mask $ \restore ->
               runExceptionT $ act (mapExceptionT restore)

--
-- Instances for the IO monad.
--

instance MonadException IO where
    catch   = E.catch
    throw   = E.throw
    finally = E.finally

#if __GLASGOW_HASKELL__ >= 700
instance MonadAsyncException IO where
    mask = E.mask
#else /* __GLASGOW_HASKELL__ < 700 */
instance MonadAsyncException IO where
    mask act = do
        b <- E.blocked
        if b
          then act id
          else E.block $ act E.unblock
#endif /* __GLASGOW_HASKELL__ < 700 */

--
-- Instances for the STM monad.
--

instance MonadException STM where
    catch = catchSTM
    throw = throwSTM

--
-- MonadException instances for transformers.
--

#if !MIN_VERSION_transformers(0,6,0)
instance (MonadException m, Error e) =>
    MonadException (ErrorT e m) where
    throw       = lift . throw
    m `catch` h = mapErrorT (\m' -> m' `catch` \e -> runErrorT (h e)) m

    act `finally` sequel =
        mapErrorT (\act' -> act' `finally` runErrorT sequel) act
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance (MonadException m) =>
    MonadException (ExceptT e' m) where
    throw       = lift . throw
    m `catch` h = mapExceptT (\m' -> m' `catch` \e -> runExceptT (h e)) m

    act `finally` sequel =
        mapExceptT (\act' -> act' `finally` runExceptT sequel) act

instance (MonadException m) =>
    MonadException (IdentityT m) where
    throw       = lift . throw
    m `catch` h = mapIdentityT (\m' -> m' `catch` \e -> runIdentityT (h e)) m

#if !MIN_VERSION_transformers(0,6,0)
instance MonadException m =>
    MonadException (ListT m) where
    throw       = lift . throw
    m `catch` h = mapListT (\m' -> m' `catch` \e -> runListT (h e)) m
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance (MonadException m) =>
    MonadException (MaybeT m) where
    throw       = lift . throw
    m `catch` h = mapMaybeT (\m' -> m' `catch` \e -> runMaybeT (h e)) m

    act `finally` sequel =
        mapMaybeT (\act' -> act' `finally` runMaybeT sequel) act

instance (Monoid w, MonadException m) =>
    MonadException (Lazy.RWST r w s m) where
    throw       = lift . throw
    m `catch` h = Lazy.RWST $ \r s ->
                  Lazy.runRWST m r s `catch` \e -> Lazy.runRWST (h e) r s

instance (Monoid w, MonadException m) =>
    MonadException (Strict.RWST r w s m) where
    throw       = lift . throw
    m `catch` h = Strict.RWST $ \r s ->
                  Strict.runRWST m r s `catch` \e -> Strict.runRWST (h e) r s

instance (MonadException m) =>
    MonadException (ReaderT r m) where
    throw       = lift . throw
    m `catch` h = ReaderT $ \r ->
                  runReaderT m r `catch` \e -> runReaderT (h e) r

instance (MonadException m) =>
    MonadException (Lazy.StateT s m) where
    throw       = lift . throw
    m `catch` h = Lazy.StateT $ \s ->
                  Lazy.runStateT m s `catch` \e -> Lazy.runStateT (h e) s

instance (MonadException m) =>
    MonadException (Strict.StateT s m) where
    throw       = lift . throw
    m `catch` h = Strict.StateT $ \s ->
                  Strict.runStateT m s `catch` \e -> Strict.runStateT (h e) s

instance (Monoid w, MonadException m) =>
    MonadException (Lazy.WriterT w m) where
    throw       = lift . throw
    m `catch` h = Lazy.WriterT $
                  Lazy.runWriterT m `catch` \e -> Lazy.runWriterT (h e)

instance (Monoid w, MonadException m) =>
    MonadException (Strict.WriterT w m) where
    throw       = lift . throw
    m `catch` h = Strict.WriterT $
                  Strict.runWriterT m `catch` \e -> Strict.runWriterT (h e)

--
-- MonadAsyncException instances for transformers.
--

#if !MIN_VERSION_transformers(0,6,0)
instance (MonadAsyncException m, Error e) =>
    MonadAsyncException (ErrorT e m) where
    mask act = ErrorT $ mask $ \restore ->
               runErrorT $ act (mapErrorT restore)
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance (MonadAsyncException m) =>
    MonadAsyncException (ExceptT e' m) where
    mask act = ExceptT $ mask $ \restore ->
               runExceptT $ act (mapExceptT restore)

instance (MonadAsyncException m) =>
    MonadAsyncException (IdentityT m) where
    mask act = IdentityT $ mask $ \restore ->
               runIdentityT $ act (mapIdentityT restore)

#if !MIN_VERSION_transformers(0,6,0)
instance (MonadAsyncException m) =>
    MonadAsyncException (ListT m) where
    mask act = ListT $ mask $ \restore ->
               runListT $ act (mapListT restore)
#endif /* !MIN_VERSION_transformers(0,6,0) */

instance (MonadAsyncException m) =>
    MonadAsyncException (MaybeT m) where
    mask act = MaybeT $ mask $ \restore ->
               runMaybeT $ act (mapMaybeT restore)

instance (Monoid w, MonadAsyncException m) =>
    MonadAsyncException (Lazy.RWST r w s m) where
    mask act = Lazy.RWST $ \r s -> mask $ \restore ->
               Lazy.runRWST (act (Lazy.mapRWST restore)) r s

instance (Monoid w, MonadAsyncException m) =>
    MonadAsyncException (Strict.RWST r w s m) where
    mask act = Strict.RWST $ \r s -> mask $ \restore ->
               Strict.runRWST (act (Strict.mapRWST restore)) r s

instance (MonadAsyncException m) =>
    MonadAsyncException (ReaderT r m) where
    mask act = ReaderT $ \r -> mask $ \restore ->
               runReaderT (act (mapReaderT restore)) r

instance (MonadAsyncException m) =>
    MonadAsyncException (Lazy.StateT s m) where
    mask act = Lazy.StateT $ \s -> mask $ \restore ->
               Lazy.runStateT (act (Lazy.mapStateT restore)) s

instance (MonadAsyncException m) =>
    MonadAsyncException (Strict.StateT s m) where
    mask act = Strict.StateT $ \s -> mask $ \restore ->
               Strict.runStateT (act (Strict.mapStateT restore)) s

instance (Monoid w, MonadAsyncException m) =>
    MonadAsyncException (Lazy.WriterT w m) where
    mask act = Lazy.WriterT $ mask $ \restore ->
               Lazy.runWriterT $ act (Lazy.mapWriterT restore)

instance (Monoid w, MonadAsyncException m) =>
    MonadAsyncException (Strict.WriterT w m) where
    mask act = Strict.WriterT $ mask $ \restore ->
               Strict.runWriterT $ act (Strict.mapWriterT restore)
