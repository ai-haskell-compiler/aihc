{-# LANGUAGE CPP #-}
-- | When you've caught all the exceptions that can be handled safely,
--   this is what you're left with.
--
-- > runExceptIO . fromIO ≡ id
--
-- It is intended that you use qualified imports with this library.
--
-- > import UnexceptionalIO.Trans (UIO)
-- > import qualified UnexceptionalIO.Trans as UIO
module UnexceptionalIO.Trans (
	UIO.UIO,
	UIO.Unexceptional(..),
	fromIO,
#ifdef __GLASGOW_HASKELL__
	fromIO',
#endif
	run,
	runExceptIO,
	-- * Unsafe entry points
	UIO.unsafeFromIO,
	-- * Pseudo exceptions
	UIO.SomeNonPseudoException,
#ifdef __GLASGOW_HASKELL__
	UIO.PseudoException(..),
	UIO.ProgrammerError(..),
	UIO.ExternalError(..),
	-- * Pseudo exception helpers
	UIO.bracket,
#if MIN_VERSION_base(4,6,0)
	UIO.forkFinally,
	UIO.fork,
	UIO.ChildThreadError(..)
#endif
#endif
) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(..))
#if MIN_VERSION_transformers(0,5,3)
import qualified Control.Monad.Trans.Accum as Trans
#endif
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.Cont as Trans
import qualified Control.Monad.Trans.Except as Trans
import qualified Control.Monad.Trans.Identity as Trans
import qualified Control.Monad.Trans.Maybe as Trans
import qualified Control.Monad.Trans.RWS.Lazy as RWSL
import qualified Control.Monad.Trans.RWS.Strict as RWSS
import qualified Control.Monad.Trans.Reader as Trans
#if MIN_VERSION_transformers(0,5,3)
import qualified Control.Monad.Trans.Select as Trans
#endif
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS
import qualified Control.Monad.Trans.Writer.Lazy as WriterL
import qualified Control.Monad.Trans.Writer.Strict as WriterS
import qualified UnexceptionalIO as UIO

-- | Catch any exception but 'PseudoException' in an 'IO' action
fromIO :: (UIO.Unexceptional m) => IO a -> Trans.ExceptT UIO.SomeNonPseudoException m a
fromIO = Trans.ExceptT . UIO.fromIO

-- | Catch any 'e' in an 'IO' action, with a default mapping for
--   unexpected cases
fromIO' :: (Exception e, UIO.Unexceptional m) =>
	(UIO.SomeNonPseudoException -> e) -- ^ Default if an unexpected exception occurs
	-> IO a
	-> Trans.ExceptT e m a
fromIO' f = Trans.ExceptT . UIO.fromIO' f

-- | Re-embed 'UIO' into 'MonadIO'
run :: (MonadIO m) => UIO.UIO a -> m a
run = liftIO . UIO.run

-- | Re-embed 'UIO' and possible exception back into 'IO'
runExceptIO :: (Exception e, MonadIO m) => Trans.ExceptT e UIO.UIO a -> m a
runExceptIO = liftIO . UIO.runEitherIO . Trans.runExceptT

#if MIN_VERSION_transformers(0,5,3)
instance (UIO.Unexceptional m, Monoid w) => UIO.Unexceptional (Trans.AccumT w m) where
	lift = Trans.lift . UIO.lift
#endif

instance (UIO.Unexceptional m) => UIO.Unexceptional (Trans.ContT r m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m) => UIO.Unexceptional (Trans.ExceptT e m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m) => UIO.Unexceptional (Trans.IdentityT m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m) => UIO.Unexceptional (Trans.MaybeT m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m, Monoid w) => UIO.Unexceptional (RWSL.RWST r w s m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m, Monoid w) => UIO.Unexceptional (RWSS.RWST r w s m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m) => UIO.Unexceptional (Trans.ReaderT r m) where
	lift = Trans.lift . UIO.lift

#if MIN_VERSION_transformers(0,5,3)
instance (UIO.Unexceptional m) => UIO.Unexceptional (Trans.SelectT r m) where
	lift = Trans.lift . UIO.lift
#endif

instance (UIO.Unexceptional m) => UIO.Unexceptional (StateL.StateT s m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m) => UIO.Unexceptional (StateS.StateT s m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m, Monoid w) => UIO.Unexceptional (WriterL.WriterT w m) where
	lift = Trans.lift . UIO.lift

instance (UIO.Unexceptional m, Monoid w) => UIO.Unexceptional (WriterS.WriterT w m) where
	lift = Trans.lift . UIO.lift
