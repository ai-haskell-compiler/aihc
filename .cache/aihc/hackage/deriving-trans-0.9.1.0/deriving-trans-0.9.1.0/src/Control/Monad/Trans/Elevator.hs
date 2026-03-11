{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Elevator where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Zip
import Data.Kind

#if defined(VERSION_exceptions)
import Control.Monad.Catch qualified as Exceptions
#endif

#if defined(VERSION_logict)
import Control.Monad.Logic.Class qualified as LogicT
#endif

#if defined(VERSION_monad_logger)
import Control.Monad.Logger qualified as MonadLogger
#endif

#if defined(VERSION_mtl)
import Control.Monad.Accum qualified as Mtl
import Control.Monad.Cont.Class qualified as Mtl
import Control.Monad.Error.Class qualified as Mtl
import Control.Monad.Reader.Class qualified as Mtl
import Control.Monad.RWS.Class qualified as Mtl (MonadRWS)
import Control.Monad.Select qualified as Mtl
import Control.Monad.State.Class qualified as Mtl
import Control.Monad.Writer.Class qualified as Mtl
#endif

#if defined(VERSION_primitive)
import Control.Monad.Primitive qualified as Primitive
#endif

#if defined(VERSION_random)
import Data.Functor.Const qualified as Random
import System.Random.Stateful qualified as Random
#endif

#if defined(VERSION_resourcet)
import Control.Monad.Trans.Resource qualified as ResourceT
#endif

#if defined(VERSION_unliftio_core)
import Control.Monad.IO.Unlift qualified as UnliftIO
#endif

-- * 'Elevator'
--
-- $elevator
--
-- 'Elevator' can be used to lift instances through monad transformers as long as they implement
-- a 'MonadTrans' \/ 'MonadTransControl' \/ 'MonadTransControlIdentity' instance.
--
-- 'MonadTransControl' is only necessary when there is atleast one method with a monadic argument.
-- 'MonadTransControlIdentity' is even stronger and only required for a few specific instances.

-- | A newtype wrapper for monad transformers.
--
-- Access instances of the inner monad @m@.
--
-- __Type level arguments:__
--
-- [@t :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] monad transformer
-- [@m :: 'Type' -> 'Type'@] monad
-- [@a :: 'Type'@] value
type Elevator :: ((Type -> Type) -> Type -> Type) -- @t@
              -> (Type -> Type) -- @m@
              -> Type -- @a@
              -> Type
newtype Elevator t m a = Ascend { descend :: t m a }
  deriving newtype (Applicative, Functor, Monad)
  deriving newtype (MonadTrans, MonadTransControl, MonadTransControlIdentity)

instance (MonadBase b m, MonadTrans t) => MonadBase b (Elevator t m) where
  liftBase = lift . liftBase

instance (MonadBaseControl b m, MonadTransControl t) => MonadBaseControl b (Elevator t m) where
  type StM (Elevator t m) a = StM m (StT t a)
  liftBaseWith f = liftWith $ \ runT -> liftBaseWith $ \ runInBase -> f $ runInBase . runT
  restoreM = restoreT . restoreM

instance (MonadBaseControlIdentity b m, MonadTransControlIdentity t) => MonadBaseControlIdentity b (Elevator t m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance (Alternative m, Monad m, MonadTransControl t) => Alternative (Elevator t m) where
  empty = lift empty
  (<|>) x y = (restoreT . pure =<<) $ liftWith $ \ runT -> runT x <|> runT y

instance (MonadFail m, MonadTrans t) => MonadFail (Elevator t m) where
  fail = lift . fail

instance (MonadFix m, MonadTransControlIdentity t) => MonadFix (Elevator t m) where
  mfix f = liftWithIdentity $ \ runT -> mfix $ \ x -> runT $ f x

instance (MonadIO m, MonadTrans t) => MonadIO (Elevator t m) where
  liftIO = lift . liftIO

instance (MonadPlus m, MonadTransControl t) => MonadPlus (Elevator t m)

instance (MonadZip m, MonadTransControlIdentity t) => MonadZip (Elevator t m) where
  mzip x y = liftWithIdentity $ \ runT ->
    mzip (runT x) (runT y)

#if defined(VERSION_exceptions)
instance (Exceptions.MonadThrow m, MonadTrans t) => Exceptions.MonadThrow (Elevator t m) where
  throwM = lift . Exceptions.throwM

instance (Exceptions.MonadCatch m, MonadTransControl t) => Exceptions.MonadCatch (Elevator t m) where
  catch throwing catching = (restoreT . pure =<<) $ liftWith $ \ runT ->
    Exceptions.catch (runT throwing) (runT . catching)

instance (Exceptions.MonadMask m, MonadTransControl t) => Exceptions.MonadMask (Elevator t m) where
  mask f = (restoreT . pure =<<) $ liftWith $ \ runT ->
    Exceptions.mask $ \ u -> runT $ f $ restoreT . u . runT
  uninterruptibleMask f = (restoreT . pure =<<) $ liftWith $ \ runT ->
    Exceptions.uninterruptibleMask $ \ u -> runT $ f $ restoreT . u . runT
  generalBracket acquire release use = do
    (usageResult', releaseResult') <- liftWith $ \ runT -> do
      Exceptions.generalBracket
        (runT acquire)
        (\ resource' exitCase' -> runT $ do
          resource <- restoreT $ pure resource'
          case exitCase' of
            Exceptions.ExitCaseSuccess x' ->
              release resource . Exceptions.ExitCaseSuccess =<< restoreT (pure x')
            Exceptions.ExitCaseException e ->
              release resource (Exceptions.ExitCaseException e)
            Exceptions.ExitCaseAbort ->
              release resource Exceptions.ExitCaseAbort
        )
        (\ resource -> runT $ use =<< restoreT (pure resource))
    usageResult <- restoreT $ pure usageResult'
    releaseResult <- restoreT $ pure releaseResult'
    pure (usageResult, releaseResult)
#endif

#if defined(VERSION_logict)
instance (LogicT.MonadLogic m, MonadTransControlIdentity t) => LogicT.MonadLogic (Elevator t m) where
  msplit tma = (((\(a, b) -> (a, lift b)) <$>) <$>) $
    liftWithIdentity $ \runT -> LogicT.msplit $ runT tma
#endif

#if defined(VERSION_monad_logger)
instance (MonadLogger.MonadLogger m, MonadTrans t) => MonadLogger.MonadLogger (Elevator t m) where
  monadLoggerLog location logSource logLevel = lift .
    MonadLogger.monadLoggerLog location logSource logLevel

instance (MonadLogger.MonadLoggerIO m, MonadTrans t) => MonadLogger.MonadLoggerIO (Elevator t m) where
  askLoggerIO = lift MonadLogger.askLoggerIO
#endif

#if defined(VERSION_mtl)
instance (Mtl.MonadAccum w m, MonadTrans t) => Mtl.MonadAccum w (Elevator t m) where
  look = lift Mtl.look
  add = lift . Mtl.add

instance (Mtl.MonadCont m, MonadTransControl t) => Mtl.MonadCont (Elevator t m) where
  callCC f = (restoreT . pure =<<) $ liftWith $ \ runT ->
    Mtl.callCC $ \ c -> runT $ f $ \ a -> restoreT $ c =<< runT (pure a)

instance (Mtl.MonadError e m, MonadTransControl t) => Mtl.MonadError e (Elevator t m) where
  throwError = lift . Mtl.throwError
  catchError throwing catching = (restoreT . pure =<<) $ liftWith $ \ runT ->
    Mtl.catchError (runT throwing) (runT . catching)

instance (Mtl.MonadReader r m, MonadTransControl t) => Mtl.MonadReader r (Elevator t m) where
  ask = lift Mtl.ask
  local f tma = (restoreT . pure =<<) $ liftWith $ \ runT ->
    Mtl.local f $ runT tma

instance (Mtl.MonadRWS r w s m, MonadTransControl t) => Mtl.MonadRWS r w s (Elevator t m)

instance (Mtl.MonadSelect r m, MonadTrans t) => Mtl.MonadSelect r (Elevator t m) where
  select = lift . Mtl.select

instance (Mtl.MonadState s m, MonadTrans t) => Mtl.MonadState s (Elevator t m) where
  get = lift Mtl.get
  put = lift . Mtl.put

instance (Mtl.MonadWriter w m, MonadTransControl t) => Mtl.MonadWriter w (Elevator t m) where
  tell = lift . Mtl.tell
  listen tma = liftWith (\ runT -> Mtl.listen $ runT tma) >>= \ (sta, w) ->
    (, w) <$> restoreT (pure sta)
  pass tma = lift . Mtl.pass . pure =<< tma
#endif

#if defined(VERSION_primitive)
instance (Primitive.PrimMonad m, MonadTrans t) => Primitive.PrimMonad (Elevator t m) where
  type PrimState (Elevator t m) = Primitive.PrimState m
  primitive = lift . Primitive.primitive
#endif

#if defined(VERSION_random)
instance (Random.StatefulGen g m, MonadTrans t) => Random.StatefulGen (Random.Const g (Elevator t)) (Elevator t m) where
  uniformWord32R word32 = lift . Random.uniformWord32R word32 . Random.getConst
  uniformWord64R word64 = lift . Random.uniformWord64R word64 . Random.getConst
  uniformWord8 = lift . Random.uniformWord8 . Random.getConst
  uniformWord16 = lift . Random.uniformWord16 . Random.getConst
  uniformWord32 = lift . Random.uniformWord32 . Random.getConst
  uniformWord64 = lift . Random.uniformWord64 . Random.getConst
  uniformShortByteString n = lift . Random.uniformShortByteString n . Random.getConst

instance (Random.FrozenGen f m, MonadTrans t) => Random.FrozenGen (Random.Const f (Elevator t)) (Elevator t m) where
  type MutableGen (Const f (Elevator t)) (Elevator t m) = Const (Random.MutableGen f m) (Elevator t)
  freezeGen = lift . fmap Random.Const . Random.freezeGen . Random.getConst
  thawGen = lift . fmap Random.Const . Random.thawGen . Random.getConst

instance (Random.RandomGenM g r m, MonadTrans t) => Random.RandomGenM (Random.Const g (Elevator t)) r (Elevator t m) where
  applyRandomGenM f = lift . Random.applyRandomGenM f . Random.getConst
#endif

#if defined(VERSION_resourcet)
instance (ResourceT.MonadResource m, MonadTrans t) => ResourceT.MonadResource (Elevator t m) where
  liftResourceT = lift . ResourceT.liftResourceT
#endif

#if defined(VERSION_unliftio_core)
instance (UnliftIO.MonadUnliftIO m, MonadTransControlIdentity t) => UnliftIO.MonadUnliftIO (Elevator t m) where
  withRunInIO f = liftWithIdentity $ \runT -> UnliftIO.withRunInIO $ \runInIO -> f $ runInIO . runT
#endif

-- * Examples

-- ** Example 1: Recover submerged instances
--
-- $example1
--
-- Let's assume you want to define a monad transformer stack.
--
-- @
-- newtype StackT m a = StackT { unStackT :: 'Control.Monad.Trans.Reader.ReaderT' 'Char' ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m) a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- Now you want to expose the inner @('Control.Monad.Reader.Class.MonadReader' 'Bool')@ instance with @(StackT m)@.
--
-- Normally it's shadowed by the @('Control.Monad.Reader.Class.MonadReader' 'Char')@ instance, but we can use 'Elevator' to
-- access the inner transformer.
--
-- @
--   deriving ('Control.Monad.Reader.Class.MonadReader' 'Bool') via 'Elevator' ('Control.Monad.Trans.Reader.ReaderT' 'Char') ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m)
-- @

-- ** Example 2: Custom transformer without boilerplate
--
-- $example2
--
-- Let's assume you have defined a monad transformer.
--
-- @
-- newtype CustomT m a = CustomT { unCustomT :: 'Control.Monad.Trans.Identity.IdentityT' m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
--   deriving newtype ('MonadTrans', 'MonadTransControl', 'MonadTransControlIdentity')
--
-- runCustomT :: CustomT m a -> m a
-- runCustomT = 'Control.Monad.Trans.Identity.runIdentityT' . unCustomT
-- @
--
-- Now you want to use this monad transformer in a transformer stack.
--
-- @
-- newtype StackT m a = StackT { unStackT :: CustomT ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m) a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- Unfortunately we can't derive a @('Monad' m => 'Control.Monad.Reader.Class.MonadReader' 'Bool' (StackT m))@ instance with
-- /GeneralizedNewtypeDeriving/, without also adding the instance to @CustomT@.
--
-- To still derive this trivial instance we can use 'Elevator' with /DerivingVia/.
--
-- @
--   deriving ('Control.Monad.Reader.Class.MonadReader' 'Bool') via ('Elevator' CustomT ('Control.Monad.Trans.Reader.ReaderT' 'Bool' m))
-- @

-- ** Example 3: Adding an instance for 'Elevator'
--
-- $example3
--
-- Suppose you define a new type class.
--
-- @
-- class 'Monad' m => MonadCustom m where
--   simpleMethod :: a -> m a
--   complicatedMethod :: (a -> m b) -> m b
-- @
--
-- A simple way to allow a type class to be lifted through other monad transformers is by adding an
-- instance for 'Elevator'.
--
-- You have to be careful about monadic state 'StT', when defining such instances using
-- 'MonadTransControl'.
--
-- @
-- instance (MonadCustom m, 'MonadTransControl' t) => MonadCustom ('Elevator' t m) where
--   simpleMethod = 'lift' . simpleMethod
--   complicatedMethod f = ('restoreT' . 'pure' '=<<') $ 'liftWith' $ \\ runT ->
--     complicatedMethod $ runT . f
-- @
--
-- Some useful examples (or exercises) are the instances for
-- [mtl](https://hackage.haskell.org/package/mtl)'s type classes ('Control.Monad.Error.Class.MonadError', 'Control.Monad.Reader.Class.MonadReader',
-- 'Control.Monad.State.Class.MonadState', 'Control.Monad.Writer.Class.MonadWriter').
