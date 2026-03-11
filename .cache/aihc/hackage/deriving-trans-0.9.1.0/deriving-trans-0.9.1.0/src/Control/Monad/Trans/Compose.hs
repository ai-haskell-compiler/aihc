{-# LANGUAGE CPP #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Compose where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Elevator
import Control.Monad.Zip
import Data.Kind

import Control.Monad.Trans.Except qualified as T
import Control.Monad.Trans.Maybe qualified as T

#if defined(VERSION_exceptions)
import Control.Monad.Catch qualified as Exceptions
import Control.Monad.Catch.Pure qualified as Exceptions.T
#endif

#if defined(VERSION_logict)
import Control.Monad.Logic qualified as LogicT.T
import Control.Monad.Logic.Class qualified as LogicT
#endif

#if defined(VERSION_monad_logger)
import Control.Monad.Logger qualified as MonadLogger
#endif

#if defined(VERSION_mtl)
import Control.Monad.Accum qualified as Mtl
import Control.Monad.Accum.OrphanInstances qualified as Mtl ()
import Control.Monad.Cont.Class qualified as Mtl
import Control.Monad.Error.Class qualified as Mtl
import Control.Monad.Reader.Class qualified as Mtl
import Control.Monad.RWS.Class qualified as Mtl (MonadRWS)
import Control.Monad.Select qualified as Mtl
import Control.Monad.Select.OrphanInstances qualified as Mtl ()
import Control.Monad.State.Class qualified as Mtl
import Control.Monad.Trans.Accum qualified as Mtl.T
import Control.Monad.Trans.Cont qualified as Mtl.T
import Control.Monad.Trans.Except qualified as Mtl.T
import Control.Monad.Trans.RWS.CPS qualified as Mtl.CPST
import Control.Monad.Trans.RWS.Lazy qualified as Mtl.LT
import Control.Monad.Trans.RWS.Strict qualified as Mtl.ST
import Control.Monad.Trans.Reader qualified as Mtl.T
import Control.Monad.Trans.Select qualified as Mtl.T
import Control.Monad.Trans.State.Lazy qualified as Mtl.LT
import Control.Monad.Trans.State.Strict qualified as Mtl.ST
import Control.Monad.Trans.Writer.CPS qualified as Mtl.CPST
import Control.Monad.Trans.Writer.Lazy qualified as Mtl.LT
import Control.Monad.Trans.Writer.Strict qualified as Mtl.ST
import Control.Monad.Writer.Class qualified as Mtl
import Data.Functor.Identity qualified as Mtl
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

-- * 'ComposeT'
--
-- $composet
--
-- 'ComposeT' can be used in monad transformer stacks to derive instances.
--
-- This also allows the usage of these instances, while in the middle of the transformer stack.
-- This proves particularly useful, when writing a runner for a transformer stack.

-- | A newtype wrapper for two stacked monad transformers.
--
-- Access instances of the intermediate monad @(t2 m)@, whenever @t1@ implements 'MonadTrans' \/
-- 'MonadTransControl' \/ 'MonadTransControlIdentity'.
--
-- __Type level arguments:__
--
-- [@t1 :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] outer monad transformer
-- [@t2 :: ('Type' -> 'Type') -> 'Type' -> 'Type'@] inner monad transformer
-- [@m :: 'Type' -> 'Type'@] monad
-- [@a :: 'Type'@] value
type ComposeT :: ((Type -> Type) -> Type -> Type) -- @t1@
              -> ((Type -> Type) -> Type -> Type) -- @t2@
              -> (Type -> Type) -- @m@
              -> Type -- @a@
              -> Type
newtype ComposeT t1 t2 m a = ComposeT { deComposeT :: t1 (t2 m) a }
  deriving newtype (Applicative, Functor, Monad)

instance (MonadTrans t1, MonadTrans t2) => MonadTrans (ComposeT t1 t2) where
  lift = ComposeT . lift . lift

instance (MonadTransControl t1, MonadTransControl t2) => MonadTransControl (ComposeT t1 t2) where
  type StT (ComposeT t1 t2) a = StT t2 (StT t1 a)
  liftWith f = defaultLiftWith2 ComposeT deComposeT $ \ x -> f x
  restoreT = defaultRestoreT2 ComposeT

instance (MonadTransControlIdentity t1, MonadTransControlIdentity t2) => MonadTransControlIdentity (ComposeT t1 t2) where
  liftWithIdentity inner = ComposeT $ liftWithIdentity $ \ runId1 ->
    liftWithIdentity $ \ runId2 -> inner $ runId2 . runId1 . deComposeT

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadBase b m
    , MonadTrans (ComposeT t1 t2)
    ) => MonadBase b (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadBaseControl b m
    , MonadTransControl (ComposeT t1 t2)
    ) => MonadBaseControl b (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadBaseControlIdentity b m
    , MonadTransControlIdentity (ComposeT t1 t2)
    ) => MonadBaseControlIdentity b (ComposeT t1 t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Alternative (t2 m)
    , Monad (t2 m)
    , MonadTransControl t1
    ) => Alternative (ComposeT t1 t2 m)

-- | Set by 'T.ExceptT'.
deriving via T.ExceptT e (t2 (m :: Type -> Type))
  instance
    ( Monoid e
    , Monad (t2 m)
    ) => Alternative (ComposeT (T.ExceptT e) t2 m)

-- | Set by 'T.MaybeT'.
deriving via T.MaybeT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Alternative (ComposeT T.MaybeT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadFail (t2 m)
    , MonadTrans t1
    ) => MonadFail (ComposeT t1 t2 m)

-- | Set by 'T.MaybeT'.
deriving via T.MaybeT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadFail (ComposeT T.MaybeT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadFix (t2 m)
    , MonadTransControlIdentity t1
    ) => MonadFix (ComposeT t1 t2 m)

-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( MonadIO m
    , MonadTrans (ComposeT t1 t2)
    ) => MonadIO (ComposeT t1 t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadPlus (t2 m)
    , MonadTransControl t1
    ) => MonadPlus (ComposeT t1 t2 m)

-- | Set by 'T.ExceptT'.
deriving via T.ExceptT e (t2 (m :: Type -> Type))
  instance
    ( Monoid e
    , Monad (t2 m)
    ) => MonadPlus (ComposeT (T.ExceptT e) t2 m)

-- | Set by 'T.MaybeT'.
deriving via T.MaybeT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadPlus (ComposeT T.MaybeT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadZip (t2 m)
    , MonadTransControlIdentity t1
    ) => MonadZip (ComposeT t1 t2 m)

#if defined(VERSION_exceptions)
-- | Set by 'Exceptions.T.CatchT'.
deriving via Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Alternative (ComposeT Exceptions.T.CatchT t2 m)

-- | Set by 'Exceptions.T.CatchT'.
deriving via Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadFail (ComposeT Exceptions.T.CatchT t2 m)

-- | Set by 'Exceptions.T.CatchT'.
deriving via Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadPlus (ComposeT Exceptions.T.CatchT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Exceptions.MonadThrow (t2 m)
    , MonadTrans t1
    ) => Exceptions.MonadThrow (ComposeT t1 t2 m)

-- | Set by 'Exceptions.T.CatchT'.
deriving via Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Exceptions.MonadThrow (ComposeT Exceptions.T.CatchT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Exceptions.MonadCatch (t2 m)
    , MonadTransControl t1
    ) => Exceptions.MonadCatch (ComposeT t1 t2 m)

-- | Set by 'Exceptions.T.CatchT'.
deriving via Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Exceptions.MonadCatch (ComposeT Exceptions.T.CatchT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Exceptions.MonadMask (t2 m)
    , MonadTransControl t1
    ) => Exceptions.MonadMask (ComposeT t1 t2 m)

-- | Set by 'Exceptions.T.CatchT'.
deriving via Exceptions.T.CatchT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Exceptions.MonadMask (ComposeT Exceptions.T.CatchT t2 m)
#endif

#if defined(VERSION_logict)
-- | Set by 'LogicT.T.LogicT'.
deriving via LogicT.T.LogicT (t2 (m :: Type -> Type))
  instance Alternative (ComposeT LogicT.T.LogicT t2 m)

-- | Set by 'Logic.T.LogicT'.
deriving via LogicT.T.LogicT (t2 (m :: Type -> Type))
  instance MonadFail (ComposeT LogicT.T.LogicT t2 m)

-- | Set by 'LogicT.T.LogicT'.
deriving via LogicT.T.LogicT (t2 (m :: Type -> Type))
  instance MonadPlus (ComposeT LogicT.T.LogicT t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
instance {-# OVERLAPPABLE #-} (LogicT.MonadLogic (t2 m), MonadTransControlIdentity t1) => LogicT.MonadLogic (ComposeT t1 t2 m) where
  msplit = (((\(a, b) -> (a, ComposeT $ descend b)) <$>) <$>) .
    ComposeT . descend . LogicT.msplit . Ascend . deComposeT

-- | Set by 'LogicT.T.LogicT'.
deriving via LogicT.T.LogicT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => LogicT.MonadLogic (ComposeT LogicT.T.LogicT t2 m)
#endif

#if defined(VERSION_monad_logger)
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadLogger.MonadLogger (t2 m)
    , MonadTrans t1
    ) => MonadLogger.MonadLogger (ComposeT t1 t2 m)

-- | Set by 'MonadLogger.LoggingT'.
deriving via MonadLogger.LoggingT (t2 (m :: Type -> Type))
  instance
    ( MonadIO (t2 m)
    ) => MonadLogger.MonadLogger (ComposeT MonadLogger.LoggingT t2 m)

-- | Set by 'MonadLogger.NoLoggingT'.
deriving via MonadLogger.NoLoggingT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadLogger.MonadLogger (ComposeT MonadLogger.NoLoggingT t2 m)

-- | Set by 'MonadLogger.WriterLoggingT'.
deriving via MonadLogger.WriterLoggingT (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => MonadLogger.MonadLogger (ComposeT MonadLogger.WriterLoggingT t2 m)

-- TODO: `MonadIO m` and `MonadTrans t2` should not be required for this instance
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( MonadLogger.MonadLoggerIO (t2 m)
    , MonadTrans t1
    , MonadIO m
    , MonadTrans t2
    ) => MonadLogger.MonadLoggerIO (ComposeT t1 t2 m)

-- TODO: `MonadIO m` and `MonadTrans t2` should not be required for this instance
-- | Set by 'MonadLogger.LoggingT'.
deriving via MonadLogger.LoggingT (t2 (m :: Type -> Type))
  instance
    ( MonadIO (t2 m)
    , MonadIO m
    , MonadTrans t2
    ) => MonadLogger.MonadLoggerIO (ComposeT MonadLogger.LoggingT t2 m)

-- TODO: `MonadIO m` and `MonadTrans t2` should not be required for this instance
-- | Set by 'MonadLogger.NoLoggingT'.
deriving via MonadLogger.NoLoggingT (t2 (m :: Type -> Type))
  instance
    ( MonadIO (t2 m)
    , MonadIO m
    , MonadTrans t2
    ) => MonadLogger.MonadLoggerIO (ComposeT MonadLogger.NoLoggingT t2 m)
#endif

#if defined(VERSION_mtl)
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadAccum w (t2 m)
    , MonadTrans t1
    ) => Mtl.MonadAccum w (ComposeT t1 t2 m)

-- | Set by 'Mtl.T.AccumT'.
deriving via Mtl.T.AccumT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadAccum w (ComposeT (Mtl.T.AccumT w) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadCont (t2 m)
    , MonadTransControl t1
    ) => Mtl.MonadCont (ComposeT t1 t2 m)

-- | Set by 'Mtl.T.ContT'.
deriving via Mtl.T.ContT (r :: Type) (t2 (m :: Type -> Type))
  instance Mtl.MonadCont (ComposeT (Mtl.T.ContT r) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadError e (t2 m)
    , MonadTransControl t1
    ) => Mtl.MonadError e (ComposeT t1 t2 m)

-- | Set by 'Mtl.T.ExceptT'.
deriving via Mtl.T.ExceptT e (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Mtl.MonadError e (ComposeT (Mtl.T.ExceptT e) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadReader r (t2 m)
    , MonadTransControl t1
    ) => Mtl.MonadReader r (ComposeT t1 t2 m)

-- | Set by 'Mtl.T.ReaderT'.
deriving via Mtl.T.ReaderT r (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Mtl.MonadReader r (ComposeT (Mtl.T.ReaderT r) t2 m)

-- | Set by 'Mtl.CPST.RWST'.
deriving via Mtl.CPST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadReader r (ComposeT (Mtl.CPST.RWST r w s) t2 m)

-- | Set by 'Mtl.LT.RWST'.
deriving via Mtl.LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadReader r (ComposeT (Mtl.LT.RWST r w s) t2 m)

-- | Set by 'Mtl.ST.RWST'.
deriving via Mtl.ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadReader r (ComposeT (Mtl.ST.RWST r w s) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadRWS r w s (t2 m)
    , MonadTransControl t1
    ) => Mtl.MonadRWS r w s (ComposeT t1 t2 m)

-- | Set by 'Mtl.CPST.RWST'.
deriving via Mtl.CPST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadRWS r w s (ComposeT (Mtl.CPST.RWST r w s) t2 m)

-- | Set by 'Mtl.LT.RWST'.
deriving via Mtl.LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadRWS r w s (ComposeT (Mtl.LT.RWST r w s) t2 m)

-- | Set by 'Mtl.ST.RWST'.
deriving via Mtl.ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadRWS r w s (ComposeT (Mtl.ST.RWST r w s) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadSelect r (t2 m)
    , MonadTrans t1
    ) => Mtl.MonadSelect r (ComposeT t1 t2 m)

-- | Set by 'Mtl.T.SelectT'.
deriving via Mtl.T.SelectT r (t2 (m :: Type -> Type))
  instance
    ( MonadBaseControlIdentity Mtl.Identity (t2 m)
    ) => Mtl.MonadSelect r (ComposeT (Mtl.T.SelectT r) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadState s (t2 m)
    , MonadTrans t1
    ) => Mtl.MonadState s (ComposeT t1 t2 m)

-- | Set by 'Mtl.LT.StateT'.
deriving via Mtl.LT.StateT s (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Mtl.MonadState s (ComposeT (Mtl.LT.StateT s) t2 m)

-- | Set by 'Mtl.ST.StateT'.
deriving via Mtl.ST.StateT s (t2 (m :: Type -> Type))
  instance
    ( Monad (t2 m)
    ) => Mtl.MonadState s (ComposeT (Mtl.ST.StateT s) t2 m)

-- | Set by 'Mtl.CPST.RWST'.
deriving via Mtl.CPST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadState s (ComposeT (Mtl.CPST.RWST r w s) t2 m)

-- | Set by 'Mtl.LT.RWST'.
deriving via Mtl.LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadState s (ComposeT (Mtl.LT.RWST r w s) t2 m)

-- | Set by 'Mtl.ST.RWST'.
deriving via Mtl.ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadState s (ComposeT (Mtl.ST.RWST r w s) t2 m)

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( Mtl.MonadWriter w (t2 m)
    , MonadTransControl t1
    ) => Mtl.MonadWriter w (ComposeT t1 t2 m)

-- | Set by 'Mtl.CPST.WriterT'.
deriving via Mtl.CPST.WriterT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadWriter w (ComposeT (Mtl.CPST.WriterT w) t2 m)

-- | Set by 'Mtl.LT.WriterT'.
deriving via Mtl.LT.WriterT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadWriter w (ComposeT (Mtl.LT.WriterT w) t2 m)

-- | Set by 'Mtl.ST.WriterT'.
deriving via Mtl.ST.WriterT w (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadWriter w (ComposeT (Mtl.ST.WriterT w) t2 m)

-- | Set by 'Mtl.CPST.RWST'.
deriving via Mtl.CPST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadWriter w (ComposeT (Mtl.CPST.RWST r w s) t2 m)

-- | Set by 'Mtl.LT.RWST'.
deriving via Mtl.LT.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadWriter w (ComposeT (Mtl.LT.RWST r w s) t2 m)

-- | Set by 'Mtl.ST.RWST'.
deriving via Mtl.ST.RWST r w s (t2 (m :: Type -> Type))
  instance
    ( Monoid w
    , Monad (t2 m)
    ) => Mtl.MonadWriter w (ComposeT (Mtl.ST.RWST r w s) t2 m)
#endif

#if defined(VERSION_primitive)
-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( Primitive.PrimMonad m
    , MonadTrans (ComposeT t1 t2)
    ) =>
    Primitive.PrimMonad (ComposeT t1 t2 m)
#endif

#if defined(VERSION_random)
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
instance {-# OVERLAPPABLE #-} (Random.StatefulGen g (t2 m), MonadTrans t1) => Random.StatefulGen (Random.Const g (ComposeT t1 t2)) (ComposeT t1 t2 m) where
  uniformWord32R word32 = ComposeT . lift . Random.uniformWord32R word32 . Random.getConst
  uniformWord64R word64 = ComposeT . lift . Random.uniformWord64R word64 . Random.getConst
  uniformWord8 = ComposeT . lift . Random.uniformWord8 . Random.getConst
  uniformWord16 = ComposeT . lift . Random.uniformWord16 . Random.getConst
  uniformWord32 = ComposeT . lift . Random.uniformWord32 . Random.getConst
  uniformWord64 = ComposeT . lift . Random.uniformWord64 . Random.getConst
  uniformShortByteString n = ComposeT . lift . Random.uniformShortByteString n . Random.getConst

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
instance {-# OVERLAPPABLE #-} (Random.FrozenGen f (t2 m), MonadTrans t1) => Random.FrozenGen (Random.Const f (ComposeT t1 t2)) (ComposeT t1 t2 m) where
  type MutableGen (Random.Const f (ComposeT t1 t2)) (ComposeT t1 t2 m) = Random.Const (Random.MutableGen f (t2 m)) (ComposeT t1 t2)
  freezeGen = ComposeT . lift . fmap Random.Const . Random.freezeGen . Random.getConst
  thawGen = ComposeT . lift . fmap Random.Const . Random.thawGen . Random.getConst

-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
instance {-# OVERLAPPABLE #-} (Random.RandomGenM g r (t2 m), MonadTrans t1) => Random.RandomGenM (Random.Const g (ComposeT t1 t2)) r (ComposeT t1 t2 m) where
  applyRandomGenM f = ComposeT . lift . Random.applyRandomGenM f . Random.getConst
#endif

#if defined(VERSION_resourcet)
-- TODO: `MonadIO m` and `MonadTrans t2` should not be required for this instance
-- | /OVERLAPPABLE/.
-- Elevated to @(t2 m)@.
deriving via Elevator t1 (t2 (m :: Type -> Type))
  instance {-# OVERLAPPABLE #-}
    ( ResourceT.MonadResource (t2 m)
    , MonadTrans t1
    , MonadIO m
    , MonadTrans t2
    ) => ResourceT.MonadResource (ComposeT t1 t2 m)

-- TODO: `MonadIO m` and `MonadTrans t2` should not be required for this instance
-- | Set by 'ResourceT.ResourceT'.
deriving via ResourceT.ResourceT (t2 (m :: Type -> Type))
  instance
    ( MonadIO (t2 m)
    , MonadIO m
    , MonadTrans t2
    ) => ResourceT.MonadResource (ComposeT ResourceT.ResourceT t2 m)
#endif

#if defined(VERSION_unliftio_core)
-- | Elevated to @m@.
deriving via Elevator (ComposeT t1 t2) m
  instance
    ( UnliftIO.MonadUnliftIO m
    , MonadTransControlIdentity (ComposeT t1 t2)
    ) =>
    UnliftIO.MonadUnliftIO (ComposeT t1 t2 m)
#endif


-- ** Run 'ComposeT'
--
-- $runComposet
--
-- You have to run the composed monad transformers to get back into the base monad at some point.

-- | Run two stacked monad transformers.
--
-- This function takes the two individual monad transformer runners as arguments.
runComposeT :: (forall a. t1 (t2 m) a -> t2 m (StT t1 a)) -- ^ run @t1@
            -> (forall a. t2 m a -> m (StT t2 a)) -- ^ run @t2@
            -> (forall a. ComposeT t1 t2 m a -> m (StT t2 (StT t1 a)))
runComposeT runT1 runT2 = runT2 . runT1 . deComposeT

-- | Equivalent to 'runComposeT', but discards the monadic state 'StT'.
-- This is a simple approach when your monad transformer stack doesn't carry monadic state.
--
-- @
-- 'StT' ('ComposeT' t1 t2) a ~ a
-- @
--
-- This can be used to improve error messages when modifying a monad transformer stack.
runComposeT' :: (t1 (t2 m) a -> t2 m a) -- ^ run @t1@
             -> (t2 m a -> m a) -- ^ run @t2@
             -> (ComposeT t1 t2 m a -> m a)
runComposeT' runT1 runT2 = runT2 . runT1 . deComposeT

-- * Examples

-- ** Example 1: Create a new type class
--
-- $example1
--
-- When creating a new type class that supports 'ComposeT', you want to add recursive instances for
-- 'ComposeT'.
--
-- @
-- class 'Monad' m => MonadCustom m where
--   simpleMethod :: a -> m a
--   complicatedMethod :: (a -> m a) -> m a
-- @
--
-- You can easily derive those instances, after implementing an instance for 'Elevator'.
-- This is explained in "Control.Monad.Trans.Elevator".
--
-- Then it's possible to derive the recursive instance.
-- This is an /OVERLAPPABLE/ instance, because we want to be able to add new "base-case" instances
-- through transformers in a stack.
--
-- @
-- deriving via 'Elevator' t1 (t2 (m :: * -> *))
--   instance {-\# OVERLAPPABLE \#-}
--     ( 'Monad' (t1 (t2 m))
--     , 'MonadTransControl' t1
--     , MonadCustom (t2 m)
--     ) => MonadCustom ('ComposeT' t1 t2 m)
-- @

-- ** Example 2: Add an instance
--
-- $example2
--
-- Add a type class instance for a new monad transformer, when there already is a recursive instance
-- for 'ComposeT'.
--
-- @
-- newtype CustomT m a = CustomT { unCustomT :: t'Control.Monad.Trans.Identity.IdentityT' m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
--   deriving newtype ('MonadTrans', 'MonadTransControl', 'MonadTransControlIdentity')
-- @
--
-- First we need the regular instance.
-- The method implementations are 'undefined' here, because they would only distract from
-- 'ComposeT'.
--
-- @
-- instance 'Monad' m => MonadCustom (CustomT m) where
--   simpleMethod = 'undefined'
--   complicatedMethod = 'undefined'
-- @
--
-- To add a "base-case" instance, that takes priority over the recursive instance,
-- /FlexibleInstances/ are required.
--
-- @
-- deriving via CustomT (t2 (m :: * -> *))
--   instance 'Monad' (t2 m) => MonadCustom ('ComposeT' CustomT t2 m)
-- @

-- ** Example 3: Build a transformer stack
--
-- $example3
--
-- Create a monad transformer stack and wrap it using a newtype.
--
-- @
-- type AppStackT = t'Control.Monad.Trans.Compose.Transparent.TransparentT' t'Control.Monad.Trans.Compose.Infix..|>' 'Control.Monad.Trans.Reader.ReaderT' 'Bool' t'Control.Monad.Trans.Compose.Infix..|>' CustomT t'Control.Monad.Trans.Compose.Infix..|>' 'Control.Monad.Trans.Reader.ReaderT' 'Char' t'Control.Monad.Trans.Compose.Infix..|>' 'Control.Monad.Trans.State.Lazy.StateT' 'Int'
-- newtype AppT m a = AppT { unAppT :: AppStackT m a }
--   deriving newtype ('Functor', 'Applicative', 'Monad')
-- @
--
-- Using t'Control.Monad.Trans.Compose.Infix..|>' we can write @AppStackT@ in the order of initialization.
-- We are adding t'Control.Monad.Trans.Compose.Transparent.TransparentT' to the bottom of the stack,
-- so that all the other transformer instances actually end up in the stack.
-- Now we can simply derive just the instances, that we want.
--
-- @
--   deriving newtype ('MonadTrans', 'MonadTransControl')
--   deriving newtype ('Control.Monad.State.Class.MonadState' 'Int')
--   deriving newtype MonadCustom
-- @
--
-- We can even access instances, that would have been shadowed in a regular transformer stack.
--
-- @
--   deriving newtype ('Control.Monad.Reader.Class.MonadReader' 'Bool')
-- @

-- ** Example 4: Run a transformer stack
--
-- $example4
--
-- This is the part, that actually contains your application logic.
-- Because of the setup with 'ComposeT', we won't have to worry about 'lift'ing during the
-- initialization.
-- With 'Control.Monad.Trans.Compose.Infix...>' we can use the order of initialization again.
--
-- @
-- runAppT :: AppT m a -> m ('StT' AppT a)
-- runAppT appTma =
--   'Control.Monad.Trans.Compose.Transparent.runTransparentT'
--     'Control.Monad.Trans.Compose.Infix../>' (\\ tma -> 'Control.Monad.Trans.Reader.runReaderT' tma 'True')
--     'Control.Monad.Trans.Compose.Infix../>' runCustomT
--     'Control.Monad.Trans.Compose.Infix../>' runReaderT'
--     'Control.Monad.Trans.Compose.Infix../>' runStateT'
--     $ unAppT appTma
--  where
--   runReaderT' :: 'Control.Monad.Reader.Class.MonadReader' 'Bool' m => 'Control.Monad.Trans.Reader.ReaderT' 'Char' m a -> m a
--   runReaderT' tma = do
--     bool <- 'Control.Monad.Reader.Class.ask'
--     let char = if bool then \'Y\' else \'N\'
--     'Control.Monad.Trans.Reader.runReaderT' tma char
--
--   runStateT' :: 'Control.Monad.Reader.Class.MonadReader' 'Char' m => 'Control.Monad.Trans.State.Lazy.StateT' 'Int' m a -> m (a, 'Int')
--   runStateT' tma = do
--     char <- 'Control.Monad.Reader.Class.ask'
--     let num = 'fromEnum' char
--     'Control.Monad.Trans.State.Lazy.runStateT' tma num
-- @
