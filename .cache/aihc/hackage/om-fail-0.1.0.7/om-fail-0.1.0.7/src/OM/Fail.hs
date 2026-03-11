{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- | Monad transformer providing 'MonadFail'. -}
module OM.Fail (
  FailT,
  runFailT,
) where


import Control.Exception.Safe (MonadThrow, MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO)
import Control.Monad.Trans.Class (MonadTrans)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)


{- | Monad transformer providing 'MonadFail'. -}
newtype FailT m a = FailT {
    unFailT :: ExceptT String m a
  }
  deriving newtype (
    Functor, Applicative, Monad, MonadIO, MonadLogger, MonadLoggerIO,
    MonadThrow, MonadCatch, MonadTrans
  )
instance (Monad m) => MonadFail (FailT m) where
  fail = FailT . throwE


{- | Run a 'FailT'. -}
runFailT :: FailT m a -> m (Either String a)
runFailT = runExceptT . unFailT


