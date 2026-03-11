{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description: Orphan instances for monad-logger's LoggingT
-}
module Control.Monad.Logger.Orphans where

import Control.Monad.Logger

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

#if !MIN_VERSION_monad_logger(0,3,40)
import Control.Monad.Trans (lift)

instance (Monad m, Alternative m) => Alternative (LoggingT m) where
  empty = lift empty
  a <|> b = LoggingT $ \f -> runLoggingT a f <|> runLoggingT b f
#endif

instance (Monad m, Alternative m) => MonadPlus (LoggingT m)
