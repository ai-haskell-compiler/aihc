{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Control.Monad.Exception.Instances
-- Copyright   :  (c) Harvard University 2008-2011
--                (c) Geoffrey Mainland 2011-2014
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Control.Monad.Exception.Instances where

import Control.Monad.Cont (MonadCont(..))
import Control.Monad.Exception (ExceptionT(..),
                                runExceptionT)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Control.Monad.Trans.Class (MonadTrans(..))

--
-- mtl2 instances for transformed monads.
--

instance (MonadCont m) => MonadCont (ExceptionT m) where
    callCC f = ExceptionT $
        callCC $ \c ->
        runExceptionT (f (\a -> ExceptionT $ c (Right a)))

instance (MonadRWS r w s m) => MonadRWS r w s (ExceptionT m)

instance (MonadReader r m) => MonadReader r (ExceptionT m) where
    ask       = lift ask
    local f m = ExceptionT $ local f (runExceptionT m)

instance (MonadState s m) => MonadState s (ExceptionT m) where
    get = lift get
    put = lift . put

instance (MonadWriter w m) => MonadWriter w (ExceptionT m) where
    tell     = lift . tell
    listen m = ExceptionT $ do
        (a, w) <- listen (runExceptionT m)
        case a of
          Left  l -> return $ Left l
          Right r -> return $ Right (r, w)
    pass m   = ExceptionT $ pass $ do
        a <- runExceptionT m
        case a of
          Left l       -> return (Left l, id)
          Right (r, f) -> return (Right r, f)
