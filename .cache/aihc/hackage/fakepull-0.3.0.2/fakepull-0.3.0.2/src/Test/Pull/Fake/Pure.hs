{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module Test.Pull.Fake.Pure
  ( PullT (..)
  , PullM
  , MonadPull
  , runPullT
  , evalPullT
  , execPullT
  , runPullM
  , evalPullM
  , execPullM
  , pull
  ) where

import           Control.Applicative        (Alternative)
import           Control.Monad              (MonadPlus)
import           Control.Monad.Catch        (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Cont.Class   (MonadCont)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.RWS.Class    (MonadRWS, MonadReader, MonadState,
                                             MonadWriter)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Control.Monad.Trans        (MonadTrans)
import           Data.Functor.Identity      (Identity, runIdentity)

#if __GLASGOW_HASKELL__ < 808
import           Control.Monad.Fail         (MonadFail)
#endif


newtype PullT payload m a =
  PullT { unPullT :: StateT [payload] m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , Alternative
    , MonadFix
    , MonadFail
    , MonadPlus
    , MonadState [payload]
    , MonadReader r
    , MonadWriter w
    , MonadRWS r w [payload]
    , MonadCont
    , MonadError e
    , MonadCatch
    , MonadMask
    , MonadThrow
    )

type PullM payload = PullT payload Identity


type MonadPull payload = MonadState [payload]


runPullT :: PullT payload m a -> [payload] -> m (a, [payload])
runPullT (PullT act) = runStateT act


evalPullT :: Functor f => PullT payload f a -> [payload] -> f a
evalPullT act = fmap fst . runPullT act


execPullT :: Functor f => PullT payload f a -> [payload] -> f [payload]
execPullT act = fmap snd . runPullT act


runPullM :: PullM payload a -> [payload] -> (a, [payload])
runPullM act = runIdentity . runPullT act


evalPullM :: PullM payload a -> [payload] -> a
evalPullM act = fst . runPullM act


execPullM :: PullM payload a -> [payload] -> [payload]
execPullM act = snd . runPullM act


pull :: MonadPull payload m => m (Maybe payload)
pull =
  get >>= \case
    [] -> return Nothing
    hd : tl -> do
      put tl
      return $ Just hd
