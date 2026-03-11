{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Run.TChan where

import Control.Monad.Trans
import Control.Moffy
import Control.Moffy.Run (Handle, HandleSt, St)
import Control.Concurrent.STM

import qualified Control.Moffy.Run as M

import Data.Map

interpret :: (MonadIO m, Adjustable es es') =>
	Handle m es' -> TChan a -> Sig s es a r -> m r
interpret h c = M.interpret h (liftIO . atomically . writeTChan c)

interpretSt :: (MonadIO m, Adjustable es es') =>
	HandleSt st m es' -> TChan a -> Sig s es a r -> St st m r
interpretSt h c = M.interpretSt h (liftIO . atomically . writeTChan c)

interpretSt' :: (MonadIO m, Adjustable es es', Ord k) =>
	HandleSt st m es' -> TVar (Map k v) -> TChan a -> Sig s es ([(k, Maybe v)], a) r -> St st m r
interpretSt' h vm c = M.interpretSt h \(kvs, x) -> liftIO $ atomically do
	modifyTVar vm $ flip (Prelude.foldr (uncurry maybeInsert)) kvs
	writeTChan c x

maybeInsert :: Ord k => k -> Maybe a -> Map k a -> Map k a
maybeInsert k = \case Nothing -> delete k; Just v -> insert k v
