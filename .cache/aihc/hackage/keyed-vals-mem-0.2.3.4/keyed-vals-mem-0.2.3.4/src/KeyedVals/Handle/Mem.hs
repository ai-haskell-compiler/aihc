{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Provides an in-memory 'Handle' implementation.
-}
module KeyedVals.Handle.Mem (
  -- * functions
  new,

  -- * module re-exports
  module KeyedVals.Handle,
) where

import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import KeyedVals.Handle
import KeyedVals.Handle.Internal (Handle (..))
import Numeric.Natural (Natural)
import UnliftIO.STM (
  STM,
  TVar,
  atomically,
  newTVarIO,
  readTVar,
  writeTVar,
 )


-- | Create a new 'Handle'.
new :: MonadUnliftIO m => m (Handle m)
new = do
  v <- liftIO $ newTVarIO (mempty, False)
  pure $
    Handle
      { hLoadVal = hLoadVal' v
      , hSaveVal = hSaveVal' v
      , hCountKVs = hCountKVs' v
      , hLoadKVs = hLoadKVs' v
      , hSaveKVs = hSaveKVs' v
      , hUpdateKVs = hUpdateKVs' v
      , hLoadFrom = hLoadFrom' v
      , hSaveTo = hSaveTo' v
      , hLoadSlice = hLoadSlice' v
      , hDeleteSelected = hDeleteSelected' v
      , hDeleteSelectedKVs = hDeleteSelectedKVs' v
      , hClose = hClose' v
      }


-- | Implement an in-memory 'Handle'.
type InMem = Map Key InMemValue


-- | Store an in-memory 'Handle'.
type InMemVar = TVar (InMem, Bool)


-- | InMemValue represents a value to be stored in an in-memory 'Handle'.
data InMemValue
  = Dict !ValsByKey
  | Simple !Val


hClose' :: MonadUnliftIO m => InMemVar -> m ()
hClose' var = liftIO $
  atomically $ do
    (fh, _) <- readTVar var
    writeTVar var (fh, True)


hLoadVal' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  m (Either HandleErr (Maybe Val))
hLoadVal' var key = withInMemKey var key $ \case
  Nothing -> pure $ Right Nothing
  Just (Dict _) -> pure $ Left BadKey
  Just (Simple v) -> pure $ Right $ Just v


hSaveVal' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  Val ->
  m (Either HandleErr ())
hSaveVal' var key value = withInMem' var $ \values -> do
  updateInMem var $ Map.insert key (Simple value) values


hLoadKVs' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  m (Either HandleErr ValsByKey)
hLoadKVs' var key = withInMemKey var key $ \case
  Nothing -> pure $ Right Map.empty
  Just (Dict v) -> pure $ Right v
  Just (Simple _) -> pure $ Left BadKey


hLoadSlice' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  Selection ->
  m (Either HandleErr ValsByKey)
hLoadSlice' var key sel = withInMemKey var key $ \case
  Nothing -> pure $ Right Map.empty
  Just (Dict v) -> pure $ Right $ Map.filterWithKey (predOf sel) v
  Just (Simple _) -> pure $ Left BadKey


hCountKVs' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  m (Either HandleErr Natural)
hCountKVs' var key = withInMemKey var key $ \case
  Nothing -> pure $ Right 0
  Just (Dict v) -> pure $ Right $ fromInteger $ toInteger $ Map.size v
  Just (Simple _) -> pure $ Left BadKey


hSaveKVs' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  ValsByKey ->
  m (Either HandleErr ())
hSaveKVs' var key d = withInMem' var $ \values -> do
  updateInMem var $ Map.insert key (Dict d) values


hUpdateKVs' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  ValsByKey ->
  m (Either HandleErr ())
hUpdateKVs' var key d = withInMem' var $ \values ->
  case Map.lookup key values of
    Nothing -> do
      updateInMem var $ Map.insert key (Dict d) values
    Just (Dict d') -> do
      updateInMem var $ Map.insert key (Dict $ Map.union d d') values
    Just (Simple _) -> pure $ Left BadKey


updateInMem :: TVar (a, Bool) -> a -> STM (Either err ())
updateInMem var newMap = do
  writeTVar var (newMap, False)
  pure $ Right ()


hSaveTo' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  Key ->
  Val ->
  m (Either HandleErr ())
hSaveTo' var key dictKey value = withInMem' var $ \values ->
  case Map.lookup key values of
    Nothing -> do
      updateInMem var $ Map.insert key (Dict $ Map.singleton dictKey value) values
    Just (Dict d) -> do
      updateInMem var $ Map.insert key (Dict $ Map.insert dictKey value d) values
    Just (Simple _) -> pure $ Left BadKey


hLoadFrom' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  Key ->
  m (Either HandleErr (Maybe Val))
hLoadFrom' var key dictKey = withInMemKey var key $ \case
  Nothing -> pure $ Right Nothing
  Just (Dict d) -> pure $ Right $ Map.lookup dictKey d
  Just (Simple _) -> pure $ Left BadKey


hDeleteSelectedKVs' ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  Selection ->
  m (Either HandleErr ())
hDeleteSelectedKVs' var key sel = withInMem' var $ \values ->
  case Map.lookup key values of
    Nothing -> pure $ Right ()
    Just (Dict d) -> do
      writeTVar var (Map.insert key (Dict (Map.filterWithKey (notSel sel) d)) values, False)
      pure $ Right ()
    Just (Simple _) -> pure $ Left BadKey


hDeleteSelected' ::
  MonadUnliftIO m =>
  InMemVar ->
  Selection ->
  m (Either HandleErr ())
hDeleteSelected' var sel = withInMem' var $ \values -> do
  writeTVar var (Map.filterWithKey (notSel sel) values, False)
  pure $ Right ()


notSel :: Selection -> Key -> p -> Bool
notSel s k _ = not $ k `isIn` s


predOf :: Selection -> Key -> p -> Bool
predOf s k _ = k `isIn` s


withInMem :: MonadIO m => TVar t -> (t -> STM a) -> m a
withInMem v f = liftIO $ atomically $ readTVar v >>= f


withInMem' ::
  MonadUnliftIO m =>
  InMemVar ->
  (InMem -> STM (Either HandleErr a)) ->
  m (Either HandleErr a)
withInMem' var f = withInMem var $ \case
  (_, True) -> pure $ Left ConnectionClosed
  (values, _) -> f values


withInMemKey ::
  MonadUnliftIO m =>
  InMemVar ->
  Key ->
  (Maybe InMemValue -> STM (Either HandleErr a)) ->
  m (Either HandleErr a)
withInMemKey var key f = withInMem var $ \case
  (_, True) -> pure $ Left ConnectionClosed
  (values, _) -> f (Map.lookup key values)
