{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Appendful.Persistent
  ( -- * Client side
    clientMakeSyncRequestQuery,
    clientMergeSyncResponseQuery,

    -- ** Raw processors
    clientSyncProcessor,

    -- * Server side
    serverProcessSyncQuery,
    serverProcessSyncWithCustomIdQuery,

    -- ** Sync processors
    serverSyncProcessor,
    serverSyncProcessorWithCustomId,

    -- * Utils

    -- ** Client side
    setupUnsyncedClientQuery,
    setupClientQuery,
    clientGetStoreQuery,

    -- ** Server side side
    serverGetStoreQuery,
    setupServerQuery,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Appendful
import qualified Data.Map as M
import Data.Maybe
import Database.Persist
import Database.Persist.Sql
import Lens.Micro

-- | Make a sync request on the client side
clientMakeSyncRequestQuery ::
  forall sid clientRecord a m.
  ( PersistEntity clientRecord,
    PersistField sid,
    PersistEntityBackend clientRecord ~ SqlBackend,
    MonadIO m
  ) =>
  -- | How to read a record
  (clientRecord -> a) ->
  -- | The server id field
  EntityField clientRecord (Maybe sid) ->
  SqlPersistT m (SyncRequest (Key clientRecord) sid a)
clientMakeSyncRequestQuery func serverIdField = do
  syncRequestAdded <-
    M.fromList . map (\(Entity cid ct) -> (cid, func ct))
      <$> selectList
        [serverIdField ==. Nothing]
        []
  syncRequestMaximumSynced <-
    ((\e -> e ^. fieldLens serverIdField) =<<)
      <$> selectFirst
        [serverIdField !=. Nothing]
        [Desc serverIdField] ::
      SqlPersistT m (Maybe sid)
  pure SyncRequest {..}

-- | Merge a sync response on the client side
clientMergeSyncResponseQuery ::
  ( PersistEntity clientRecord,
    PersistField sid,
    PersistEntityBackend clientRecord ~ SqlBackend,
    SafeToInsert clientRecord,
    MonadIO m
  ) =>
  -- | Create an un-deleted synced record on the client side
  (sid -> a -> clientRecord) ->
  -- | The server id field
  EntityField clientRecord (Maybe sid) ->
  SyncResponse (Key clientRecord) sid a ->
  SqlPersistT m ()
clientMergeSyncResponseQuery func serverIdField = mergeSyncResponseCustom $ clientSyncProcessor func serverIdField

clientSyncProcessor ::
  ( PersistEntity clientRecord,
    PersistField sid,
    PersistEntityBackend clientRecord ~ SqlBackend,
    SafeToInsert clientRecord,
    MonadIO m
  ) =>
  -- | Create an un-deleted synced record on the client side
  (sid -> a -> clientRecord) ->
  -- | The server id field
  EntityField clientRecord (Maybe sid) ->
  ClientSyncProcessor (Key clientRecord) sid a (SqlPersistT m)
clientSyncProcessor func serverIdField = ClientSyncProcessor {..}
  where
    clientSyncProcessorSyncServerAdded m = forM_ (M.toList m) $ \(si, st) ->
      insert_ $ func si st
    clientSyncProcessorSyncClientAdded m = forM_ (M.toList m) $ \(cid, sid) ->
      update cid [serverIdField =. Just sid]

-- | Process a sync query on the server side.
serverProcessSyncQuery ::
  ( PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    SafeToInsert record,
    MonadIO m
  ) =>
  -- | Filters to select the relevant items
  --
  -- Use these if you have multiple users and you want to sync per-user
  [Filter record] ->
  -- | How to read a record
  (record -> a) ->
  -- | How to insert a _new_ record
  (a -> record) ->
  SyncRequest ci (Key record) a ->
  SqlPersistT m (SyncResponse ci (Key record) a)
serverProcessSyncQuery filters funcTo funcFrom = processServerSyncCustom $ serverSyncProcessor filters funcTo funcFrom

-- | A server sync processor that uses the sqlkey of the record as the name
serverSyncProcessor ::
  ( PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    SafeToInsert record,
    MonadIO m
  ) =>
  -- | Filters to select the relevant items
  --
  -- Use these if you have multiple users and you want to sync per-user
  [Filter record] ->
  -- | How to read a record
  (record -> a) ->
  -- | How to insert a _new_ record
  (a -> record) ->
  ServerSyncProcessor ci (Key record) a (SqlPersistT m)
serverSyncProcessor filters funcTo funcFrom =
  ServerSyncProcessor {..}
  where
    serverSyncProcessorRead = M.fromList . map (\(Entity i record) -> (i, funcTo record)) <$> selectList filters []
    serverSyncProcessorAddItems = mapM $ insert . funcFrom

-- | Process a sync query on the server side with a custom id.
serverProcessSyncWithCustomIdQuery ::
  ( Ord sid,
    PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    SafeToInsert record,
    MonadIO m
  ) =>
  -- | The action to generate new identifiers
  SqlPersistT m sid ->
  -- | Filters to select the relevant items
  --
  -- Use these if you have multiple users and you want to sync per-user
  [Filter record] ->
  -- | How to read a record
  (record -> (sid, a)) ->
  -- | How to insert a _new_ record
  (sid -> a -> record) ->
  SyncRequest ci sid a ->
  SqlPersistT m (SyncResponse ci sid a)
serverProcessSyncWithCustomIdQuery genId filters funcTo funcFrom = processServerSyncCustom $ serverSyncProcessorWithCustomId genId filters funcTo funcFrom

-- | A server sync processor that uses a custom key as the name
serverSyncProcessorWithCustomId ::
  ( Ord sid,
    PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    SafeToInsert record,
    MonadIO m
  ) =>
  -- | The action to generate new identifiers
  SqlPersistT m sid ->
  -- | Filters to select the relevant items
  --
  -- Use these if you have multiple users and you want to sync per-user
  [Filter record] ->
  -- | How to read a record
  (record -> (sid, a)) ->
  -- | How to insert a _new_ record
  (sid -> a -> record) ->
  ServerSyncProcessor ci sid a (SqlPersistT m)
serverSyncProcessorWithCustomId genId filters funcTo funcFrom =
  ServerSyncProcessor {..}
  where
    serverSyncProcessorRead = M.fromList . map (funcTo . entityVal) <$> selectList filters []
    serverSyncProcessorAddItems = mapM $ \a -> do
      sid <- genId
      let record = funcFrom sid a
      insert_ record
      pure sid

-- | Setup an unsynced client store
--
-- You shouldn't need this.
setupUnsyncedClientQuery ::
  ( PersistEntity clientRecord,
    PersistEntityBackend clientRecord ~ SqlBackend,
    SafeToInsert clientRecord,
    MonadIO m
  ) =>
  -- | How to insert a _new_ record
  (a -> clientRecord) ->
  [a] ->
  SqlPersistT m ()
setupUnsyncedClientQuery func = mapM_ (insert . func)

-- | Setup a client store
--
-- You shouldn't need this.
setupClientQuery ::
  ( PersistEntity clientRecord,
    PersistEntityBackend clientRecord ~ SqlBackend,
    SafeToInsert clientRecord,
    MonadIO m
  ) =>
  -- | Create an un-deleted unsynced record on the client side
  (a -> clientRecord) ->
  -- | Create an un-deleted synced record on the client side
  (sid -> a -> clientRecord) ->
  ClientStore (Key clientRecord) sid a ->
  SqlPersistT m ()
setupClientQuery funcU funcS ClientStore {..} = do
  forM_ (M.toList clientStoreAdded) $ \(cid, st) ->
    insertKey
      cid
      (funcU st)
  forM_ (M.toList clientStoreSynced) $ \(sid, st) ->
    insert_ (funcS sid st)

-- | Get a client store
--
-- You shouldn't need this.
clientGetStoreQuery ::
  ( Ord sid,
    PersistEntity clientRecord,
    PersistField sid,
    PersistEntityBackend clientRecord ~ SqlBackend,
    MonadIO m
  ) =>
  -- | How to red a record
  (clientRecord -> a) ->
  -- | The server id field
  EntityField clientRecord (Maybe sid) ->
  SqlPersistT m (ClientStore (Key clientRecord) sid a)
clientGetStoreQuery func serverIdField = do
  clientStoreAdded <-
    M.fromList . map (\(Entity cid ct) -> (cid, func ct))
      <$> selectList
        [ serverIdField ==. Nothing
        ]
        []
  clientStoreSynced <-
    M.fromList . mapMaybe (\e@(Entity _ ct) -> (,) <$> (e ^. fieldLens serverIdField) <*> pure (func ct))
      <$> selectList
        [ serverIdField !=. Nothing
        ]
        []
  pure ClientStore {..}

-- | Get the server store from the database
--
-- You shouldn't need this.
serverGetStoreQuery ::
  ( PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    MonadIO m
  ) =>
  -- | How to read a record
  (record -> a) ->
  SqlPersistT m (ServerStore (Key record) a)
serverGetStoreQuery func = ServerStore . M.fromList . map (\(Entity stid st) -> (stid, func st)) <$> selectList [] []

-- | Set up a server store in the database.
--
-- You shouldn't need this.
-- This uses 'insertKey' function and is therefore unsafe.
setupServerQuery ::
  ( PersistEntity record,
    PersistEntityBackend record ~ SqlBackend,
    MonadIO m
  ) =>
  -- | How to write a record
  (a -> record) ->
  ServerStore (Key record) a ->
  SqlPersistT m ()
setupServerQuery func ServerStore {..} = forM_ (M.toList serverStoreItems) $ \(i, e) -> void $ insertKey i $ func e

#if !MIN_VERSION_persistent(2,14,0)
type SafeToInsert a = a ~ a
#endif
