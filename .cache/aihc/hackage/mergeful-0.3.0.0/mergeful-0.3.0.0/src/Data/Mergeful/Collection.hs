{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A way to synchronise a single item with safe merge conflicts.
--
-- The setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
--
-- = A client should operate as follows:
--
-- The client starts with an 'initialClientStore'.
--
-- * The client produces a 'SyncRequest' with 'makeSyncRequest'.
-- * The client sends that request to the central server and gets a 'SyncResponse'.
-- * The client then updates its local store with 'mergeSyncResponseIgnoreProblems'.
--
--
-- = The central server should operate as follows:
--
-- The server starts with an 'initialServerStore'.
--
-- * The server accepts a 'SyncRequest'.
-- * The server performs operations according to the functionality of 'processServerSync' or 'processServerSyncCustom'.
-- * The server respons with a 'SyncResponse'.
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful.Collection
  ( -- * Client side
    ClientStore (..),
    Timed (..),
    ServerTime (..),
    initialClientStore,

    -- ** Querying the client store
    clientStoreSize,
    clientStoreClientIdSet,
    clientStoreUndeletedSyncIdSet,
    clientStoreSyncIdSet,
    clientStoreItems,

    -- ** Changing the client store
    addItemToClientStore,
    findFreeSpot,
    markItemDeletedInClientStore,
    changeItemInClientStore,
    deleteItemFromClientStore,

    -- ** Making a sync request
    SyncRequest (..),
    initialSyncRequest,
    makeSyncRequest,

    -- ** Merging the response
    SyncResponse (..),
    ClientAddition (..),
    ItemMergeStrategy (..),
    ChangeConflictResolution (..),
    ClientDeletedConflictResolution (..),
    ServerDeletedConflictResolution (..),
    mergeFromServerStrategy,
    mergeFromClientStrategy,
    mergeUsingCRDTStrategy,
    mergeSyncResponseFromServer,
    mergeSyncResponseFromClient,
    mergeSyncResponseUsingCRDT,
    mergeSyncResponseUsingStrategy,
    ClientSyncProcessor (..),
    mergeSyncResponseCustom,

    -- *** Utility functions for implementing pure client-side merging
    ClientId (..),
    mergeAddedItems,
    mergeSyncedButChangedItems,
    mergeDeletedItems,

    -- *** Utility functions for implementing custom client-side merging
    mergeSyncedButChangedConflicts,
    mergeClientDeletedConflicts,
    mergeServerDeletedConflicts,

    -- * Server side

    -- ** The store
    ServerStore (..),
    initialServerStore,

    -- ** Processing a sync request
    processServerSync,
    ServerSyncProcessor (..),
    processServerSyncCustom,
    emptySyncResponse,
    initialServerTime,
    incrementServerTime,
  )
where

import Autodocodec
import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.State
import Data.Aeson (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import Data.Kind
import Data.List
import Data.Map (Map)
import qualified Data.Map as M
import Data.Mergeful.Item
import Data.Mergeful.Timed
import Data.Set (Set)
import qualified Data.Set as S
import Data.Validity
import Data.Validity.Containers ()
import Data.Word
import GHC.Generics (Generic)

-- | A Client-side identifier for items.
--
-- These only need to be unique at the client.
newtype ClientId = ClientId
  { unClientId :: Word64
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Enum, Bounded, ToJSONKey, FromJSONKey)
  deriving (FromJSON, ToJSON) via (Autodocodec ClientId)

instance Validity ClientId

instance NFData ClientId

instance HasCodec ClientId where
  codec = dimapCodec ClientId unClientId codec <?> "ClientId"

data ClientStore ci si a = ClientStore
  { -- | These items are new locally but have not been synced to the server yet.
    clientStoreAddedItems :: !(Map ci a),
    -- | These items have been synced at their respective 'ServerTime's.
    clientStoreSyncedItems :: !(Map si (Timed a)),
    -- | These items have been synced at their respective 'ServerTime's
    -- but modified locally since then.
    clientStoreSyncedButChangedItems :: !(Map si (Timed a)),
    -- | These items have been deleted locally after they were synced
    -- but the server has not been notified of that yet.
    clientStoreDeletedItems :: !(Map si ServerTime)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ClientStore ci si a))

instance
  (Validity ci, Validity si, Show ci, Show si, Ord ci, Ord si, Validity a) =>
  Validity (ClientStore ci si a)
  where
  validate cs@ClientStore {..} =
    mconcat
      [ genericValidate cs,
        declare "There are no duplicate IDs" $
          distinct $
            concat
              [ M.keys clientStoreSyncedItems,
                M.keys clientStoreSyncedButChangedItems,
                M.keys clientStoreDeletedItems
              ]
      ]

instance (NFData ci, NFData si, NFData a) => NFData (ClientStore ci si a)

instance
  ( Ord ci,
    FromJSONKey ci,
    ToJSONKey ci,
    Ord si,
    FromJSONKey si,
    ToJSONKey si,
    Eq a,
    HasCodec a
  ) =>
  HasCodec (ClientStore ci si a)
  where
  codec =
    object "ClientStore" $
      ClientStore
        <$> optionalFieldWithOmittedDefault "added" M.empty "added items" .= clientStoreAddedItems
        <*> optionalFieldWithOmittedDefault "synced" M.empty "synced items" .= clientStoreSyncedItems
        <*> optionalFieldWithOmittedDefault "changed" M.empty "changed items" .= clientStoreSyncedButChangedItems
        <*> optionalFieldWithOmittedDefault "deleted" M.empty "deleted items" .= clientStoreDeletedItems

-- | A client store to start with.
--
-- This store contains no items.
initialClientStore :: ClientStore ci si a
initialClientStore =
  ClientStore
    { clientStoreAddedItems = M.empty,
      clientStoreSyncedItems = M.empty,
      clientStoreSyncedButChangedItems = M.empty,
      clientStoreDeletedItems = M.empty
    }

-- | The number of items in a client store
--
-- This does not count the deleted items, so that they really look deleted..
clientStoreSize :: ClientStore ci si a -> Word
clientStoreSize ClientStore {..} =
  fromIntegral $
    sum
      [ M.size clientStoreAddedItems,
        M.size clientStoreSyncedItems,
        M.size clientStoreSyncedButChangedItems
      ]

-- | The set of client ids.
--
-- These are only the client ids of the added items that have not been synced yet.
clientStoreClientIdSet :: ClientStore ci si a -> Set ci
clientStoreClientIdSet ClientStore {..} = M.keysSet clientStoreAddedItems

-- | The set of server ids.
--
-- This does not include the ids of items that have been marked as deleted.
clientStoreUndeletedSyncIdSet :: Ord si => ClientStore ci si a -> Set si
clientStoreUndeletedSyncIdSet ClientStore {..} =
  S.unions [M.keysSet clientStoreSyncedItems, M.keysSet clientStoreSyncedButChangedItems]

-- | The set of server ids.
--
-- This includes the ids of items that have been marked as deleted.
clientStoreSyncIdSet :: Ord si => ClientStore ci si a -> Set si
clientStoreSyncIdSet ClientStore {..} =
  S.unions
    [ M.keysSet clientStoreSyncedItems,
      M.keysSet clientStoreSyncedButChangedItems,
      M.keysSet clientStoreDeletedItems
    ]

-- | The set of items in the client store
--
-- This map does not include items that have been marked as deleted.
clientStoreItems :: (Ord ci, Ord si) => ClientStore ci si a -> Map (Either ci si) a
clientStoreItems ClientStore {..} =
  M.unions
    [ M.mapKeys Left clientStoreAddedItems,
      M.mapKeys Right $ M.map timedValue clientStoreSyncedItems,
      M.mapKeys Right $ M.map timedValue clientStoreSyncedButChangedItems
    ]

-- | Add an item to a client store as an added item.
--
-- This will take care of the uniqueness constraint of the 'ci's in the map.
addItemToClientStore ::
  (Ord ci, Enum ci, Bounded ci) => a -> ClientStore ci si a -> ClientStore ci si a
addItemToClientStore a cs =
  let oldAddedItems = clientStoreAddedItems cs
      newAddedItems =
        let newKey = findFreeSpot oldAddedItems
         in M.insert newKey a oldAddedItems
   in cs {clientStoreAddedItems = newAddedItems}

-- | Find a free client id to use
--
-- You shouldn't need this function, 'addItemToClientStore' takes care of this.
--
-- The values wrap around when reaching 'maxBound'.
findFreeSpot :: (Ord ci, Enum ci, Bounded ci) => Map ci a -> ci
findFreeSpot m =
  if M.null m
    then minBound
    else
      let (i, _) = M.findMax m
       in go (next i)
  where
    go i =
      if M.member i m
        then go (next i)
        else i
    next ci
      | ci == maxBound = minBound
      | otherwise = succ ci

-- | Mark an item deleted in a client store.
--
-- This function will not delete the item, but mark it as deleted instead.
markItemDeletedInClientStore :: Ord si => si -> ClientStore ci si a -> ClientStore ci si a
markItemDeletedInClientStore u cs =
  let oldSyncedItems = clientStoreSyncedItems cs
      oldChangedItems = clientStoreSyncedButChangedItems cs
      oldDeletedItems = clientStoreDeletedItems cs
      mItem = M.lookup u oldSyncedItems <|> M.lookup u oldChangedItems
   in case mItem of
        Nothing -> cs
        Just t ->
          let newSyncedItems = M.delete u oldSyncedItems
              newChangedItems = M.delete u oldChangedItems
              newDeletedItems = M.insert u (timedTime t) oldDeletedItems
           in cs
                { clientStoreSyncedItems = newSyncedItems,
                  clientStoreSyncedButChangedItems = newChangedItems,
                  clientStoreDeletedItems = newDeletedItems
                }

-- | Replace the given item with a new value.
--
-- This function will correctly mark the item as changed, if it exist.
--
-- It will not add an item to the store with the given id, because the
-- server may not have been the origin of that id.
changeItemInClientStore :: Ord si => si -> a -> ClientStore ci si a -> ClientStore ci si a
changeItemInClientStore i a cs =
  case M.lookup i (clientStoreSyncedItems cs) of
    Just t ->
      cs
        { clientStoreSyncedItems = M.delete i (clientStoreSyncedItems cs),
          clientStoreSyncedButChangedItems =
            M.insert i (t {timedValue = a}) (clientStoreSyncedButChangedItems cs)
        }
    Nothing ->
      case M.lookup i (clientStoreSyncedButChangedItems cs) of
        Nothing -> cs
        Just _ ->
          cs
            { clientStoreSyncedButChangedItems =
                M.adjust (\t -> t {timedValue = a}) i (clientStoreSyncedButChangedItems cs)
            }

-- | Delete an unsynced item from a client store.
--
-- This function will immediately delete the item, because it has never been synced.
deleteItemFromClientStore :: Ord ci => ci -> ClientStore ci si a -> ClientStore ci si a
deleteItemFromClientStore i cs = cs {clientStoreAddedItems = M.delete i (clientStoreAddedItems cs)}

newtype ServerStore si a = ServerStore
  { -- | A map of items, named using an 'si', together with the 'ServerTime' at which
    -- they were last synced.
    serverStoreItems :: Map si (Timed a)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ServerStore si a))

instance (Validity si, Show si, Ord si, Validity a) => Validity (ServerStore si a)

instance (NFData si, NFData a) => NFData (ServerStore si a)

instance
  ( Ord si,
    FromJSONKey si,
    ToJSONKey si,
    HasCodec a
  ) =>
  HasCodec (ServerStore si a)
  where
  codec = dimapCodec ServerStore serverStoreItems codec

-- | A server store to start with
--
-- This store contains no items.
initialServerStore :: ServerStore si a
initialServerStore = ServerStore {serverStoreItems = M.empty}

data SyncRequest ci si a = SyncRequest
  { -- | These items are new locally but have not been synced to the server yet.
    syncRequestNewItems :: !(Map ci a),
    -- | These items have been synced at their respective 'ServerTime's.
    syncRequestKnownItems :: !(Map si ServerTime),
    -- | These items have been synced at their respective 'ServerTime's
    -- but modified locally since then.
    syncRequestKnownButChangedItems :: !(Map si (Timed a)),
    -- | These items have been deleted locally after they were synced
    -- but the server has not been notified of that yet.
    syncRequestDeletedItems :: !(Map si ServerTime)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (SyncRequest ci si a))

instance
  (Validity ci, Validity si, Show ci, Show si, Ord ci, Ord si, Validity a) =>
  Validity (SyncRequest ci si a)
  where
  validate sr@SyncRequest {..} =
    mconcat
      [ genericValidate sr,
        declare "There are no duplicate IDs" $
          distinct $
            concat
              [ M.keys syncRequestKnownItems,
                M.keys syncRequestKnownButChangedItems,
                M.keys syncRequestDeletedItems
              ]
      ]

instance (NFData ci, NFData si, NFData a) => NFData (SyncRequest ci si a)

instance
  ( Ord ci,
    FromJSONKey ci,
    ToJSONKey ci,
    Ord si,
    FromJSONKey si,
    ToJSONKey si,
    Eq a,
    HasCodec a
  ) =>
  HasCodec (SyncRequest ci si a)
  where
  codec =
    object "SyncRequest" $
      SyncRequest
        <$> optionalFieldWithOmittedDefault "added" M.empty "new items" .= syncRequestNewItems
        <*> optionalFieldWithOmittedDefault "synced" M.empty "known items" .= syncRequestKnownItems
        <*> optionalFieldWithOmittedDefault "changed" M.empty "known but changed items" .= syncRequestKnownButChangedItems
        <*> optionalFieldWithOmittedDefault "deleted" M.empty "deleted items" .= syncRequestDeletedItems

-- | An intial 'SyncRequest' to start with.
--
-- It just asks the server to send over whatever it knows.
initialSyncRequest :: SyncRequest ci si a
initialSyncRequest =
  SyncRequest
    { syncRequestNewItems = M.empty,
      syncRequestKnownItems = M.empty,
      syncRequestKnownButChangedItems = M.empty,
      syncRequestDeletedItems = M.empty
    }

data ClientAddition i = ClientAddition
  { clientAdditionId :: !i,
    clientAdditionServerTime :: !ServerTime
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ClientAddition i))

instance Validity i => Validity (ClientAddition i)

instance NFData i => NFData (ClientAddition i)

instance HasCodec i => HasCodec (ClientAddition i) where
  codec =
    object "ClientAddition" $
      ClientAddition
        <$> requiredField "id" "client-side identifier" .= clientAdditionId
        <*> requiredField "time" "server-side time" .= clientAdditionServerTime

data SyncResponse ci si a = SyncResponse
  { -- | The client added these items and server has succesfully been made aware of that.
    --
    -- The client needs to update their server times
    syncResponseClientAdded :: !(Map ci (ClientAddition si)),
    -- | The client changed these items and server has succesfully been made aware of that.
    --
    -- The client needs to update their server times
    syncResponseClientChanged :: !(Map si ServerTime),
    -- | The client deleted these items and server has succesfully been made aware of that.
    --
    -- The client can delete them from its deleted items
    syncResponseClientDeleted :: !(Set si),
    -- | These items have been added on the server side
    --
    -- The client should add them too.
    syncResponseServerAdded :: !(Map si (Timed a)),
    -- | These items have been modified on the server side.
    --
    -- The client should modify them too.
    syncResponseServerChanged :: !(Map si (Timed a)),
    -- | These items were deleted on the server side
    --
    -- The client should delete them too
    syncResponseServerDeleted :: !(Set si),
    -- | These are conflicts where the server and the client both have an item, but it is different.
    --
    -- The server kept its part of each, the client can either take whatever the server gave them
    -- or deal with the conflicts somehow, and then try to re-sync.
    syncResponseConflicts :: !(Map si (Timed a)),
    -- | These are conflicts where the server has an item but the client does not.
    --
    -- The server kept its item, the client can either take whatever the server gave them
    -- or deal with the conflicts somehow, and then try to re-sync.
    syncResponseConflictsClientDeleted :: !(Map si (Timed a)),
    -- | These are conflicts where the server has no item but the client has a modified item.
    --
    -- The server left its item deleted, the client can either delete its items too
    -- or deal with the conflicts somehow, and then try to re-sync.
    syncResponseConflictsServerDeleted :: !(Set si)
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (SyncResponse ci si a))

instance
  (Validity ci, Validity si, Show ci, Show si, Ord ci, Ord si, Validity a) =>
  Validity (SyncResponse ci si a)
  where
  validate sr@SyncResponse {..} =
    mconcat
      [ genericValidate sr,
        declare "There are no duplicate IDs" $
          distinct $
            concat
              [ map (\(_, ClientAddition {..}) -> clientAdditionId) $ M.toList syncResponseClientAdded,
                M.keys syncResponseClientChanged,
                S.toList syncResponseClientDeleted,
                M.keys syncResponseServerAdded,
                M.keys syncResponseServerChanged,
                S.toList syncResponseServerDeleted,
                M.keys syncResponseConflicts,
                M.keys syncResponseConflictsClientDeleted,
                S.toList syncResponseConflictsServerDeleted
              ]
      ]

instance (NFData ci, NFData si, NFData a) => NFData (SyncResponse ci si a)

instance
  ( Ord ci,
    FromJSONKey ci,
    ToJSONKey ci,
    HasCodec ci,
    Ord si,
    FromJSONKey si,
    ToJSONKey si,
    HasCodec si,
    Eq a,
    HasCodec a
  ) =>
  HasCodec (SyncResponse ci si a)
  where
  codec =
    object "SyncResponse" $
      SyncResponse
        <$> optionalFieldWithOmittedDefault "client-added" M.empty "items added by the client" .= syncResponseClientAdded
        <*> optionalFieldWithOmittedDefault "client-changed" M.empty "items changed by the client" .= syncResponseClientChanged
        <*> optionalFieldWithOmittedDefault "client-deleted" S.empty "items deleted by the client" .= syncResponseClientDeleted
        <*> optionalFieldWithOmittedDefault "server-added" M.empty "items added by the server" .= syncResponseServerAdded
        <*> optionalFieldWithOmittedDefault "server-changed" M.empty "items changed by the server" .= syncResponseServerChanged
        <*> optionalFieldWithOmittedDefault "server-deleted" S.empty "items deleted by the server" .= syncResponseServerDeleted
        <*> optionalFieldWithOmittedDefault "conflict" M.empty "items that were changed simultaneously" .= syncResponseConflicts
        <*> optionalFieldWithOmittedDefault "conflict-client-deleted" M.empty "items that the server changed while the client deleted it" .= syncResponseConflictsClientDeleted
        <*> optionalFieldWithOmittedDefault "conflict-server-deleted" S.empty "items that the client changed while the server deleted it" .= syncResponseConflictsServerDeleted

-- | A sync response to start with.
--
-- It is entirely empty.
emptySyncResponse :: SyncResponse ci si a
emptySyncResponse =
  SyncResponse
    { syncResponseClientAdded = M.empty,
      syncResponseClientChanged = M.empty,
      syncResponseClientDeleted = S.empty,
      syncResponseServerAdded = M.empty,
      syncResponseServerChanged = M.empty,
      syncResponseServerDeleted = S.empty,
      syncResponseConflicts = M.empty,
      syncResponseConflictsClientDeleted = M.empty,
      syncResponseConflictsServerDeleted = S.empty
    }

-- | Produce an 'SyncRequest' from a 'ClientStore'.
--
-- Send this to the server for synchronisation.
makeSyncRequest :: ClientStore ci si a -> SyncRequest ci si a
makeSyncRequest ClientStore {..} =
  SyncRequest
    { syncRequestNewItems = clientStoreAddedItems,
      syncRequestKnownItems = M.map timedTime clientStoreSyncedItems,
      syncRequestKnownButChangedItems = clientStoreSyncedButChangedItems,
      syncRequestDeletedItems = clientStoreDeletedItems
    }

-- | Merge a 'SyncResponse' into the current 'ClientStore' by taking whatever the server gave the client in case of conflict.
--
-- Pro: Clients will converge on the same value.
--
-- __Con: Conflicting updates will be lost.__
mergeSyncResponseFromServer ::
  (Ord ci, Ord si) => ClientStore ci si a -> SyncResponse ci si a -> ClientStore ci si a
mergeSyncResponseFromServer =
  mergeSyncResponseUsingStrategy mergeFromServerStrategy

-- | Merge a 'SyncResponse' into the current 'ClientStore' by keeping whatever the client had in case of conflict.
--
-- Pro: No data will be lost
--
-- __Con: Clients will diverge when conflicts occur.__
mergeSyncResponseFromClient ::
  (Ord ci, Ord si) => ClientStore ci si a -> SyncResponse ci si a -> ClientStore ci si a
mergeSyncResponseFromClient = mergeSyncResponseUsingStrategy mergeFromClientStrategy

-- | Merge a 'SyncResponse' into the current 'ClientStore' by using the given GADT merging function in case of conflict
mergeSyncResponseUsingCRDT :: (Ord ci, Ord si) => (a -> a -> a) -> ClientStore ci si a -> SyncResponse ci si a -> ClientStore ci si a
mergeSyncResponseUsingCRDT = mergeSyncResponseUsingStrategy . mergeUsingCRDTStrategy

-- | Merge an 'SyncResponse' into the current 'ClientStore' with the given merge strategy.
--
-- In order for clients to converge on the same collection correctly, this function must be:
--
-- * Associative
-- * Idempotent
-- * The same on all clients
--
-- This function ignores mismatches.
mergeSyncResponseUsingStrategy ::
  (Ord ci, Ord si) =>
  ItemMergeStrategy a ->
  ClientStore ci si a ->
  SyncResponse ci si a ->
  ClientStore ci si a
mergeSyncResponseUsingStrategy strat cs sr =
  flip execState cs $ mergeSyncResponseCustom strat pureClientSyncProcessor sr

pureClientSyncProcessor :: forall ci si a. (Ord ci, Ord si) => ClientSyncProcessor ci si a (State (ClientStore ci si a))
pureClientSyncProcessor =
  ClientSyncProcessor
    { clientSyncProcessorQuerySyncedButChangedValues = \s ->
        gets
          ( \cs -> M.intersection (clientStoreSyncedButChangedItems cs) (M.fromSet (const ()) s)
          ),
      clientSyncProcessorSyncClientAdded = \m ->
        modify
          ( \cs ->
              let (leftovers, added) = mergeAddedItems (clientStoreAddedItems cs) m
               in cs {clientStoreAddedItems = leftovers, clientStoreSyncedItems = added `M.union` clientStoreSyncedItems cs}
          ),
      clientSyncProcessorSyncClientChanged = \m ->
        modify
          ( \cs ->
              let (leftovers, changed) = mergeSyncedButChangedItems (clientStoreSyncedButChangedItems cs) m
               in cs {clientStoreSyncedButChangedItems = leftovers, clientStoreSyncedItems = changed `M.union` clientStoreSyncedItems cs}
          ),
      clientSyncProcessorSyncClientDeleted = \s ->
        modify
          ( \cs ->
              let leftovers = mergeDeletedItems (clientStoreDeletedItems cs) s
               in cs {clientStoreDeletedItems = leftovers}
          ),
      clientSyncProcessorSyncChangeConflictKeepLocal = \_ -> pure (),
      clientSyncProcessorSyncChangeConflictMerged = \resolved ->
        modify
          ( \cs ->
              let newSyncedButChanged = M.union resolved (clientStoreSyncedButChangedItems cs)
               in cs {clientStoreSyncedButChangedItems = newSyncedButChanged, clientStoreSyncedItems = clientStoreSyncedItems cs `M.difference` newSyncedButChanged}
          ),
      clientSyncProcessorSyncChangeConflictTakeRemote = \m ->
        modify
          ( \cs ->
              let newSynced = m `M.union` clientStoreSyncedItems cs
               in cs {clientStoreSyncedItems = newSynced, clientStoreSyncedButChangedItems = clientStoreSyncedButChangedItems cs `M.difference` newSynced}
          ),
      clientSyncProcessorSyncClientDeletedConflictTakeRemoteChanged = \m ->
        modify (\cs -> cs {clientStoreSyncedItems = m `M.union` clientStoreSyncedItems cs}),
      clientSyncProcessorSyncClientDeletedConflictStayDeleted = \_ -> pure (),
      clientSyncProcessorSyncServerDeletedConflictKeepLocalChange = \_ -> pure (),
      clientSyncProcessorSyncServerDeletedConflictDelete = \s ->
        modify
          ( \cs ->
              let m = M.fromSet (const ()) s
               in cs
                    { clientStoreSyncedItems = clientStoreSyncedItems cs `M.difference` m,
                      clientStoreSyncedButChangedItems = clientStoreSyncedButChangedItems cs `M.difference` m
                    }
          ),
      clientSyncProcessorSyncServerAdded = \m ->
        modify (\cs -> cs {clientStoreSyncedItems = m `M.union` clientStoreSyncedItems cs}),
      clientSyncProcessorSyncServerChanged = \m ->
        modify
          ( \cs ->
              let newSynced = m `M.union` clientStoreSyncedItems cs
               in cs {clientStoreSyncedItems = newSynced, clientStoreSyncedButChangedItems = clientStoreSyncedButChangedItems cs `M.difference` newSynced}
          ),
      clientSyncProcessorSyncServerDeleted = \s ->
        modify
          ( \cs ->
              let m = M.fromSet (const ()) s
               in cs
                    { clientStoreSyncedItems = clientStoreSyncedItems cs `M.difference` m,
                      clientStoreSyncedButChangedItems = clientStoreSyncedButChangedItems cs `M.difference` m
                    }
          )
    }

-- | Merge the local added items with the ones that the server has acknowledged as added.
mergeAddedItems ::
  forall ci si a.
  (Ord ci, Ord si) =>
  Map ci a ->
  Map ci (ClientAddition si) ->
  (Map ci a, Map si (Timed a))
mergeAddedItems local added = M.foldlWithKey go (M.empty, M.empty) local
  where
    go :: (Map ci a, Map si (Timed a)) -> ci -> a -> (Map ci a, Map si (Timed a))
    go (as, m) ci a =
      case M.lookup ci added of
        Nothing -> (M.insert ci a as, m)
        Just ClientAddition {..} ->
          ( as,
            M.insert
              clientAdditionId
              (Timed {timedValue = a, timedTime = clientAdditionServerTime})
              m
          )

-- | Merge the local synced but changed items with the ones that the server has acknowledged as changed.
mergeSyncedButChangedItems ::
  forall i a.
  Ord i =>
  Map i (Timed a) ->
  Map i ServerTime ->
  (Map i (Timed a), Map i (Timed a))
mergeSyncedButChangedItems local changed = M.foldlWithKey go (M.empty, M.empty) local
  where
    go :: (Map i (Timed a), Map i (Timed a)) -> i -> Timed a -> (Map i (Timed a), Map i (Timed a))
    go (m1, m2) k t =
      case M.lookup k changed of
        Nothing -> (M.insert k t m1, m2)
        Just st' -> (m1, M.insert k (t {timedTime = st'}) m2)

-- | Merge the local deleted items with the ones that the server has acknowledged as deleted.
mergeDeletedItems :: Ord i => Map i b -> Set i -> Map i b
mergeDeletedItems m s = m `M.difference` M.fromSet (const ()) s

-- | A processor for dealing with @SyncResponse@s on the client side.
--
-- It has to deal with each of the 13 cases:
--
-- - server
--
--     - added
--     - changed
--     - deleted
--
-- - client
--
--     - added
--     - changed
--     - deleted
--
-- - client-deleted conflict
--
--     - take remote
--     - delete
--
-- - server-deleted conflict
--
--     - delete
--     - keep local
--
-- - change conflict
--
--     - take remote
--     - merge
--     - keep local
--
-- It is a lot of work to implement one of these, so make sure to have a look at the mergeful companion packages to see if maybe there is already one for your application domain.
data ClientSyncProcessor ci si a (m :: Type -> Type) = ClientSyncProcessor
  { -- | Get the synced values with keys in the given set
    clientSyncProcessorQuerySyncedButChangedValues :: !(Set si -> m (Map si (Timed a))),
    -- | Complete additions that were acknowledged by the server.
    --
    -- This involves saving the server id and the server time
    clientSyncProcessorSyncClientAdded :: !(Map ci (ClientAddition si) -> m ()),
    -- | Complete changes that were acknowledged by the server
    --
    -- This involves updating the server time
    clientSyncProcessorSyncClientChanged :: !(Map si ServerTime -> m ()),
    -- | Complete deletions that were acknowledged by the server
    --
    -- This means deleting these tombstoned items entirely
    clientSyncProcessorSyncClientDeleted :: !(Set si -> m ()),
    -- | Re-create the items that need to be created locally as a result of a 'client deleted' conflict that has been merged by taking the remote value.
    --
    -- You can likely implement this in the same way as @clientSyncProcessorSyncServerAdded@.
    clientSyncProcessorSyncClientDeletedConflictTakeRemoteChanged :: !(Map si (Timed a) -> m ()),
    -- | Leave the items deleted that need to be left deleted as a result of a 'client deleted' conflict that has been merged by leaving it deleted.
    --
    -- You likely don't have to do anything with these, as they are the way that has been decided they should be, but you may want to log them or so.
    clientSyncProcessorSyncClientDeletedConflictStayDeleted :: !(Map si (Timed a) -> m ()),
    -- | Leave the items undeleted that need to be left deleted as a result of a 'server deleted' conflict that has been merged by leaving it undeleted.
    --
    -- You likely don't have to do anything with these, as they are the way that has been decided they should be, but you may want to log them or so.
    clientSyncProcessorSyncServerDeletedConflictKeepLocalChange :: !(Set si -> m ()),
    -- | Delete the items that need to be deleted locally as a result of a 'server deleted' conflict that has been merged by deleting the local value.
    --
    -- You can likely implement this in the same way as @clientSyncProcessorSyncServerDeleted@.
    clientSyncProcessorSyncServerDeletedConflictDelete :: !(Set si -> m ()),
    -- | Deal with the items for which no conflict was resolved.
    --
    -- You likely don't have to do anything with these, as they are the way that has been decided they should be, but you may want to log them or so.
    clientSyncProcessorSyncChangeConflictKeepLocal :: !(Map si (Timed a) -> m ()),
    -- | Store the items that were in a conflict but the conflict was resolved correctly.
    -- These items should be marked as changed.
    clientSyncProcessorSyncChangeConflictMerged :: !(Map si (Timed a) -> m ()),
    -- | Store the items that were in a conflict but the client will take the remote values
    -- These items should be marked as unchanged.
    --
    -- You can likely implement this in the same way as @clientSyncProcessorSyncServerChanged@.
    clientSyncProcessorSyncChangeConflictTakeRemote :: !(Map si (Timed a) -> m ()),
    -- | Store the items that the server added
    clientSyncProcessorSyncServerAdded :: !(Map si (Timed a) -> m ()),
    -- | Store the items that the server changed
    clientSyncProcessorSyncServerChanged :: !(Map si (Timed a) -> m ()),
    -- | Store the items that the server deleted
    clientSyncProcessorSyncServerDeleted :: !(Set si -> m ())
  }
  deriving (Generic)

mergeSyncResponseCustom :: (Ord si, Monad m) => ItemMergeStrategy a -> ClientSyncProcessor ci si a m -> SyncResponse ci si a -> m ()
mergeSyncResponseCustom ItemMergeStrategy {..} ClientSyncProcessor {..} SyncResponse {..} = do
  -- Every client deleted conflict needs to be added, if the sync processor says so
  let (remoteChangeToTake, remoteChangesToIgnore) = mergeClientDeletedConflicts itemMergeStrategyMergeClientDeletedConflict syncResponseConflictsClientDeleted
  -- Every change conflict, unless the client item is kept, needs to be updated
  -- The unresolved conflicts don't need to be updated.
  clientChangeConflicts <- clientSyncProcessorQuerySyncedButChangedValues $ M.keysSet syncResponseConflicts
  let (unresolvedChangeConflicts, mergedChangeConflicts, resolvedChangeConflicts) = mergeSyncedButChangedConflicts itemMergeStrategyMergeChangeConflict clientChangeConflicts syncResponseConflicts
  -- Every served deleted conflict needs to be deleted, if the sync processor says so
  clientServerDeletedConflicts <- clientSyncProcessorQuerySyncedButChangedValues syncResponseConflictsServerDeleted
  let (localChangesToDelete, localChangesToBeKept) = mergeServerDeletedConflicts itemMergeStrategyMergeServerDeletedConflict clientServerDeletedConflicts
  -- The order here matters.
  clientSyncProcessorSyncServerAdded syncResponseServerAdded
  clientSyncProcessorSyncServerChanged syncResponseServerChanged
  clientSyncProcessorSyncServerDeleted syncResponseServerDeleted
  clientSyncProcessorSyncChangeConflictTakeRemote resolvedChangeConflicts
  clientSyncProcessorSyncChangeConflictMerged mergedChangeConflicts
  clientSyncProcessorSyncChangeConflictKeepLocal unresolvedChangeConflicts
  clientSyncProcessorSyncClientDeletedConflictTakeRemoteChanged remoteChangeToTake
  clientSyncProcessorSyncClientDeletedConflictStayDeleted remoteChangesToIgnore
  clientSyncProcessorSyncServerDeletedConflictDelete localChangesToDelete
  clientSyncProcessorSyncServerDeletedConflictKeepLocalChange localChangesToBeKept
  clientSyncProcessorSyncClientDeleted syncResponseClientDeleted
  clientSyncProcessorSyncClientChanged syncResponseClientChanged
  clientSyncProcessorSyncClientAdded syncResponseClientAdded

-- | Resolve change conflicts
mergeSyncedButChangedConflicts ::
  forall si a.
  Ord si =>
  (a -> a -> ChangeConflictResolution a) ->
  -- | The conflicting items on the client side
  Map si (Timed a) ->
  -- | The conflicting items on the server side
  Map si (Timed a) ->
  -- | Unresolved conflicts on the left, merged conflicts in the middle, resolved conflicts on the right
  --
  -- * The unresolved conflicts should remain as-is
  -- * The merged conflicts should be updated and marked as changed
  -- * The resolved conflicts should be updated and marked as unchanged
  (Map si (Timed a), Map si (Timed a), Map si (Timed a))
mergeSyncedButChangedConflicts func clientItems =
  M.foldlWithKey go (M.empty, M.empty, M.empty)
  where
    go ::
      (Map si (Timed a), Map si (Timed a), Map si (Timed a)) ->
      si ->
      Timed a ->
      (Map si (Timed a), Map si (Timed a), Map si (Timed a))
    go tup@(unresolved, merged, resolved) key s@(Timed si st) = case M.lookup key clientItems of
      Nothing -> tup -- TODO not even sure what this would mean. Should not happen I guess. Just throw it away
      Just c@(Timed ci _) -> case func ci si of
        KeepLocal ->
          (M.insert key c unresolved, merged, resolved)
        Merged mi ->
          (unresolved, M.insert key (Timed mi st) merged, M.insert key s resolved)
        TakeRemote ->
          (unresolved, merged, M.insert key s resolved)

-- | Resolve client deleted conflicts
mergeClientDeletedConflicts ::
  (a -> ClientDeletedConflictResolution) ->
  -- | The conflicting items on the server side
  Map si (Timed a) ->
  -- | A map of items that need to be updated on the client.
  (Map si (Timed a), Map si (Timed a))
mergeClientDeletedConflicts func = M.partition $ \(Timed si _) ->
  case func si of
    TakeRemoteChange -> True
    StayDeleted -> False

-- | Resolve server deleted conflicts
mergeServerDeletedConflicts ::
  (a -> ServerDeletedConflictResolution) ->
  -- | The conflicting items on the client side
  Map si (Timed a) ->
  -- | The result is a map of items that need to be deleted on the client.
  (Set si, Set si)
mergeServerDeletedConflicts func m = both M.keysSet $
  flip M.partition m $ \(Timed si _) -> case func si of
    KeepLocalChange -> False
    Delete -> True
  where
    both :: (a -> b) -> (a, a) -> (b, b)
    both f (a1, a2) = (f a1, f a2)

data Identifier ci si
  = OnlyServer !si
  | BothServerAndClient !si !ci
  deriving (Show, Eq, Ord, Generic)

instance (Validity ci, Validity si) => Validity (Identifier ci si)

instance (NFData ci, NFData si) => NFData (Identifier ci si)

data ServerSyncProcessor ci si a m = ServerSyncProcessor
  { -- | Read all items
    serverSyncProcessorRead :: !(m (Map si (Timed a))),
    -- | Add an item with 'initialServerTime', can fail.
    serverSyncProcessorAddItem :: !(ci -> a -> m (Maybe si)),
    -- | Update an item
    serverSyncProcessorChangeItem :: !(si -> ServerTime -> a -> m ()),
    -- | Delete an item
    serverSyncProcessorDeleteItem :: !(si -> m ())
  }
  deriving (Generic)

-- | Process a server sync
--
-- === __Implementation Details__
--
-- There are four cases for the items in the sync request
--
-- - Added (A)
-- - Synced (S)
-- - Changed (C)
-- - Deleted (D)
--
-- Each of them present options and may require action on the sever side:
--
-- * Added:
--
--     * Client Added (CA) (This is the only case where a new identifier needs to be generated.)
--
-- * Synced:
--
--     * Server Changed (SC) (Nothing)
--     * Server Deleted (SD) (Nothing)
--
-- * Changed:
--
--     * Client Changed (CC) (Update value and increment server time)
--     * Change Conflict (CConf) (Nothing)
--     * Server Deleted Conflict (SDC) (Nothing)
--
-- * Deleted:
--
--     * Client Deleted (CD) (Delete the item)
--     * Client Deleted Conflict (CDC) (Nothing)
--
-- * Extra:
--
--     * Server Added (SA) (Nothing)
--
-- For more detailed comments of the nine cases, see the source of 'processServerItemSync' in the "Data.Mergeful.Item".
processServerSyncCustom ::
  forall ci si a m.
  ( Ord si,
    Monad m
  ) =>
  -- | Your server sync processor
  ServerSyncProcessor ci si a m ->
  SyncRequest ci si a ->
  m (SyncResponse ci si a)
processServerSyncCustom ServerSyncProcessor {..} SyncRequest {..} = do
  serverItems <- serverSyncProcessorRead
  -- A: CA (generate a new identifier)
  syncResponseClientAdded <- fmap (M.mapMaybe id) $
    flip M.traverseWithKey syncRequestNewItems $ \cid a -> do
      msi <- serverSyncProcessorAddItem cid a
      pure $ (\si -> ClientAddition {clientAdditionId = si, clientAdditionServerTime = initialServerTime}) <$> (msi :: Maybe si)
  -- C:
  let decideOnSynced tup@(sc, sd) (si, ct) =
        case M.lookup si serverItems of
          -- SD: The server must have deleted it.
          Nothing -> (sc, S.insert si sd)
          Just t@(Timed _ st) ->
            if ct >= st
              then tup -- In sync
              else (M.insert si t sc, sd) -- SC: The server has changed it because its server time is newer
  let (syncResponseServerChanged, syncResponseServerDeleted) = foldl decideOnSynced (M.empty, S.empty) (M.toList syncRequestKnownItems)
  -- S:
  let decideOnChanged (cc, cConf, sdc) (si, Timed clientItem ct) = do
        case M.lookup si serverItems of
          -- SDC
          Nothing -> pure (cc, cConf, S.insert si sdc)
          Just serverTimed@(Timed _ st) ->
            if ct >= st
              then do
                -- CC
                let st' = incrementServerTime st
                -- Update the server item
                serverSyncProcessorChangeItem si st' clientItem
                pure (M.insert si st' cc, cConf, sdc)
              else do
                -- CConf
                pure (cc, M.insert si serverTimed cConf, sdc)
  (syncResponseClientChanged, syncResponseConflicts, syncResponseConflictsServerDeleted) <- foldM decideOnChanged (M.empty, M.empty, S.empty) (M.toList syncRequestKnownButChangedItems)
  --- D:
  let decideOnDeleted (cd, cdc) (si, ct) = do
        case M.lookup si serverItems of
          Nothing -> do
            -- CD: It was already deleted on the server side, Just pretend that the client made that happen.
            pure (S.insert si cd, cdc)
          Just serverTimed@(Timed _ st) ->
            if ct >= st
              then do
                -- CD
                -- Delete the item
                serverSyncProcessorDeleteItem si
                pure (S.insert si cd, cdc)
              else do
                -- CDC
                pure (cd, M.insert si serverTimed cdc)
  (syncResponseClientDeleted, syncResponseConflictsClientDeleted) <- foldM decideOnDeleted (S.empty, M.empty) (M.toList syncRequestDeletedItems)
  -- Extra: for all items that are in the server but not in the sync request, we need to say they are server added.
  let syncResponseServerAdded = serverItems `M.difference` M.unions [() <$ syncRequestKnownItems, () <$ syncRequestKnownButChangedItems, () <$ syncRequestDeletedItems]
  pure SyncResponse {..}

-- | Serve an 'SyncRequest' using the current 'ServerStore', producing an 'SyncResponse' and a new 'ServerStore'.
processServerSync ::
  forall ci si a m.
  ( Ord si,
    Monad m
  ) =>
  -- | The action that is guaranteed to generate unique identifiers
  m si ->
  ServerStore si a ->
  SyncRequest ci si a ->
  m (SyncResponse ci si a, ServerStore si a)
processServerSync genId ss sr = runStateT (processServerSyncCustom (pureServerSyncProcessor genId) sr) ss

-- | A potentially pure sync processor
pureServerSyncProcessor ::
  (Ord si, Monad m) =>
  -- | The action that is guaranteed to generate unique identifiers
  m si ->
  ServerSyncProcessor ci si a (StateT (ServerStore si a) m)
pureServerSyncProcessor genId = ServerSyncProcessor {..}
  where
    serverSyncProcessorRead = gets serverStoreItems
    serverSyncProcessorAddItem _ a = do
      i <- lift genId
      modify (\(ServerStore m) -> ServerStore (M.insert i (Timed a initialServerTime) m))
      pure (Just i) -- Always succeed
    serverSyncProcessorChangeItem si st a =
      modify
        ( \(ServerStore m) ->
            let m' = M.adjust (const (Timed a st)) si m
             in ServerStore m'
        )
    serverSyncProcessorDeleteItem si =
      modify
        ( \(ServerStore m) ->
            let m' = M.delete si m
             in ServerStore m'
        )

distinct :: Eq a => [a] -> Bool
distinct ls = nub ls == ls
