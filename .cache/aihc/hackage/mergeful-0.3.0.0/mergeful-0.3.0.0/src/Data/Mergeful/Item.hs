{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | A way to synchronise an item with safe merge conflicts.
--
-- The item is "zero or one" value.
-- One could say that @Item a = Maybe a@ but there are so such types here.
-- This methaphor just serves as explanation
--
--
-- The setup is as follows:
--
-- * A central server is set up to synchronise with
-- * Each client synchronises with the central server, but never with eachother
--
--
-- = A client should operate as follows:
--
-- The clients starts with an 'initialClientItem'.
--
-- * The client produces a 'ItemSyncRequest' with 'makeItemSyncRequest'.
-- * The client sends that request to the central server and gets a 'ItemSyncResponse'.
-- * The client then updates its local store with 'mergeItemSyncResponseRaw' or 'mergeItemSyncResponseIgnoreProblems'.
--
--
-- = The central server should operate as follows:
--
-- The server starts with an 'initialServerItem'.
--
-- * The server accepts a 'ItemSyncRequest'.
-- * The server performs operations according to the functionality of 'processServerItemSync'.
-- * The server respons with a 'ItemSyncResponse'.
--
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful.Item
  ( initialClientItem,
    initialItemSyncRequest,
    makeItemSyncRequest,
    mergeItemSyncResponseFromServer,
    mergeItemSyncResponseFromClient,
    mergeItemSyncResponseUsingCRDT,
    mergeItemSyncResponseUsingStrategy,
    mergeFromServer,
    mergeFromServerStrategy,
    mergeFromClient,
    mergeFromClientStrategy,
    mergeUsingCRDT,
    mergeUsingCRDTStrategy,
    ItemMergeStrategy (..),
    ChangeConflictResolution (..),
    ClientDeletedConflictResolution (..),
    ServerDeletedConflictResolution (..),
    mergeUsingStrategy,
    mergeItemSyncResponseRaw,
    ItemMergeResult (..),

    -- * Server side
    initialServerItem,
    processServerItemSync,

    -- * Types, for reference
    ClientItem (..),
    ItemSyncRequest (..),
    ItemSyncResponse (..),
    ServerItem (..),
  )
where

import Autodocodec
import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import Data.Mergeful.Timed
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)

data ClientItem a
  = -- | There is no item on the client side
    ClientEmpty
  | -- | There is is an item but the server is not aware of it yet.
    ClientAdded !a
  | -- | There is is an item and it has been synced with the server.
    ClientItemSynced !(Timed a)
  | -- | There is is an item and it has been synced with the server, but it has since been modified.
    ClientItemSyncedButChanged !(Timed a)
  | -- | There was an item, and it has been deleted locally, but the server has not been made aware of this.
    ClientDeleted !ServerTime
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ClientItem a))

instance Validity a => Validity (ClientItem a)

instance NFData a => NFData (ClientItem a)

instance HasCodec a => HasCodec (ClientItem a) where
  codec =
    object "ClientItem" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              (typeField "empty" <*> pure ())
              (typeField "added" <*> requiredField "value" "item that was added, client-side")
          )
          ( disjointEitherCodec
              (typeField "synced" <*> timedObjectCodec)
              ( disjointEitherCodec
                  (typeField "changed" <*> timedObjectCodec)
                  (typeField "deleted" <*> requiredField "time" "last time the server confirmed a change, from the client's perspective")
              )
          )
    where
      f = \case
        Left (Left ()) -> ClientEmpty
        Left (Right v) -> ClientAdded v
        Right (Left tv) -> ClientItemSynced tv
        Right (Right (Left tv)) -> ClientItemSyncedButChanged tv
        Right (Right (Right st)) -> ClientDeleted st
      g = \case
        ClientEmpty -> Left (Left ())
        ClientAdded v -> Left (Right v)
        ClientItemSynced tv -> Right (Left tv)
        ClientItemSyncedButChanged tv -> Right (Right (Left tv))
        ClientDeleted st -> Right (Right (Right st))

      typeField :: Text -> ObjectCodec b (a -> a)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

-- | A client item to start with.
--
-- It contains no value.
initialClientItem :: ClientItem a
initialClientItem = ClientEmpty

data ServerItem a
  = -- | There is no item on the server side
    ServerEmpty
  | -- | There is an item on the server side, and it was last synced at the given 'ServerTime'.
    ServerFull !(Timed a)
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ServerItem a))

instance Validity a => Validity (ServerItem a)

instance NFData a => NFData (ServerItem a)

instance HasCodec a => HasCodec (ServerItem a) where
  codec =
    object "ServerItem" $
      dimapCodec f g $ possiblyJointEitherCodec timedObjectCodec (pure ())
    where
      f = \case
        Left tv -> ServerFull tv
        Right () -> ServerEmpty
      g = \case
        ServerFull tv -> Left tv
        ServerEmpty -> Right ()

-- | A server item to start with.
--
-- It contains no value.
initialServerItem :: ServerItem a
initialServerItem = ServerEmpty

data ItemSyncRequest a
  = -- | There is no item locally
    ItemSyncRequestPoll
  | -- | There is an item locally that has not been synced to the server yet.
    ItemSyncRequestNew !a
  | -- | There is an item locally that was synced at the given 'ServerTime'
    ItemSyncRequestKnown !ServerTime
  | -- | There is an item locally that was synced at the given 'ServerTime'
    -- but it has been changed since then.
    ItemSyncRequestKnownButChanged !(Timed a)
  | -- | There was an item locally that has been deleted but the
    -- deletion wasn't synced to the server yet.
    ItemSyncRequestDeletedLocally !ServerTime
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemSyncRequest a))

instance Validity a => Validity (ItemSyncRequest a)

instance NFData a => NFData (ItemSyncRequest a)

instance HasCodec a => HasCodec (ItemSyncRequest a) where
  codec =
    object "ItemSyncRequest" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              (typeField "empty" <*> pure ())
              (typeField "added" <*> requiredField "value" "item that was added, client-side")
          )
          ( disjointEitherCodec
              (typeField "synced" <*> requiredField "time" "last time the server confirmed a change, from the client's perspective")
              ( disjointEitherCodec
                  (typeField "changed" <*> timedObjectCodec)
                  (typeField "deleted" <*> requiredField "time" "last time the server confirmed a change, from the client's perspective")
              )
          )
    where
      f = \case
        Left (Left ()) -> ItemSyncRequestPoll
        Left (Right v) -> ItemSyncRequestNew v
        Right (Left tv) -> ItemSyncRequestKnown tv
        Right (Right (Left tv)) -> ItemSyncRequestKnownButChanged tv
        Right (Right (Right st)) -> ItemSyncRequestDeletedLocally st
      g = \case
        ItemSyncRequestPoll -> Left (Left ())
        ItemSyncRequestNew v -> Left (Right v)
        ItemSyncRequestKnown tv -> Right (Left tv)
        ItemSyncRequestKnownButChanged tv -> Right (Right (Left tv))
        ItemSyncRequestDeletedLocally st -> Right (Right (Right st))

      typeField :: Text -> ObjectCodec b (a -> a)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

-- | An intial 'ItemSyncRequest' to start with.
--
-- It just asks the server to send over whatever it knows.
initialItemSyncRequest :: ItemSyncRequest a
initialItemSyncRequest = ItemSyncRequestPoll

data ItemSyncResponse a
  = -- | The client and server are fully in sync, and both empty
    --
    -- Nothing needs to be done at the client side.
    ItemSyncResponseInSyncEmpty
  | -- | The client and server are fully in sync.
    --
    -- Nothing needs to be done at the client side.
    ItemSyncResponseInSyncFull
  | -- | The client added an item and server has succesfully been made aware of that.
    --
    -- The client needs to update its server time
    ItemSyncResponseClientAdded !ServerTime
  | -- | The client changed an item and server has succesfully been made aware of that.
    --
    -- The client needs to update its server time
    ItemSyncResponseClientChanged !ServerTime
  | -- | The client deleted an item and server has succesfully been made aware of that.
    --
    -- The client can delete it from its deleted items
    ItemSyncResponseClientDeleted
  | -- | This item has been added on the server side
    --
    -- The client should add it too.
    ItemSyncResponseServerAdded !(Timed a)
  | -- | This item has been modified on the server side.
    --
    -- The client should modify it too.
    ItemSyncResponseServerChanged !(Timed a)
  | -- | The item was deleted on the server side
    --
    -- The client should delete it too.
    ItemSyncResponseServerDeleted
  | -- | The item at the server side
    -- | A conflict occurred.
    --
    -- The server has an item but the client does not.
    -- The server kept its part, the client can either take whatever the server gave them
    -- or deal with the conflict somehow, and then try to re-sync.
    ItemSyncResponseConflict !(Timed a)
  | -- | The item at the server side
    -- | A conflict occurred.
    --
    -- The client has a (modified) item but the server does not have any item.
    -- The server left its item deleted, the client can either delete its item too
    -- or deal with the conflict somehow, and then try to re-sync.
    ItemSyncResponseConflictClientDeleted !(Timed a)
  | ItemSyncResponseConflictServerDeleted
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ItemSyncResponse a))

instance Validity a => Validity (ItemSyncResponse a)

instance NFData a => NFData (ItemSyncResponse a)

instance HasCodec a => HasCodec (ItemSyncResponse a) where
  codec =
    object "ItemSyncResponse" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              ( disjointEitherCodec
                  (typeField "in-sync-empty" <*> pure ())
                  (typeField "in-sync-full" <*> pure ())
              )
              ( disjointEitherCodec
                  (typeField "client-added" <*> requiredField "time" "server's confirmation of the addition")
                  (typeField "client-changed" <*> requiredField "time" "server's confirmation of the addition")
              )
          )
          ( disjointEitherCodec
              ( disjointEitherCodec
                  ( disjointEitherCodec
                      (typeField "client-deleted" <*> pure ())
                      (typeField "server-added" <*> timedObjectCodec)
                  )
                  ( disjointEitherCodec
                      (typeField "server-changed" <*> timedObjectCodec)
                      (typeField "server-deleted" <*> pure ())
                  )
              )
              ( disjointEitherCodec
                  ( disjointEitherCodec
                      (typeField "conflict" <*> timedObjectCodec)
                      (typeField "conflict-client-deleted" <*> timedObjectCodec)
                  )
                  (typeField "conflict-server-deleted" <*> pure ())
              )
          )
    where
      f = \case
        Left (Left (Left ())) -> ItemSyncResponseInSyncEmpty
        Left (Left (Right ())) -> ItemSyncResponseInSyncFull
        Left (Right (Left st)) -> ItemSyncResponseClientAdded st
        Left (Right (Right st)) -> ItemSyncResponseClientChanged st
        Right (Left (Left (Left ()))) -> ItemSyncResponseClientDeleted
        Right (Left (Left (Right tv))) -> ItemSyncResponseServerAdded tv
        Right (Left (Right (Left tv))) -> ItemSyncResponseServerChanged tv
        Right (Left (Right (Right ()))) -> ItemSyncResponseServerDeleted
        Right (Right (Left (Left tv))) -> ItemSyncResponseConflict tv
        Right (Right (Left (Right tv))) -> ItemSyncResponseConflictClientDeleted tv
        Right (Right (Right ())) -> ItemSyncResponseConflictServerDeleted
      g = \case
        ItemSyncResponseInSyncEmpty -> Left (Left (Left ()))
        ItemSyncResponseInSyncFull -> Left (Left (Right ()))
        ItemSyncResponseClientAdded st -> Left (Right (Left st))
        ItemSyncResponseClientChanged st -> Left (Right (Right st))
        ItemSyncResponseClientDeleted -> Right (Left (Left (Left ())))
        ItemSyncResponseServerAdded tv -> Right (Left (Left (Right tv)))
        ItemSyncResponseServerChanged tv -> Right (Left (Right (Left tv)))
        ItemSyncResponseServerDeleted -> Right (Left (Right (Right ())))
        ItemSyncResponseConflict tv -> Right (Right (Left (Left tv)))
        ItemSyncResponseConflictClientDeleted tv -> Right (Right (Left (Right tv)))
        ItemSyncResponseConflictServerDeleted -> Right (Right (Right ()))

      typeField :: Text -> ObjectCodec b (a -> a)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

-- | Produce an 'ItemSyncRequest' from a 'ClientItem'.
--
-- Send this to the server for synchronisation.
makeItemSyncRequest :: ClientItem a -> ItemSyncRequest a
makeItemSyncRequest cs =
  case cs of
    ClientEmpty -> ItemSyncRequestPoll
    ClientAdded i -> ItemSyncRequestNew i
    ClientItemSynced t -> ItemSyncRequestKnown (timedTime t)
    ClientItemSyncedButChanged t -> ItemSyncRequestKnownButChanged t
    ClientDeleted st -> ItemSyncRequestDeletedLocally st

data ItemMergeResult a
  = -- | The merger went succesfully, no conflicts or desyncs
    MergeSuccess !(ClientItem a)
  | -- | The item at the server side
    -- | There was a merge conflict. The client had deleted the item while the server had modified it.
    MergeConflict !a !(Timed a)
  | -- | The item at the server side
    -- | There was a merge conflict. The server had deleted the item while the client had modified it.
    MergeConflictClientDeleted !(Timed a)
  | -- | The item at the client side
    -- | The server responded with a response that did not make sense given the client's request.
    MergeConflictServerDeleted !a
  | MergeMismatch
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ItemMergeResult a)

instance NFData a => NFData (ItemMergeResult a)

-- | Merge an 'ItemSyncResponse' into the current 'ClientItem'.
--
-- This function will not make any decisions about what to do with
-- conflicts or mismatches between the request and the response.
-- It only produces a 'ItemMergeResult' so you can decide what to do with it.
mergeItemSyncResponseRaw :: ClientItem a -> ItemSyncResponse a -> ItemMergeResult a
mergeItemSyncResponseRaw cs sr =
  case cs of
    ClientEmpty ->
      case sr of
        ItemSyncResponseInSyncEmpty -> MergeSuccess cs
        ItemSyncResponseServerAdded t -> MergeSuccess $ ClientItemSynced t
        _ -> MergeMismatch
    ClientAdded ci ->
      case sr of
        ItemSyncResponseClientAdded st ->
          MergeSuccess $ ClientItemSynced $ Timed {timedValue = ci, timedTime = st}
        ItemSyncResponseConflict si -> MergeConflict ci si
        _ -> MergeMismatch
    ClientItemSynced t ->
      case sr of
        ItemSyncResponseInSyncFull -> MergeSuccess $ ClientItemSynced t
        ItemSyncResponseServerChanged st -> MergeSuccess $ ClientItemSynced st
        ItemSyncResponseServerDeleted -> MergeSuccess ClientEmpty
        _ -> MergeMismatch
    ClientItemSyncedButChanged ct ->
      case sr of
        ItemSyncResponseClientChanged st -> MergeSuccess $ ClientItemSynced $ ct {timedTime = st}
        ItemSyncResponseConflict si -> MergeConflict (timedValue ct) si
        ItemSyncResponseConflictServerDeleted -> MergeConflictServerDeleted (timedValue ct)
        _ -> MergeMismatch
    ClientDeleted _ ->
      case sr of
        ItemSyncResponseClientDeleted -> MergeSuccess ClientEmpty
        ItemSyncResponseConflictClientDeleted si -> MergeConflictClientDeleted si
        _ -> MergeMismatch

mergeItemSyncResponseUsingStrategy :: ItemMergeStrategy a -> ClientItem a -> ItemSyncResponse a -> ClientItem a
mergeItemSyncResponseUsingStrategy strat ci sr = mergeUsingStrategy strat ci $ mergeItemSyncResponseRaw ci sr

mergeItemSyncResponseFromServer :: ClientItem a -> ItemSyncResponse a -> ClientItem a
mergeItemSyncResponseFromServer = mergeItemSyncResponseUsingStrategy mergeFromServerStrategy

mergeItemSyncResponseFromClient :: ClientItem a -> ItemSyncResponse a -> ClientItem a
mergeItemSyncResponseFromClient = mergeItemSyncResponseUsingStrategy mergeFromClientStrategy

mergeItemSyncResponseUsingCRDT :: (a -> a -> a) -> ClientItem a -> ItemSyncResponse a -> ClientItem a
mergeItemSyncResponseUsingCRDT = mergeItemSyncResponseUsingStrategy . mergeUsingCRDTStrategy

-- | A strategy to merge conflicts for item synchronisation
data ItemMergeStrategy a = ItemMergeStrategy
  { -- | How to merge modification conflicts
    --
    -- The first argument is the client item and the second argument is the server item.
    itemMergeStrategyMergeChangeConflict :: !(a -> a -> ChangeConflictResolution a),
    -- | How to merge conflicts where the client deleted an item that the server modified
    itemMergeStrategyMergeClientDeletedConflict :: !(a -> ClientDeletedConflictResolution),
    -- | How to merge conflicts where the server deleted an item that the client modified
    itemMergeStrategyMergeServerDeletedConflict :: !(a -> ServerDeletedConflictResolution)
  }
  deriving (Generic)

data ChangeConflictResolution a
  = KeepLocal
  | TakeRemote
  | Merged a
  deriving (Show, Eq, Generic)

data ClientDeletedConflictResolution
  = TakeRemoteChange
  | StayDeleted
  deriving (Show, Eq, Generic)

data ServerDeletedConflictResolution
  = KeepLocalChange
  | Delete
  deriving (Show, Eq, Generic)

-- | Resolve an 'ItemMergeResult' using a given merge strategy.
--
-- This function ignores 'MergeMismatch' and will just return the original 'ClientItem' in that case.
--
-- In order for clients to converge on the same item correctly, this function must be:
--
-- * Associative
-- * Idempotent
-- * The same on all clients
mergeUsingStrategy :: ItemMergeStrategy a -> ClientItem a -> ItemMergeResult a -> ClientItem a
mergeUsingStrategy ItemMergeStrategy {..} ci mr =
  case mr of
    MergeSuccess ci' -> ci'
    MergeConflict a1 ri -> mergeChangeConflict itemMergeStrategyMergeChangeConflict ci a1 ri
    MergeConflictClientDeleted ri -> mergeClientDeletedConflict itemMergeStrategyMergeClientDeletedConflict ci ri
    MergeConflictServerDeleted ca -> mergeServerDeletedConflict itemMergeStrategyMergeServerDeletedConflict ci ca
    MergeMismatch -> ci

mergeChangeConflict :: (a -> a -> ChangeConflictResolution a) -> ClientItem a -> a -> Timed a -> ClientItem a
mergeChangeConflict func ci a1 ri@(Timed a2 st) = case func a1 a2 of
  KeepLocal -> ci
  TakeRemote -> ClientItemSynced ri
  Merged ma -> ClientItemSynced $ Timed ma st

mergeClientDeletedConflict :: (a -> ClientDeletedConflictResolution) -> ClientItem a -> Timed a -> ClientItem a
mergeClientDeletedConflict func ci ri@(Timed sa _) = case func sa of
  TakeRemoteChange -> ClientItemSynced ri
  StayDeleted -> ci -- We can't just use 'ClientEmpty' here because otherwise the 'mergeUsingStrategy' wouldn't be idempotent anymore.

mergeServerDeletedConflict :: (a -> ServerDeletedConflictResolution) -> ClientItem a -> a -> ClientItem a
mergeServerDeletedConflict func ci ca = case func ca of
  KeepLocalChange -> ci -- We can't just use 'ClientAdded ca' here because otherwise the 'mergeUsingStrategy' wouldn't be idempotent anymore.
  Delete -> ClientEmpty

-- | Resolve an 'ItemMergeResult' by taking whatever the server gave the client.
--
-- Pro: Clients will converge on the same value.
--
-- __Con: Conflicting updates will be lost.__
mergeFromServer :: ClientItem a -> ItemMergeResult a -> ClientItem a
mergeFromServer =
  mergeUsingStrategy mergeFromServerStrategy

-- | A merge strategy that takes whatever the server gave the client.
--
-- Pro: Clients will converge on the same value.
--
-- __Con: Conflicting updates will be lost.__
mergeFromServerStrategy :: ItemMergeStrategy a
mergeFromServerStrategy =
  ItemMergeStrategy
    { itemMergeStrategyMergeChangeConflict = \_ _ -> TakeRemote,
      itemMergeStrategyMergeClientDeletedConflict = \_ -> TakeRemoteChange,
      itemMergeStrategyMergeServerDeletedConflict = \_ -> Delete
    }

-- | Resolve an 'ItemMergeResult' by keeping whatever the client had.
--
-- Pro: does not lose data
--
-- __Con: Clients will diverge when a conflict occurs__
mergeFromClient :: ClientItem a -> ItemMergeResult a -> ClientItem a
mergeFromClient = mergeUsingStrategy mergeFromClientStrategy

-- | A merge strategy that keeps whatever the client had.
--
-- Pro: does not lose data
--
-- __Con: Clients will diverge when a conflict occurs__
mergeFromClientStrategy :: ItemMergeStrategy a
mergeFromClientStrategy =
  ItemMergeStrategy
    { itemMergeStrategyMergeChangeConflict = \_ _ -> KeepLocal,
      itemMergeStrategyMergeClientDeletedConflict = \_ -> StayDeleted,
      itemMergeStrategyMergeServerDeletedConflict = \_ -> KeepLocalChange
    }

mergeUsingCRDT :: (a -> a -> a) -> ClientItem a -> ItemMergeResult a -> ClientItem a
mergeUsingCRDT = mergeUsingStrategy . mergeUsingCRDTStrategy

-- | A merge strategy that uses a CRDT merging function to merge items.
--
-- In case of other-than-change conflicts, this will be the same as the 'mergeFromServerStrategy' strategy.
-- If this is not what you want, create your own 'ItemMergeStrategy' manually.
mergeUsingCRDTStrategy :: (a -> a -> a) -> ItemMergeStrategy a
mergeUsingCRDTStrategy merge =
  mergeFromServerStrategy
    { itemMergeStrategyMergeChangeConflict = \a1 a2 -> Merged (merge a1 a2)
    }

-- | Serve an 'ItemSyncRequest' using the current 'ServerItem', producing an 'ItemSyncResponse' and a new 'ServerItem'.
processServerItemSync :: ServerItem a -> ItemSyncRequest a -> (ItemSyncResponse a, ServerItem a)
processServerItemSync store sr =
  case store of
    ServerEmpty ->
      let t = initialServerTime
       in case sr of
            ItemSyncRequestPoll -> (ItemSyncResponseInSyncEmpty, store)
            ItemSyncRequestNew ci ->
              (ItemSyncResponseClientAdded t, ServerFull $ Timed {timedValue = ci, timedTime = t})
            ItemSyncRequestKnown _ ->
              -- This indicates that the server synced with another client and was told to
              -- delete its item.
              --
              -- Given that the client indicates that it did not change anything locally,
              -- the server will just instruct the client to delete its item too.
              -- No conflict here.
              (ItemSyncResponseServerDeleted, store)
            ItemSyncRequestKnownButChanged _ ->
              -- This indicates that the server synced with another client and was told to
              -- delete its item.
              --
              -- Given that the client indicates that it *did* change its item locally,
              -- there is a conflict.
              (ItemSyncResponseConflictServerDeleted, store)
            ItemSyncRequestDeletedLocally _ ->
              -- This means that the server synced with another client,
              -- was instructed to delete its item by that client,
              -- and is now being told to delete its item again.
              --
              -- That's fine, it will just remain deleted.
              -- No conflict here
              (ItemSyncResponseClientDeleted, store)
    ServerFull t@(Timed si st) ->
      let st' = incrementServerTime st
       in case sr of
            ItemSyncRequestPoll ->
              -- The client is empty but the server is not.
              -- This means that the server has synced with another client before,
              -- so we can just send the item to the client.
              (ItemSyncResponseServerAdded (Timed {timedValue = si, timedTime = st}), store)
            ItemSyncRequestNew _ ->
              -- The client has a newly added item, so it thought it was empty before that,
              -- but the server has already synced with another client before.
              -- This indicates a conflict.
              -- The server is always right, so the item at the server will remain unmodified.
              -- The client will receive the conflict.
              (ItemSyncResponseConflict t, store)
            ItemSyncRequestKnown ct ->
              if ct >= st
                then -- The client time is equal to the server time.
                -- The client indicates that the item was not modified at their side.
                -- This means that the items are in sync.
                -- (Unless the server somehow modified the item but not its server time,
                -- which would beconsidered a bug.)
                  (ItemSyncResponseInSyncFull, store)
                else -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item was not modified at their side,
                -- we can just send it back to the client to have them update their version.
                -- No conflict here.

                  ( ItemSyncResponseServerChanged (Timed {timedValue = si, timedTime = st}),
                    store
                  )
            ItemSyncRequestKnownButChanged Timed {timedValue = ci, timedTime = ct} ->
              if ct >= st
                then -- The client time is equal to the server time.
                -- The client indicates that the item *was* modified at their side.
                -- This means that the server needs to be updated.

                  ( ItemSyncResponseClientChanged st',
                    ServerFull (Timed {timedValue = ci, timedTime = st'})
                  )
                else -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item *was* modified at their side,
                -- there is a conflict.
                  (ItemSyncResponseConflict t, store)
            ItemSyncRequestDeletedLocally ct ->
              if ct >= st
                then -- The client time is equal to the server time.
                -- The client indicates that the item was deleted on their side.
                -- This means that the server item needs to be deleted as well.
                  (ItemSyncResponseClientDeleted, ServerEmpty)
                else -- The client time is less than the server time
                -- That means that the server has synced with another client in the meantime.
                -- Since the client indicates that the item was deleted at their side,
                -- there is a conflict.
                  (ItemSyncResponseConflictClientDeleted t, store)
