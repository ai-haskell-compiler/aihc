{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A way to synchronise a single value with safe merge conflicts.
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
-- == For the first sychronisation
--
-- The client should ask the server for the current server value.
-- The server should send over a 'Timed' vaule, and the client should create its 'ClientValue' with 'initialClientValue'.
--
-- == For any following synchronisation:
--
--   * The client produces a 'ValueSyncRequest' with 'makeValueSyncRequest'.
--   * The client sends that request to the central server and gets a 'ValueSyncResponse'.
--   * The client then updates its local store with 'mergeValueSyncResponseRaw' or 'mergeValueSyncResponseIgnoreProblems'.
--
--
-- = The central server should operate as follows:
--
-- * The server should create an initial 'ServerValue' using 'initialServerValue'.
-- * The server accepts a 'ValueSyncRequest'.
-- * The server performs operations according to the functionality of 'processServerValueSync'.
-- * The server respons with a 'ValueSyncResponse'.
--
--
-- WARNING:
-- This whole approach can break down if a server resets its server times
-- or if a client syncs with two different servers using the same server times.
module Data.Mergeful.Value
  ( initialClientValue,
    makeValueSyncRequest,
    mergeValueSyncResponseRaw,
    ValueMergeResult (..),
    mergeValueSyncResponseIgnoreProblems,
    mergeIgnoringProblems,
    mergeFromServer,
    mergeUsingFunction,

    -- * Server side
    initialServerValue,
    processServerValueSync,

    -- * Types, for reference
    ChangedFlag (..),
    ClientValue (..),
    ValueSyncRequest (..),
    ValueSyncResponse (..),
    ServerValue (..),
  )
where

import Autodocodec
import Control.DeepSeq
import Data.Aeson (FromJSON, ToJSON)
import Data.Mergeful.Timed
import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)

data ChangedFlag
  = Changed
  | NotChanged
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ChangedFlag)

instance Validity ChangedFlag

instance NFData ChangedFlag

instance HasCodec ChangedFlag where
  codec = dimapCodec f g codec
    where
      f = \case
        True -> Changed
        False -> NotChanged
      g = \case
        Changed -> True
        NotChanged -> False

-- | The client side value.
--
-- The only differences between `a` and 'ClientValue a' are that
-- 'ClientValue a' also remembers the last synchronisation time from
-- the server, and whether the item has been modified at the client
--
-- There cannot be an unsynced 'ClientValue'.
data ClientValue a = ClientValue
  { clientValueTimedValue :: !(Timed a),
    clientValueChanged :: !ChangedFlag
  }
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ClientValue a))

instance Validity a => Validity (ClientValue a)

instance NFData a => NFData (ClientValue a)

instance HasCodec a => HasCodec (ClientValue a) where
  codec =
    object "ClientValue" $
      ClientValue
        <$> timedObjectCodec .= clientValueTimedValue
        <*> requiredField "changed" "whether the value has changed, client-side" .= clientValueChanged

-- | Produce a client value based on an initial synchronisation request
initialClientValue :: Timed a -> ClientValue a
initialClientValue t = ClientValue t NotChanged

-- | The server-side value.
--
-- The only difference between 'a' and 'ServerValue a' is that 'ServerValue a' also
-- remembers the last time this value was changed during synchronisation.
newtype ServerValue a = ServerValue {unServerValue :: Timed a}
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ServerValue a))

instance Validity a => Validity (ServerValue a)

instance NFData a => NFData (ServerValue a)

instance HasCodec a => HasCodec (ServerValue a) where
  codec = object "ServerValue" $ ServerValue <$> timedObjectCodec .= unServerValue

-- | Initialise a server value.
--
-- Note that the server has to start with a value, the value 'a' cannot be omitted.
initialServerValue :: a -> ServerValue a
initialServerValue a = ServerValue $ Timed {timedValue = a, timedTime = initialServerTime}

data ValueSyncRequest a
  = -- | There is an item locally that was synced at the given 'ServerTime'
    ValueSyncRequestKnown !ServerTime
  | -- | There is an item locally that was synced at the given 'ServerTime'
    -- but it has been changed since then.
    ValueSyncRequestKnownButChanged !(Timed a)
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ValueSyncRequest a))

instance Validity a => Validity (ValueSyncRequest a)

instance NFData a => NFData (ValueSyncRequest a)

instance HasCodec a => HasCodec (ValueSyncRequest a) where
  codec =
    object "ValueSyncRequest" $
      dimapCodec f g $
        disjointEitherCodec
          (typeField "synced" <*> requiredField "time" "time at which the server said the value was last synced")
          (typeField "changed" <*> timedObjectCodec)
    where
      f = \case
        Left st -> ValueSyncRequestKnown st
        Right tv -> ValueSyncRequestKnownButChanged tv
      g = \case
        ValueSyncRequestKnown st -> Left st
        ValueSyncRequestKnownButChanged tv -> Right tv

      typeField :: Text -> ObjectCodec b (a -> a)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

data ValueSyncResponse a
  = -- | The client and server are fully in sync.
    --
    -- Nothing needs to be done at the client side.
    ValueSyncResponseInSync
  | -- | The client changed the value and server has succesfully been made aware of that.
    --
    -- The client needs to update its server time
    ValueSyncResponseClientChanged !ServerTime
  | -- | This value has been changed on the server side.
    --
    -- The client should change it too.
    ValueSyncResponseServerChanged !(Timed a)
  | -- | The item at the server side
    ValueSyncResponseConflict !(Timed a)
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec (ValueSyncResponse a))

instance Validity a => Validity (ValueSyncResponse a)

instance NFData a => NFData (ValueSyncResponse a)

instance HasCodec a => HasCodec (ValueSyncResponse a) where
  codec =
    object "ValueSyncResponse" $
      dimapCodec f g $
        disjointEitherCodec
          ( disjointEitherCodec
              (typeField "in-sync" <*> pure ())
              (typeField "client-changed" <*> requiredField "time" "server time")
          )
          ( disjointEitherCodec
              (typeField "server-changed" <*> timedObjectCodec)
              (typeField "conflict" <*> timedObjectCodec)
          )
    where
      f = \case
        Left (Left ()) -> ValueSyncResponseInSync
        Left (Right st) -> ValueSyncResponseClientChanged st
        Right (Left tv) -> ValueSyncResponseServerChanged tv
        Right (Right tv) -> ValueSyncResponseConflict tv
      g = \case
        ValueSyncResponseInSync -> Left (Left ())
        ValueSyncResponseClientChanged st -> Left (Right st)
        ValueSyncResponseServerChanged tv -> Right (Left tv)
        ValueSyncResponseConflict tv -> Right (Right tv)

      typeField :: Text -> ObjectCodec b (a -> a)
      typeField typeName = id <$ requiredFieldWith' "type" (literalTextCodec typeName) .= const typeName

-- | Produce an 'ItemSyncRequest' from a 'ClientItem'.
--
-- Send this to the server for synchronisation.
makeValueSyncRequest :: ClientValue a -> ValueSyncRequest a
makeValueSyncRequest (ClientValue t cf) =
  case cf of
    NotChanged -> ValueSyncRequestKnown (timedTime t)
    Changed -> ValueSyncRequestKnownButChanged t

data ValueMergeResult a
  = -- | The merger went succesfully, no conflicts or desyncs
    MergeSuccess !(ClientValue a)
  | -- | The item at the server side
    MergeConflict !a !(Timed a)
  | -- | The server responded with a response that did not make sense given the client's request.
    --
    -- This should not happen in practice.
    MergeMismatch
  deriving (Show, Eq, Generic)

instance Validity a => Validity (ValueMergeResult a)

instance NFData a => NFData (ValueMergeResult a)

-- | Merge an 'ValueSyncResponse' into the current 'ClientValue'.
--
-- This function will not make any decisions about what to do with
-- conflicts or mismatches between the request and the response.
-- It only produces a 'ValueMergeResult' so you can decide what to do with it.
mergeValueSyncResponseRaw :: ClientValue a -> ValueSyncResponse a -> ValueMergeResult a
mergeValueSyncResponseRaw cv@(ClientValue ct cf) sr =
  case cf of
    NotChanged ->
      case sr of
        ValueSyncResponseInSync -> MergeSuccess cv
        ValueSyncResponseServerChanged st -> MergeSuccess $ ClientValue st NotChanged
        _ -> MergeMismatch
    Changed ->
      case sr of
        ValueSyncResponseClientChanged st ->
          MergeSuccess $ ClientValue (ct {timedTime = st}) NotChanged
        ValueSyncResponseConflict si -> MergeConflict (timedValue ct) si
        _ -> MergeMismatch

-- | Resolve a 'ValueSyncResponse' into the current 'ClientValue'.
--
-- This function ignores any problems that may occur.
-- In the case of a conclict, it will just not update the client item.
-- The next sync request will then produce a conflict again.
--
-- > mergeValueSyncResponseIgnoreProblems cs = mergeIgnoringProblems cs . mergeValueSyncResponseRaw cs
mergeValueSyncResponseIgnoreProblems :: ClientValue a -> ValueSyncResponse a -> ClientValue a
mergeValueSyncResponseIgnoreProblems cs = mergeIgnoringProblems cs . mergeValueSyncResponseRaw cs

-- | Ignore any merge problems in a 'ValueMergeResult'.
--
-- This function just returns the original 'ClientValue' if anything other than 'MergeSuccess' occurs.
--
-- This function ignores any problems that may occur.
-- In the case of a conclict, it will just not update the client item.
-- The next sync request will then produce a conflict again.
--
-- Pro: does not lose data
--
-- __Con: Clients will diverge when a conflict occurs__
mergeIgnoringProblems :: ClientValue a -> ValueMergeResult a -> ClientValue a
mergeIgnoringProblems cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict _ _ -> cs
    MergeMismatch -> cs

-- | Resolve a 'ValueMergeResult' using a given merge strategy.
--
-- This function ignores 'MergeMismatch' and will just return the original 'ClientValue' in that case.
--
-- In order for clients to converge on the same value correctly, this function must be:
--
-- * Associative
-- * Idempotent
-- * The same on all clients
mergeUsingFunction ::
  (a -> Timed a -> Timed a) -> ClientValue a -> ValueMergeResult a -> ClientValue a
mergeUsingFunction func cs mr =
  case mr of
    MergeSuccess cs' -> cs'
    MergeConflict a1 a2 -> ClientValue (func a1 a2) NotChanged
    MergeMismatch -> cs

-- | Resolve a 'ValueMergeResult' by taking whatever the server gave the client.
--
-- Pro: Clients will converge on the same value.
--
-- __Con: Conflicting updates will be lost.__
mergeFromServer :: ClientValue a -> ValueMergeResult a -> ClientValue a
mergeFromServer = mergeUsingFunction (\_ serverItem -> serverItem)

-- | Serve an 'ValueSyncRequest' using the current 'ServerValue', producing an 'ValueSyncResponse' and a new 'ServerValue'.
processServerValueSync ::
  ServerValue a -> ValueSyncRequest a -> (ValueSyncResponse a, ServerValue a)
processServerValueSync sv@(ServerValue t@(Timed _ st)) sr =
  case sr of
    ValueSyncRequestKnown ct ->
      if ct >= st
        then -- The client time is equal to the server time.
        -- The client indicates that the item was not modified at their side.
        -- This means that the items are in sync.
        -- (Unless the server somehow modified the item but not its server time,
        -- which would beconsidered a bug.)
          (ValueSyncResponseInSync, sv)
        else -- The client time is less than the server time
        -- That means that the server has synced with another client in the meantime.
        -- Since the client indicates that the item was not modified at their side,
        -- we can just send it back to the client to have them update their version.
        -- No conflict here.
          (ValueSyncResponseServerChanged t, sv)
    ValueSyncRequestKnownButChanged Timed {timedValue = ci, timedTime = ct} ->
      if ct >= st
        then -- The client time is equal to the server time.
        -- The client indicates that the item *was* modified at their side.
        -- This means that the server needs to be updated.

          let st' = incrementServerTime st
           in ( ValueSyncResponseClientChanged st',
                ServerValue (Timed {timedValue = ci, timedTime = st'})
              )
        else -- The client time is less than the server time
        -- That means that the server has synced with another client in the meantime.
        -- Since the client indicates that the item *was* modified at their side,
        -- there is a conflict.
          (ValueSyncResponseConflict t, sv)
