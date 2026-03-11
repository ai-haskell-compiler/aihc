{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtils.ClientDB where

import Data.Maybe
import Data.Mergeful
import Data.Mergeful.Persistent
import Data.Validity
import Data.Validity.Persist ()
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import TestUtils.ServerDB

share
  [mkPersist sqlSettings, mkMigrate "migrateClient"]
  [persistLowerCase|

ClientThing
  -- All the fields of 'Thing' go here.
  number Int
  serverId ServerThingId Maybe -- Nothing means it's not been synced
  serverTime ServerTime Maybe -- Nothing means it's not been synced
  deleted Bool -- True means this item has been tombstoned, and it must have been synced before.
  changed Bool -- True means it's been changed after it's been synced, it must have been synced before.

  ClientUniqueServerId serverId !force

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ClientThing where
  validate ct@ClientThing {..} =
    mconcat
      [ genericValidate ct,
        declare "The server id and time are either both Nothing or both Just" $ case (clientThingServerId, clientThingServerTime) of
          (Nothing, Nothing) -> True
          (Just _, Just _) -> True
          (Just _, Nothing) -> True
          (Nothing, Just _) -> True,
        declare "If it has been deleted, then it must have been synced" $
          if clientThingDeleted
            then isJust clientThingServerId
            else True,
        declare
          "If it has been changed, then it must have been synced before"
          $ if clientThingChanged
            then isJust clientThingServerId
            else True
      ]

setupUnsyncedClientQuery :: [Thing] -> SqlPersistT IO ()
setupUnsyncedClientQuery = mapM_ $ \Thing {..} ->
  insert_
    ClientThing
      { clientThingNumber = thingNumber,
        clientThingServerId = Nothing,
        clientThingServerTime = Nothing,
        clientThingDeleted = False,
        clientThingChanged = False
      }

setupClientThingQuery :: ClientStore ClientThingId ServerThingId Thing -> SqlPersistT IO ()
setupClientThingQuery = setupClientQuery makeUnsyncedClientThing makeSyncedClientThing makeSyncedButChangedClientThing makeDeletedClientThing

clientGetStoreThingQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId Thing)
clientGetStoreThingQuery =
  clientGetStoreQuery
    ClientThingServerId
    ClientThingServerTime
    ClientThingChanged
    ClientThingDeleted
    unmakeUnsyncedClientThing
    unmakeSyncedClientThing
    unmakeDeletedClientThing

clientMakeSyncRequestThingQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId Thing)
clientMakeSyncRequestThingQuery =
  clientMakeSyncRequestQuery
    ClientThingServerId
    ClientThingServerTime
    ClientThingChanged
    ClientThingDeleted
    unmakeUnsyncedClientThing
    unmakeSyncedClientThing
    unmakeDeletedClientThing

clientMergeSyncResponseThingQuery :: ItemMergeStrategy Thing -> SyncResponse ClientThingId ServerThingId Thing -> SqlPersistT IO ()
clientMergeSyncResponseThingQuery =
  clientMergeSyncResponseQuery
    ClientThingServerId
    ClientThingServerTime
    ClientThingChanged
    ClientThingDeleted
    makeSyncedClientThing
    unmakeSyncedClientThing
    clientThingRecordUpdates

unmakeUnsyncedClientThing :: ClientThing -> Thing
unmakeUnsyncedClientThing ClientThing {..} = Thing {thingNumber = clientThingNumber}

makeUnsyncedClientThing :: Thing -> ClientThing
makeUnsyncedClientThing Thing {..} =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingDeleted = False,
      clientThingServerId = Nothing,
      clientThingChanged = False,
      clientThingServerTime = Nothing
    }

unmakeSyncedClientThing :: ClientThing -> (ServerThingId, Timed Thing)
unmakeSyncedClientThing ClientThing {..} =
  ( fromJust clientThingServerId,
    Timed
      { timedValue = Thing {thingNumber = clientThingNumber},
        timedTime = fromJust clientThingServerTime
      }
  )

makeSyncedClientThing :: ServerThingId -> Timed Thing -> ClientThing
makeSyncedClientThing sid (Timed Thing {..} st) =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = False,
      clientThingDeleted = False
    }

makeSyncedButChangedClientThing :: ServerThingId -> Timed Thing -> ClientThing
makeSyncedButChangedClientThing sid (Timed Thing {..} st) =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = True,
      clientThingDeleted = False
    }

unmakeDeletedClientThing :: ClientThing -> (ServerThingId, ServerTime)
unmakeDeletedClientThing ClientThing {..} =
  (fromJust clientThingServerId, fromJust clientThingServerTime)

makeDeletedClientThing :: ServerThingId -> ServerTime -> ClientThing
makeDeletedClientThing sid st =
  ClientThing
    { clientThingNumber = 0, -- dummy
      clientThingServerId = Just sid,
      clientThingServerTime = Just st,
      clientThingChanged = False, -- dummy
      clientThingDeleted = True
    }

clientMakeThing :: ClientThing -> Thing
clientMakeThing ClientThing {..} = Thing {thingNumber = clientThingNumber}

clientThingRecordUpdates :: Thing -> [Update ClientThing]
clientThingRecordUpdates Thing {..} = [ClientThingNumber =. thingNumber]
