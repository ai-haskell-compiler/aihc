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

import Data.Appendful
import Data.Appendful.Persistent
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)
import TestUtils.ServerDB

share
  [mkPersist sqlSettings, mkMigrate "migrateClient"]
  [persistLowerCase|

ClientThing
  number Int
  serverId ServerThingId Maybe -- Nothing means it's not been synced

  ClientUniqueServerId serverId !force

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

setupUnsyncedClientThingQuery :: [Thing] -> SqlPersistT IO ()
setupUnsyncedClientThingQuery = setupUnsyncedClientQuery makeUnsyncedClientThing

setupClientThingQuery :: ClientStore ClientThingId ServerThingId Thing -> SqlPersistT IO ()
setupClientThingQuery = setupClientQuery makeUnsyncedClientThing makeSyncedClientThing

clientGetStoreThingQuery :: SqlPersistT IO (ClientStore ClientThingId ServerThingId Thing)
clientGetStoreThingQuery = clientGetStoreQuery clientMakeThing ClientThingServerId

clientMakeSyncRequestThingQuery :: SqlPersistT IO (SyncRequest ClientThingId ServerThingId Thing)
clientMakeSyncRequestThingQuery = clientMakeSyncRequestQuery clientMakeThing ClientThingServerId

clientMergeSyncResponseThingQuery :: SyncResponse ClientThingId ServerThingId Thing -> SqlPersistT IO ()
clientMergeSyncResponseThingQuery = clientMergeSyncResponseQuery makeSyncedClientThing ClientThingServerId

makeUnsyncedClientThing :: Thing -> ClientThing
makeUnsyncedClientThing Thing {..} =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingServerId = Nothing
    }

makeSyncedClientThing :: ServerThingId -> Thing -> ClientThing
makeSyncedClientThing sid Thing {..} =
  ClientThing
    { clientThingNumber = thingNumber,
      clientThingServerId = Just sid
    }

clientMakeThing :: ClientThing -> Thing
clientMakeThing ClientThing {..} = Thing {thingNumber = clientThingNumber}
