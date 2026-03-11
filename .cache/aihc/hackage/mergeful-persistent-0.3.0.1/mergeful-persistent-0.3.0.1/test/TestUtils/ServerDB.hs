{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module TestUtils.ServerDB where

import Data.GenValidity
import Data.GenValidity.Mergeful ()
import Data.Mergeful
import Data.Mergeful.Persistent
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics (Generic)

-- The thing that we'll synchronise on
newtype Thing = Thing {thingNumber :: Int}
  deriving (Show, Eq, Ord, Generic)

instance Validity Thing

instance GenValid Thing

share
  [mkPersist sqlSettings, mkMigrate "migrateServer"]
  [persistLowerCase|

ServerThing
  -- All the fields of 'Thing' go here.
  number Int
  time ServerTime

  deriving Show
  deriving Eq
  deriving Ord
  deriving Generic

|]

instance Validity ServerThing

instance GenValid ServerThing

setupServerThingQuery :: ServerStore ServerThingId Thing -> SqlPersistT IO ()
setupServerThingQuery = setupServerQuery serverUnmakeThing

serverGetStoreThingQuery :: SqlPersistT IO (ServerStore ServerThingId Thing)
serverGetStoreThingQuery = serverGetStoreQuery (\(Entity sid sr) -> (sid, serverMakeThing sr))

serverProcessSyncThingQuery :: forall ci. Ord ci => SyncRequest ci ServerThingId Thing -> SqlPersistT IO (SyncResponse ci ServerThingId Thing)
serverProcessSyncThingQuery = serverProcessSyncQuery ServerThingTime [] serverMakeThing thingToServer serverRecordUpdates

serverMakeThing :: ServerThing -> Timed Thing
serverMakeThing ServerThing {..} = Timed {timedValue = Thing {thingNumber = serverThingNumber}, timedTime = serverThingTime}

serverUnmakeThing :: ServerThingId -> Timed Thing -> Entity ServerThing
serverUnmakeThing sid Timed {..} = Entity sid $ ServerThing {serverThingNumber = thingNumber timedValue, serverThingTime = timedTime}

serverRecordUpdates :: Thing -> [Update ServerThing]
serverRecordUpdates Thing {..} = [ServerThingNumber =. thingNumber]

thingToServer :: cid -> Thing -> ServerThing
thingToServer _ Thing {..} = ServerThing {serverThingTime = initialServerTime, serverThingNumber = thingNumber}
