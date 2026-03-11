{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeful.Persistent.TwoClientsSpec
  ( spec,
  )
where

import Control.Monad.Reader
import qualified Data.Map as M
import Data.Mergeful
import qualified Data.Set as S
import Database.Persist.Sql
import Test.Syd hiding (Timed (..), runTest)
import Test.Syd.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  twoClientsSpec $ do
    describe "sanity" $ do
      describe "setupClient & clientGetStore" $ do
        it "roundtrips" $ \te -> forAllValid $ \cstore -> runTest te $ do
          setupClient A cstore
          cstore' <- clientGetStore A
          liftIO $ cstore' `shouldBe` cstore
      describe "setupServer & serverGetStore" $ do
        it "roundtrips" $ \te -> forAllValid $ \sstore -> runTest te $ do
          setupServer sstore
          sstore' <- serverGetStore
          liftIO $ sstore' `shouldBe` sstore
    describe "mergeFromServerStrategy" $ do
      let strat = mergeFromServerStrategy
      mergeFunctionSpec strat
      noDivergenceSpec strat
      xdescribe "Does not hold" $ noDataLossSpec strat
    describe "mergeFromClientStrategy" $ do
      let strat = mergeFromClientStrategy
      mergeFunctionSpec strat
      noDataLossSpec strat
      xdescribe "Does not hold" $ noDivergenceSpec strat
    describe "mergeUsingCRDTStrategy" $ do
      let strat = mergeUsingCRDTStrategy max
      mergeFunctionSpec strat
      noDataLossSpec strat
      noDivergenceSpec strat

mergeFunctionSpec :: ItemMergeStrategy Thing -> SpecWith TestEnv
mergeFunctionSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
  describe "Multiple clients" $ do
    describe "Single item" $ do
      it "successfully syncs an addition accross to a second client" $ \te -> forAllValid $ \k -> forAllValid $ \i -> runTest te $ do
        -- Client A has one item
        setupClient A $ initialClientStore {clientStoreAddedItems = M.singleton k i}
        -- Client B is empty
        setupClient B initialClientStore
        -- The server is empty
        setupServer initialServerStore
        -- Client A makes sync request 1
        req1 <- clientMakeSyncRequest A
        -- The server processes sync request 1
        resp1 <- serverProcessSync req1
        sstore2 <- serverGetStore
        let addedItems = syncResponseClientAdded resp1
        case M.toList addedItems of
          [(k', ClientAddition uuid st)] -> do
            lift $ k' `shouldBe` k
            let time = initialServerTime
            lift $ st `shouldBe` time
            let items = M.singleton uuid (Timed i st)
            lift $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
            -- Client A merges the response
            mergeFunc A resp1
            cAstore2 <- clientGetStore A
            lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
            -- Client B makes sync request 2
            req2 <- clientMakeSyncRequest B
            -- The server processes sync request 2
            resp2 <- serverProcessSync req2
            sstore3 <- serverGetStore
            lift $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
              sstore3 `shouldBe` sstore2
            -- Client B merges the response
            mergeFunc B resp2
            cBstore2 <- clientGetStore B
            lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
            -- Client A and Client B now have the same store
            lift $ cAstore2 `shouldBe` cBstore2
          _ ->
            lift $
              expectationFailure
                "Should have found exactly one added item."
      it "successfully syncs a modification accross to a second client" $ \te -> forAllValid $ \uuid -> forAllValid $ \i -> forAllValid $ \j -> forAllValid $ \time1 ->
        runTest te $ do
          -- Client A has a synced item.
          setupClient A $
            initialClientStore
              { clientStoreSyncedItems = M.singleton uuid (Timed i time1)
              }
          -- Client B had synced that same item, but has since modified it
          setupClient B $
            initialClientStore
              { clientStoreSyncedButChangedItems = M.singleton uuid (Timed j time1)
              }
          -- The server is has the item that both clients had before
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
          -- Client B makes sync request 1
          req1 <- clientMakeSyncRequest B
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          let time2 = incrementServerTime time1
          lift $ do
            resp1
              `shouldBe` emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2}
            sstore2
              `shouldBe` ServerStore {serverStoreItems = M.singleton uuid (Timed j time2)}
          -- Client B merges the response
          mergeFunc B resp1
          cBstore2 <- clientGetStore B
          lift $
            cBstore2
              `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
          -- Client A makes sync request 2
          req2 <- clientMakeSyncRequest A
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2
              `shouldBe` emptySyncResponse
                { syncResponseServerChanged = M.singleton uuid (Timed j time2)
                }
            sstore3 `shouldBe` sstore2
          -- Client A merges the response
          mergeFunc A resp2
          cAstore2 <- clientGetStore A
          lift $
            cAstore2
              `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
          -- Client A and Client B now have the same store
          lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs a deletion across to a second client" $ \te -> forAllValid $ \uuid -> forAllValid $ \time1 -> forAllValid $ \i ->
        runTest te $ do
          setupClient A $
            initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i time1)}
          -- Client A has a synced item.
          -- Client B had synced that same item, but has since deleted it.
          setupClient B $ initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
          -- The server still has the undeleted item
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
          -- Client B makes sync request 1
          req1 <- clientMakeSyncRequest B
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          lift $ do
            resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
            sstore2 `shouldBe` initialServerStore
          -- Client B merges the response
          mergeFunc B resp1
          cBstore2 <- clientGetStore B
          lift $ cBstore2 `shouldBe` initialClientStore
          -- Client A makes sync request 2
          req2 <- clientMakeSyncRequest A
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
            sstore3 `shouldBe` sstore2
          -- Client A merges the response
          mergeFunc A resp2
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` initialClientStore
          -- Client A and Client B now have the same store
          lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $ \te -> forAllValid $ \uuid -> forAllValid $ \time1 -> forAllValid $ \i ->
        runTest te $ do
          setupClient A $ initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
          -- Both client a and client b delete an item.
          setupClient B $ initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
          -- The server still has the undeleted item
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
          -- Client A makes sync request 1
          req1 <- clientMakeSyncRequest A
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          lift $ do
            resp1
              `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
            sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
          -- Client A merges the response
          mergeFunc A resp1
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` initialClientStore
          -- Client B makes sync request 2
          req2 <- clientMakeSyncRequest B
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2
              `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
            sstore3 `shouldBe` sstore2
          -- Client B merges the response
          mergeFunc B resp2
          cBstore2 <- clientGetStore B
          lift $ do
            cBstore2 `shouldBe` initialClientStore
            -- Client A and Client B now have the same store
            cAstore2 `shouldBe` cBstore2
    describe "Multiple items" $ do
      it "successfully syncs additions accross to a second client" $ \te -> forAllValid $ \is ->
        runTest te $ do
          setupClient A $ initialClientStore {clientStoreAddedItems = is}
          -- Client B is empty
          setupClient B initialClientStore
          -- The server is empty
          setupServer initialServerStore
          -- Client A makes sync request 1
          req1 <- clientMakeSyncRequest A
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          let (rest, items) = mergeAddedItems is (syncResponseClientAdded resp1)
          lift $ do
            rest `shouldBe` M.empty
            sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
          -- Client A merges the response
          mergeFunc A resp1
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
          -- Client B makes sync request 2
          req2 <- clientMakeSyncRequest B
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
            sstore3 `shouldBe` sstore2
          -- Client B merges the response
          mergeFunc B resp2
          cBstore2 <- clientGetStore B
          lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
          -- Client A and Client B now have the same store
          lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs deletions across to a second client" $ \te -> forAllValid $ \items -> forAllValid $ \time1 ->
        runTest te $ do
          let syncedItems = M.map (\i -> Timed i time1) items
              itemTimes = M.map (const time1) items
              itemIds = M.keysSet items
          setupClient A $ initialClientStore {clientStoreSyncedItems = syncedItems}
          -- Client A has synced items
          -- Client B had synced the same items, but has since deleted them.
          setupClient B $ initialClientStore {clientStoreDeletedItems = itemTimes}
          -- The server still has the undeleted item
          setupServer $ ServerStore {serverStoreItems = syncedItems}
          -- Client B makes sync request 1
          req1 <- clientMakeSyncRequest B
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          lift $ do
            resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = itemIds}
            sstore2 `shouldBe` initialServerStore
          -- Client B merges the response
          mergeFunc B resp1
          cBstore2 <- clientGetStore B
          lift $ cBstore2 `shouldBe` initialClientStore
          -- Client A makes sync request 2
          req2 <- clientMakeSyncRequest A
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = itemIds}
            sstore3 `shouldBe` sstore2
          -- Client A merges the response
          mergeFunc A resp2
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` initialClientStore
          -- Client A and Client B now have the same store
          lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $ \te -> forAllValid $ \items -> forAllValid $ \time1 ->
        runTest te $ do
          setupClient A $
            initialClientStore {clientStoreDeletedItems = M.map (const time1) items}
          -- Both client a and client b delete their items.
          setupClient B $
            initialClientStore {clientStoreDeletedItems = M.map (const time1) items}
          -- The server still has the undeleted items
          setupServer $ ServerStore {serverStoreItems = M.map (\i -> Timed i time1) items}
          -- Client A makes sync request 1
          req1 <- clientMakeSyncRequest A
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          lift $ do
            resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
            sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
            -- Client A merges the response
          mergeFunc A resp1
          cAstore2 <- clientGetStore A
          lift $ cAstore2 `shouldBe` initialClientStore
          -- Client B makes sync request 2
          req2 <- clientMakeSyncRequest B
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          lift $ do
            resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
            sstore3 `shouldBe` sstore2
          -- Client B merges the response
          mergeFunc B resp2
          cBstore2 <- clientGetStore B
          lift $ do
            cBstore2 `shouldBe` initialClientStore
            -- Client A and Client B now have the same store
            cAstore2 `shouldBe` cBstore2

noDataLossSpec ::
  ItemMergeStrategy Thing ->
  SpecWith TestEnv
noDataLossSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
  it "does not lose data after a conflict occurs" $ \te -> forAllValid $ \uuid -> forAllValid $ \time1 -> forAllValid $ \i1 -> forAllValid $ \i2 -> forAllValid $ \i3 ->
    runTest te $ do
      setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed i1 time1)}
      -- The server has an item
      -- The first client has synced it, and modified it.
      setupClient A $
        initialClientStore
          { clientStoreSyncedButChangedItems = M.singleton uuid (Timed i2 time1)
          }
      -- The second client has synced it too, and modified it too.
      setupClient B $
        initialClientStore
          { clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)
          }
      -- Client A makes sync request 1
      req1 <- clientMakeSyncRequest A
      -- The server processes sync request 1
      resp1 <- serverProcessSync req1
      sstore2 <- serverGetStore
      let time2 = incrementServerTime time1
      -- The server updates the item accordingly
      lift $ do
        resp1
          `shouldBe` (emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2})
        sstore2
          `shouldBe` (ServerStore {serverStoreItems = M.singleton uuid (Timed i2 time2)})
      -- Client A merges the response
      mergeFunc A resp1
      cAstore2 <- clientGetStore A
      lift $
        cAstore2
          `shouldBe` (initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i2 time2)})
      -- Client B makes sync request 2
      req2 <- clientMakeSyncRequest B
      -- The server processes sync request 2
      resp2 <- serverProcessSync req2
      sstore3 <- serverGetStore
      -- The server reports a conflict and does not change its store
      lift $ do
        resp2
          `shouldBe` (emptySyncResponse {syncResponseConflicts = M.singleton uuid (Timed i2 time2)})
        sstore3 `shouldBe` sstore2
      -- Client B merges the response
      clientMergeSyncResponse mergeFromClientStrategy B resp2
      cBstore2 <- clientGetStore B
      -- Client does not update, but keeps its conflict
      -- Client A and Client B now *do not* have the same store
      lift $
        cBstore2
          `shouldBe` ( initialClientStore
                         { clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)
                         }
                     )

noDivergenceSpec :: ItemMergeStrategy Thing -> SpecWith TestEnv
noDivergenceSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
  it "does not diverge after a conflict occurs" $ \te ->
    forAllValid $ \uuid -> forAllValid $ \time1 -> forAllValid $ \iS -> forAllValid $ \iA ->
      forAllValid $ \iB ->
        runTest te $ do
          setupServer $ ServerStore {serverStoreItems = M.singleton uuid (Timed iS time1)}
          -- The server has an item
          -- The first client has synced it, and modified it.
          setupClient A $
            initialClientStore
              { clientStoreSyncedButChangedItems = M.singleton uuid (Timed iA time1)
              }
          -- The second client has synced it too, and modified it too.
          setupClient B $
            initialClientStore
              { clientStoreSyncedButChangedItems = M.singleton uuid (Timed iB time1)
              }
          -- Client A makes sync request 1
          req1 <- clientMakeSyncRequest A
          -- The server processes sync request 1
          resp1 <- serverProcessSync req1
          sstore2 <- serverGetStore
          let time2 = incrementServerTime time1
          -- The server updates the item accordingly
          lift $ do
            resp1
              `shouldBe` (emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2})
            sstore2
              `shouldBe` (ServerStore {serverStoreItems = M.singleton uuid (Timed iA time2)})
          -- Client A merges the response
          mergeFunc A resp1
          cAstore2 <- clientGetStore A
          -- Client A has the item from the server because there was no conflict.
          lift $
            cAstore2
              `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed iA time2)}
          -- Client B makes sync request 2
          req2 <- clientMakeSyncRequest B
          -- The server processes sync request 2
          resp2 <- serverProcessSync req2
          sstore3 <- serverGetStore
          -- The server reports a conflict and does not change its store
          lift $ do
            resp2
              `shouldBe` (emptySyncResponse {syncResponseConflicts = M.singleton uuid (Timed iA time2)})
            sstore3 `shouldBe` sstore2
          -- Client B merges the response
          mergeFunc B resp2
          cBstore2 <- clientGetStore B
          lift $ do
            let expected = case itemMergeStrategyMergeChangeConflict strat iB iA of
                  KeepLocal -> initialClientStore {clientStoreSyncedButChangedItems = M.singleton uuid (Timed iB time1)}
                  TakeRemote -> initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed iA time2)}
                  Merged im -> initialClientStore {clientStoreSyncedButChangedItems = M.singleton uuid (Timed im time2)}
            cBstore2
              `shouldBe` expected
          -- In case of a previous merge, the synced item will still be changed, so we need to sync again with B and then with A
          req3 <- clientMakeSyncRequest B
          resp3 <- serverProcessSync req3
          mergeFunc B resp3
          cBstore3 <- clientGetStore B
          req4 <- clientMakeSyncRequest A
          resp4 <- serverProcessSync req4
          mergeFunc A resp4
          cAstore3 <- clientGetStore A
          lift $
            cBstore3 `shouldBe` cAstore3

type T a = ReaderT TestEnv IO a

runTest :: TestEnv -> T a -> IO a
runTest = flip runReaderT

runClientDB :: Client -> SqlPersistT IO a -> T a
runClientDB num func = do
  pool <- asks $ case num of
    A -> testEnvClient1Pool
    B -> testEnvClient2Pool
  liftIO $ runSqlPool func pool

runServerDB :: SqlPersistT IO a -> T a
runServerDB func = do
  pool <- asks testEnvServerPool
  liftIO $ runSqlPool func pool

type CS = ClientStore ClientThingId ServerThingId Thing

type SReq = SyncRequest ClientThingId ServerThingId Thing

type SS = ServerStore ServerThingId Thing

type SResp = SyncResponse ClientThingId ServerThingId Thing

setupClient :: Client -> CS -> T ()
setupClient client = runClientDB client . setupClientThingQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerThingQuery

clientGetStore :: Client -> T CS
clientGetStore client = runClientDB client clientGetStoreThingQuery

clientMakeSyncRequest :: Client -> T SReq
clientMakeSyncRequest client = runClientDB client clientMakeSyncRequestThingQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreThingQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncThingQuery

clientMergeSyncResponse :: ItemMergeStrategy Thing -> Client -> SResp -> T ()
clientMergeSyncResponse strat client = runClientDB client . clientMergeSyncResponseThingQuery strat

data Client = A | B
  deriving (Show, Eq)

data TestEnv = TestEnv
  { testEnvServerPool :: !ConnectionPool,
    testEnvClient1Pool :: !ConnectionPool,
    testEnvClient2Pool :: !ConnectionPool
  }

twoClientsSpec :: SpecWith TestEnv -> Spec
twoClientsSpec =
  modifyMaxSuccess (`div` 10)
    . around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool ->
    withClientPool $ \client1Pool ->
      withClientPool $ \client2Pool -> do
        let tenv =
              TestEnv
                { testEnvServerPool = serverPool,
                  testEnvClient1Pool = client1Pool,
                  testEnvClient2Pool = client2Pool
                }
        liftIO $ func tenv
