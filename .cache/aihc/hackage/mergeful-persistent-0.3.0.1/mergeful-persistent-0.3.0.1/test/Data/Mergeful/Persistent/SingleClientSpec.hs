{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Mergeful.Persistent.SingleClientSpec
  ( spec,
  )
where

import Control.Monad.Reader
import Data.List
import qualified Data.Map as M
import Data.Mergeful
import Database.Persist.Sql
import Test.Syd hiding (Timed (..), runTest)
import Test.Syd.Validity
import TestUtils

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec =
  oneClientSpec $ do
    describe "sanity" $ do
      describe "setupClient & clientGetStore" $ do
        it "roundtrips" $ \te -> forAllValid $ \cstore -> runTest te $ do
          setupClient cstore
          cstore' <- clientGetStore
          liftIO $ cstore' `shouldBe` cstore
      describe "setupServer & serverGetStore" $ do
        it "roundtrips" $ \te -> forAllValid $ \sstore -> runTest te $ do
          setupServer sstore
          sstore' <- serverGetStore
          liftIO $ sstore' `shouldBe` sstore
    describe "mergeFromServerStrategy" $ do
      let strat = mergeFromServerStrategy
      mergeFunctionSpec strat
      emptyResponseSpec strat
    describe "mergeFromClientStrategy" $ do
      let strat = mergeFromClientStrategy
      mergeFunctionSpec strat
      xdescribe "does not hold" $ emptyResponseSpec strat
    describe "mergeUsingCRDTStrategy" $ do
      let strat = mergeUsingCRDTStrategy max
      mergeFunctionSpec strat
      emptyResponseSpec strat

mergeFunctionSpec :: ItemMergeStrategy Thing -> SpecWith TestEnv
mergeFunctionSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
  describe "Single Client" $ do
    describe "Single item" $ do
      it "succesfully downloads an item from the server for an empty client" $ \te ->
        forAllValid $ \uuid ->
          forAllValid $ \tst -> runTest te $ do
            let sstore1 = ServerStore $ M.singleton uuid tst
            setupClient initialClientStore
            setupServer sstore1
            req <- clientMakeSyncRequest
            resp <- serverProcessSync req
            sstore2 <- serverGetStore
            mergeFunc resp
            cstore2 <- clientGetStore
            lift $ do
              sstore2 `shouldBe` sstore1
              clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads an item to the server for an empty server" $ \te ->
        forAllValid $ \cid ->
          forAllValid $ \t ->
            runTest te $ do
              let cstore1 = initialClientStore {clientStoreAddedItems = M.singleton cid t}
              let sstore1 = initialServerStore
              setupClient cstore1
              setupServer sstore1
              req <- clientMakeSyncRequest
              resp <- serverProcessSync req
              sstore2 <- serverGetStore
              mergeFunc resp
              cstore2 <- clientGetStore
              lift $ do
                sort (M.elems (M.map timedValue (clientStoreSyncedItems cstore2)))
                  `shouldBe` sort [t]
                clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
    describe "Multiple items" $ do
      it "succesfully downloads an item from the server for an empty client" $ \te ->
        forAllValid $ \sstore1 -> runTest te $ do
          setupClient initialClientStore
          setupServer sstore1
          req <- clientMakeSyncRequest
          resp <- serverProcessSync req
          sstore2 <- serverGetStore
          mergeFunc resp
          cstore2 <- clientGetStore
          lift $ do
            sstore2 `shouldBe` sstore1
            clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
      it "succesfully uploads everything to the server for an empty server" $ \te ->
        forAllValid $ \items ->
          runTest te $ do
            let cstore1 = initialClientStore {clientStoreAddedItems = items}
            let sstore1 = initialServerStore
            setupClient cstore1
            setupServer sstore1
            req <- clientMakeSyncRequest
            resp <- serverProcessSync req
            sstore2 <- serverGetStore
            mergeFunc resp
            cstore2 <- clientGetStore
            lift $ do
              sort (M.elems (M.map timedValue (clientStoreSyncedItems cstore2)))
                `shouldBe` sort (M.elems items)
              clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2

emptyResponseSpec :: ItemMergeStrategy Thing -> SpecWith TestEnv
emptyResponseSpec strat = do
  let mergeFunc = clientMergeSyncResponse strat
  it "is returns an empty response on the second sync with no modifications" $ \te -> forAllValid $ \cstore1 -> forAllValid $ \sstore1 ->
    runTest te $ do
      setupClient cstore1
      setupServer sstore1
      req1 <- clientMakeSyncRequest
      resp1 <- serverProcessSync req1
      mergeFunc resp1
      req2 <- clientMakeSyncRequest
      resp2 <- serverProcessSync req2
      lift $ resp2 `shouldBe` emptySyncResponse

type T a = ReaderT TestEnv IO a

runTest :: TestEnv -> T a -> IO a
runTest = flip runReaderT

runClientDB :: SqlPersistT IO a -> T a
runClientDB func = do
  pool <- asks testEnvClientPool
  liftIO $ runSqlPool func pool

runServerDB :: SqlPersistT IO a -> T a
runServerDB func = do
  pool <- asks testEnvServerPool
  liftIO $ runSqlPool func pool

type CS = ClientStore ClientThingId ServerThingId Thing

type SReq = SyncRequest ClientThingId ServerThingId Thing

type SS = ServerStore ServerThingId Thing

type SResp = SyncResponse ClientThingId ServerThingId Thing

setupClient :: CS -> T ()
setupClient = runClientDB . setupClientThingQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerThingQuery

clientGetStore :: T CS
clientGetStore = runClientDB clientGetStoreThingQuery

clientMakeSyncRequest :: T SReq
clientMakeSyncRequest = runClientDB clientMakeSyncRequestThingQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreThingQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncThingQuery

clientMergeSyncResponse :: ItemMergeStrategy Thing -> SResp -> T ()
clientMergeSyncResponse strat = runClientDB . clientMergeSyncResponseThingQuery strat

data TestEnv = TestEnv
  { testEnvClientPool :: !ConnectionPool,
    testEnvServerPool :: !ConnectionPool
  }

oneClientSpec :: SpecWith TestEnv -> Spec
oneClientSpec =
  modifyMaxSuccess (`div` 10)
    . around withTestEnv

withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv func =
  withServerPool $ \serverPool -> withClientPool $ \clientPool -> do
    let tenv = TestEnv {testEnvClientPool = clientPool, testEnvServerPool = serverPool}
    liftIO $ func tenv
