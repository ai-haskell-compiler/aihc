{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Appendful.Persistent.TwoClientsSpec
  ( spec,
  )
where

import Control.Monad
import Control.Monad.Reader
import Data.Appendful
import Data.GenValidity.Appendful ()
import qualified Data.Map as M
import Database.Persist.Sql
import Test.Syd hiding (runTest)
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
    describe "Single item" $ do
      it "successfully syncs an addition accross to a second client" $
        \te ->
          forAllValid $ \st -> runTest te $ do
            setupUnsyncedClient A [st]
            setupUnsyncedClient B []
            setupServer emptyServerStore
            req1 <- clientMakeSyncRequest A
            resp1 <- serverProcessSync req1
            sstore2 <- serverGetStore
            case M.toList (syncResponseClientAdded resp1) of
              [(_, clientAdditionId)] -> do
                let items = M.singleton clientAdditionId st
                liftIO $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                clientMergeSyncResponse A resp1
                cAstore2 <- clientGetStore A
                liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                req2 <- clientMakeSyncRequest B
                resp2 <- serverProcessSync req2
                sstore3 <- serverGetStore
                liftIO $ do
                  resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                  sstore3 `shouldBe` sstore2
                clientMergeSyncResponse B resp2
                cBstore2 <- clientGetStore B
                liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
                liftIO $ cAstore2 `shouldBe` cBstore2
              _ -> liftIO $ expectationFailure "Should have found exactly one added item."
    describe "Multiple items" $ do
      it
        "makes no change if the sync request reflects the same local state with an empty sync response"
        $ \te ->
          forAllValid $ \sis -> runTest te $ do
            let cs = ServerStore sis
            setupServer cs
            sr <-
              serverProcessSync
                SyncRequest
                  { syncRequestAdded = M.empty,
                    syncRequestMaximumSynced = fst <$> M.lookupMax sis
                  }
            cs' <- serverGetStore
            liftIO $
              do
                cs' `shouldBe` cs
                sr
                  `shouldBe` SyncResponse
                    { syncResponseClientAdded = M.empty,
                      syncResponseServerAdded = M.empty
                    }
      it "successfully syncs additions accross to a second client" $
        \te -> forAllValid $ \is ->
          runTest te $ do
            setupClient A $ emptyClientStore {clientStoreAdded = is}
            -- Client B is empty
            setupClient B emptyClientStore
            -- The server is empty
            setupServer emptyServerStore
            -- Client A makes sync request 1
            req1 <- clientMakeSyncRequest A
            -- The server processes sync request 1
            resp1 <- serverProcessSync req1
            sstore2 <- serverGetStore
            -- Client A merges the response
            clientMergeSyncResponse A resp1
            cAstore2 <- clientGetStore A
            let items = clientStoreSynced cAstore2
            liftIO $ do
              clientStoreAdded cAstore2 `shouldBe` M.empty
              sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
            liftIO $ cAstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
            -- Client B makes sync request 2
            req2 <- clientMakeSyncRequest B
            -- The server processes sync request 2
            resp2 <- serverProcessSync req2
            sstore3 <- serverGetStore
            liftIO $ do
              resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
              sstore3 `shouldBe` sstore2
            -- Client B merges the response
            clientMergeSyncResponse B resp2
            cBstore2 <- clientGetStore B
            liftIO $ cBstore2 `shouldBe` (emptyClientStore {clientStoreSynced = items})
            -- Client A and Client B now have the same store
            liftIO $ cAstore2 `shouldBe` cBstore2
    describe "General properties" $
      it "successfully syncs two clients using a central store" $
        \te ->
          forAllValid $ \addedItems ->
            runTest te $
              do
                setupServer $ ServerStore M.empty
                let store1 = emptyClientStore {clientStoreAdded = addedItems}
                setupClient A store1
                setupClient B emptyClientStore
                void $ sync A
                (_, _, _, store2') <- sync B
                (_, _, _, store1'') <- sync A
                liftIO $ store1'' `shouldBe` store2'

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

sync :: Client -> T (CS, SS, SS, CS)
sync n = do
  cstore1 <- clientGetStore n
  req <- clientMakeSyncRequest n
  sstore1 <- serverGetStore
  resp <- serverProcessSync req
  sstore2 <- serverGetStore
  clientMergeSyncResponse n resp
  cstore2 <- clientGetStore n
  pure (cstore1, sstore1, sstore2, cstore2)

setupUnsyncedClient :: Client -> [Thing] -> T ()
setupUnsyncedClient n =
  runClientDB n . setupUnsyncedClientThingQuery

setupClient :: Client -> CS -> T ()
setupClient n = runClientDB n . setupClientThingQuery

setupServer :: SS -> T ()
setupServer = runServerDB . setupServerThingQuery

clientGetStore :: Client -> T CS
clientGetStore n = runClientDB n clientGetStoreThingQuery

clientMakeSyncRequest :: Client -> T SReq
clientMakeSyncRequest n = runClientDB n clientMakeSyncRequestThingQuery

serverGetStore :: T SS
serverGetStore = runServerDB serverGetStoreThingQuery

serverProcessSync :: SReq -> T SResp
serverProcessSync = runServerDB . serverProcessSyncThingQuery

clientMergeSyncResponse :: Client -> SResp -> T ()
clientMergeSyncResponse n = runClientDB n . clientMergeSyncResponseThingQuery

data Client = A | B
  deriving (Show, Eq)

data TestEnv = TestEnv
  { testEnvServerPool :: ConnectionPool,
    testEnvClient1Pool :: ConnectionPool,
    testEnvClient2Pool :: ConnectionPool
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
