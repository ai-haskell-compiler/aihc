{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Data.Mergeful.CollectionSpec
  ( spec,
  )
where

import Autodocodec
import Autodocodec.Yaml
import Control.Monad
import Control.Monad.State
import Data.Data
import Data.Functor.Identity
import Data.GenValidity.Mergeful ()
import Data.GenValidity.Mergeful.Item ()
import Data.GenValidity.UUID ()
import Data.List
import qualified Data.Map as M
import Data.Mergeful
import qualified Data.Set as S
import Data.UUID
import Data.Word
import GHC.Generics (Generic)
import System.Random
import Test.QuickCheck
import Test.Syd hiding (Timed (..))
import Test.Syd.Validity
import Test.Syd.Validity.Aeson
import Test.Syd.Validity.Utils
import Text.Colour

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = modifyMaxShrinks (min 0) $ do
  let yamlSchemaSpec :: forall a. (Typeable a, GenValid a, HasCodec a) => FilePath -> Spec
      yamlSchemaSpec filePath = do
        it ("outputs the same schema as before for " <> nameOf @a) $
          pureGoldenTextFile
            ("test_resources/collection/" <> filePath <> ".txt")
            (renderChunksText With24BitColours $ schemaChunksViaCodec @a)

  describe "ClientId" $ do
    genValidSpec @ClientId
    jsonSpec @ClientId
    yamlSchemaSpec @ClientId "client-id"
  describe "ClientStore" $ do
    genValidSpec @(ClientStore ClientId Word8 Word8)
    jsonSpec @(ClientStore ClientId Word8 Word8)
    yamlSchemaSpec @(ClientStore ClientId Word8 Word8) "client-store"
  describe "ServerStore" $ do
    genValidSpec @(ServerStore Word8 Word8)
    jsonSpec @(ServerStore Word8 Word8)
    yamlSchemaSpec @(ServerStore Word8 Word8) "server-store"
  describe "SyncRequest" $ do
    genValidSpec @(SyncRequest ClientId Word8 Word8)
    jsonSpec @(SyncRequest ClientId Word8 Word8)
    yamlSchemaSpec @(SyncRequest ClientId Word8 Word8) "request"
  describe "ClientAddition" $ do
    genValidSpec @(ClientAddition Word8)
    jsonSpec @(ClientAddition Word8)
    yamlSchemaSpec @(ClientAddition Word8) "client-addition"
  describe "SyncResponse" $ do
    genValidSpec @(SyncResponse ClientId Word8 Word8)
    jsonSpec @(SyncResponse ClientId Word8 Word8)
    yamlSchemaSpec @(SyncResponse ClientId Word8 Word8) "response"

  describe "initialClientStore" $
    it "is valid" $
      shouldBeValid $
        initialClientStore @ClientId @Int @Int
  describe "addItemToClientStore" $ do
    it "produces valid stores" $ producesValid2 (addItemToClientStore @ClientId @Int @Int)
    it "makes the client store one bigger" $
      forAllValid $
        \cs ->
          forAllValid $ \a ->
            clientStoreSize @ClientId @Int @Int (addItemToClientStore a cs)
              `shouldBe` (clientStoreSize cs + 1)
    it "ensures that the added item is in fact in the result" $
      forAllValid $
        \cs ->
          forAllValid $ \a ->
            let cs' = addItemToClientStore @ClientId @Int @Int a cs
             in a `elem` clientStoreAddedItems cs'
  describe "markItemDeletedInClientStore" $ do
    it "produces valid stores" $
      producesValid2 (markItemDeletedInClientStore @ClientId @Int @Int)
    it "makes the client store one smaller" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              (clientStoreSize @ClientId @Int @Int (markItemDeletedInClientStore i cs) + 1)
                `shouldBe` clientStoreSize cs
    it "ensures that the added item is in fact not in the result" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              let cs' = markItemDeletedInClientStore @ClientId @Int @Int i cs
               in M.lookup (Right i) (clientStoreItems cs') `shouldBe` Nothing
  describe "changeItemInClientStore" $ do
    it "produces valid stores" $
      producesValid3 (changeItemInClientStore @ClientId @Int @Int)
    it "doesn't change the size of the client store" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              forAllValid $ \a ->
                clientStoreSize @ClientId @Int @Int (changeItemInClientStore i a cs)
                  `shouldBe` clientStoreSize cs
    it "ensures that the changed item is in fact in the result" $
      let p cs = S.size (clientStoreUndeletedSyncIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreUndeletedSyncIdSet cs) $ \i ->
              forAllValid $ \a ->
                let cs' = changeItemInClientStore @ClientId @Int @Int i a cs
                 in M.lookup (Right i) (clientStoreItems cs') `shouldBe` Just a
  describe "deleteItemFromClientStore" $ do
    it "produces valid stores" $
      producesValid2 (deleteItemFromClientStore @ClientId @Int @Int)
    it "makes the client store one smaller" $
      let p cs = S.size (clientStoreClientIdSet cs) > 0
       in forAllShrink (genValid `suchThat` p) (filter p . shrinkValid) $ \cs ->
            forAll (elements $ S.toList $ clientStoreClientIdSet cs) $ \cid ->
              (clientStoreSize @ClientId @Int @Int (deleteItemFromClientStore cid cs) + 1)
                `shouldBe` clientStoreSize cs
  describe "initialServerStore" $ it "is valid" $ shouldBeValid $ initialServerStore @Int @Int
  describe "initialSyncRequest" $
    it "is valid" $
      shouldBeValid $
        initialSyncRequest @ClientId @Int @Int
  describe "emptySyncResponse" $
    it "is valid" $
      shouldBeValid $
        emptySyncResponse @ClientId @Int @Int
  describe "makeSyncRequest" $
    it "produces valid requests" $
      producesValid (makeSyncRequest @ClientId @Int @Int)
  describe "mergeAddedItems" $
    it "produces valid results" $
      producesValid2 (mergeAddedItems @ClientId @Int @Int)
  describe "mergeSyncedButChangedItems" $
    it "produces valid results" $
      producesValid2 (mergeSyncedButChangedItems @Int @Int)
  describe "mergeDeletedItems" $
    it "produces valid results" $
      producesValid2 (mergeDeletedItems @Int @Int)
  xdescribe "There must not be id conflicts" $ do
    describe "mergeSyncResponseFromServer" $
      it "produces valid requests" $
        forAllValid $
          \store ->
            forAllValid $ \response ->
              let res = mergeSyncResponseFromServer @ClientId @Int @Int store response
               in case prettyValidate res of
                    Right _ -> pure ()
                    Left err ->
                      expectationFailure $
                        unlines
                          [ "Store:",
                            ppShow store,
                            "Response:",
                            ppShow response,
                            "Invalid result:",
                            ppShow res,
                            "error:",
                            err
                          ]
    describe "mergeSyncResponseFromClient" $
      it "produces valid requests" $
        forAllValid $
          \store ->
            forAllValid $ \response ->
              let res = mergeSyncResponseFromClient @ClientId @Int @Int store response
               in case prettyValidate res of
                    Right _ -> pure ()
                    Left err ->
                      expectationFailure $
                        unlines
                          [ "Store:",
                            ppShow store,
                            "Response:",
                            ppShow response,
                            "Invalid result:",
                            ppShow res,
                            "error:",
                            err
                          ]
    describe "mergeSyncResponseUsingCRDT" $
      it "produces valid requests" $
        forAllValid $
          \store ->
            forAllValid $ \response ->
              let res = mergeSyncResponseUsingCRDT @ClientId @Int @Int max store response
               in case prettyValidate res of
                    Right _ -> pure ()
                    Left err ->
                      expectationFailure $
                        unlines
                          [ "Store:",
                            ppShow store,
                            "Response:",
                            ppShow response,
                            "Invalid result:",
                            ppShow res,
                            "error:",
                            err
                          ]
  describe "processServerSync" $
    it "produces valid tuples of a response and a store" $
      producesValid2
        ( \store request ->
            evalD $ processServerSync @ClientId @UUID genD (store :: ServerStore UUID Int) request
        )
  describe "Syncing with mergeSyncResponseFromClient" $ do
    let strat = mergeFromClientStrategy
    helperFunctionsSpec strat
    mergeFunctionSpec @Int strat
    noDataLossSpec @Int strat
    xdescribe "does not hold" $ do
      emptyResponseSpec @Int strat
      noDivergenceSpec @Int strat
  describe "Syncing with mergeSyncResponseFromServer" $ do
    let strat = mergeFromServerStrategy
    helperFunctionsSpec strat
    mergeFunctionSpec @Int strat
    noDivergenceSpec @Int strat
    emptyResponseSpec @Int strat
    noDifferentExceptForConflicts @Int strat mergeFromClientStrategy
    xdescribe "does not hold" $ noDataLossSpec @Int strat
  describe "Syncing with mergeSyncResponseUsingStrategy with a GCounter" $ do
    let strat = mergeUsingCRDTStrategy max
    helperFunctionsSpec strat
    mergeFunctionSpec strat
    noDataLossSpec strat
    noDivergenceSpec strat
    emptyResponseSpec strat
    noDifferentExceptForConflicts strat mergeFromClientStrategy
    noDifferentExceptForConflicts strat mergeFromServerStrategy

helperFunctionsSpec :: ItemMergeStrategy Int -> Spec
helperFunctionsSpec ItemMergeStrategy {..} = do
  describe "mergeSyncedButChangedConflicts" $
    it "produces valid results" $
      producesValid2 (mergeSyncedButChangedConflicts @Int @Int itemMergeStrategyMergeChangeConflict)
  describe "mergeClientDeletedConflicts" $
    it "produces valid results" $
      producesValid (mergeClientDeletedConflicts @Int @Int itemMergeStrategyMergeClientDeletedConflict)
  describe "mergeServerDeletedConflicts" $
    it "produces valid results" $
      producesValid (mergeServerDeletedConflicts @Int @Int itemMergeStrategyMergeServerDeletedConflict)

mergeFunctionSpec ::
  forall a.
  (Show a, Ord a, GenValid a) =>
  ItemMergeStrategy a ->
  Spec
mergeFunctionSpec strat = do
  let mergeFunc = mergeSyncResponseUsingStrategy strat
  describe "Single client" $
    describe "Multi-item" $
      do
        it "succesfully downloads everything from the server for an empty client" $
          forAllValid $
            \sstore1 ->
              evalDM $ do
                let cstore1 = initialClientStore
                let req = makeSyncRequest cstore1
                (resp, sstore2) <- processServerSync genD sstore1 req
                let cstore2 = mergeFunc cstore1 resp
                lift $ do
                  sstore2 `shouldBe` sstore1
                  clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
        it "succesfully uploads everything to the server for an empty server" $
          forAllValid $
            \items ->
              evalDM $ do
                let cstore1 = initialClientStore {clientStoreAddedItems = items}
                let sstore1 = initialServerStore
                let req = makeSyncRequest cstore1
                (resp, sstore2) <- processServerSync genD sstore1 req
                let cstore2 = mergeFunc cstore1 resp
                lift $ do
                  sort (M.elems (M.map timedValue (clientStoreSyncedItems cstore2)))
                    `shouldBe` sort (M.elems items)
                  clientStoreSyncedItems cstore2 `shouldBe` serverStoreItems sstore2
        it "is idempotent with one client" $
          forAllValid $
            \cstore1 ->
              forAllValid $ \sstore1 ->
                evalDM $ do
                  let req1 = makeSyncRequest cstore1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let cstore2 = mergeFunc cstore1 resp1
                      req2 = makeSyncRequest cstore2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  let cstore3 = mergeFunc cstore2 resp2
                  lift $ do
                    cstore2 `shouldBe` cstore3
                    sstore2 `shouldBe` sstore3
  describe "Multiple clients" $ do
    describe "Single-item" $ do
      it "successfully syncs an addition accross to a second client" $
        forAllValid $
          \i ->
            evalDM $ do
              let cAstore1 = initialClientStore {clientStoreAddedItems = M.singleton (ClientId 0) i}
              -- Client B is empty
              let cBstore1 = initialClientStore
              -- The server is empty
              let sstore1 = initialServerStore
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              let addedItems = syncResponseClientAdded resp1
              case M.toList addedItems of
                [(ClientId 0, ClientAddition uuid st)] -> do
                  let time = initialServerTime
                  lift $ st `shouldBe` time
                  let items = M.singleton uuid (Timed i st)
                  lift $ sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
                  -- Client A merges the response
                  let cAstore2 = mergeFunc cAstore1 resp1
                  lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  lift $ do
                    resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                    sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                  let cBstore2 = mergeFunc cBstore1 resp2
                  lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
                  -- Client A and Client B now have the same store
                  lift $ cAstore2 `shouldBe` cBstore2
                _ -> lift $ expectationFailure "Should have found exactly one added item."
      it "successfully syncs a modification accross to a second client" $
        forAllValid $
          \uuid ->
            forAllValid $ \i ->
              forAllValid $ \j ->
                forAllValid $ \time1 ->
                  evalDM $ do
                    let cAstore1 =
                          initialClientStore
                            { clientStoreSyncedItems = M.singleton uuid (Timed i time1)
                            }
                    -- Client B had synced that same item, but has since modified it
                    let cBstore1 =
                          initialClientStore
                            { clientStoreSyncedButChangedItems = M.singleton uuid (Timed j time1)
                            }
                    -- The server is has the item that both clients had before
                    let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
                    -- Client B makes sync request 1
                    let req1 = makeSyncRequest cBstore1
                    -- The server processes sync request 1
                    (resp1, sstore2) <- processServerSync genD sstore1 req1
                    let time2 = incrementServerTime time1
                    lift $ do
                      resp1
                        `shouldBe` emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2}
                      sstore2
                        `shouldBe` ServerStore {serverStoreItems = M.singleton uuid (Timed j time2)}
                    -- Client B merges the response
                    let cBstore2 = mergeFunc cBstore1 resp1
                    lift $
                      cBstore2
                        `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
                    -- Client A makes sync request 2
                    let req2 = makeSyncRequest cAstore1
                    -- The server processes sync request 2
                    (resp2, sstore3) <- processServerSync genD sstore2 req2
                    lift $ do
                      resp2
                        `shouldBe` emptySyncResponse
                          { syncResponseServerChanged = M.singleton uuid (Timed j time2)
                          }
                      sstore3 `shouldBe` sstore2
                    -- Client A merges the response
                    let cAstore2 = mergeFunc cAstore1 resp2
                    lift $
                      cAstore2
                        `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed j time2)}
                    -- Client A and Client B now have the same store
                    lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs a deletion across to a second client" $
        forAllValid $
          \uuid ->
            forAllValid $ \time1 ->
              forAllValid $ \i ->
                evalDM $ do
                  let cAstore1 =
                        initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i time1)}
                  -- Client A has a synced item.
                  -- Client B had synced that same item, but has since deleted it.
                  let cBstore1 = initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- The server still has the undeleted item
                  let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
                  -- Client B makes sync request 1
                  let req1 = makeSyncRequest cBstore1
                  -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  lift $ do
                    resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = S.singleton uuid}
                    sstore2 `shouldBe` initialServerStore
                  -- Client B merges the response
                  let cBstore2 = mergeFunc cBstore1 resp1
                  lift $ cBstore2 `shouldBe` initialClientStore
                  -- Client A makes sync request 2
                  let req2 = makeSyncRequest cAstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  lift $ do
                    resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = S.singleton uuid}
                    sstore3 `shouldBe` sstore2
                  -- Client A merges the response
                  let cAstore2 = mergeFunc cAstore1 resp2
                  lift $ cAstore2 `shouldBe` initialClientStore
                  -- Client A and Client B now have the same store
                  lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $
        forAllValid $
          \uuid ->
            forAllValid $ \time1 ->
              forAllValid $ \i ->
                evalDM $ do
                  let cAstore1 = initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- Both client a and client b delete an item.
                  let cBstore1 = initialClientStore {clientStoreDeletedItems = M.singleton uuid time1}
                  -- The server still has the undeleted item
                  let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i time1)}
                  -- Client A makes sync request 1
                  let req1 = makeSyncRequest cAstore1
                  -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  lift $ do
                    resp1
                      `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                    sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty})
                  -- Client A merges the response
                  let cAstore2 = mergeFunc cAstore1 resp1
                  lift $ cAstore2 `shouldBe` initialClientStore
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  lift $ do
                    resp2
                      `shouldBe` (emptySyncResponse {syncResponseClientDeleted = S.singleton uuid})
                    sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                  let cBstore2 = mergeFunc cBstore1 resp2
                  lift $ do
                    cBstore2 `shouldBe` initialClientStore
                    -- Client A and Client B now have the same store
                    cAstore2 `shouldBe` cBstore2
    describe "Multiple items" $ do
      it "successfully syncs additions accross to a second client" $
        forAllValid $
          \is ->
            evalDM $ do
              let cAstore1 = initialClientStore {clientStoreAddedItems = is}
              -- Client B is empty
              let cBstore1 = initialClientStore
              -- The server is empty
              let sstore1 = initialServerStore
              -- Client A makes sync request 1
              let req1 = makeSyncRequest cAstore1
              -- The server processes sync request 1
              (resp1, sstore2) <- processServerSync genD sstore1 req1
              let (rest, items) = mergeAddedItems is (syncResponseClientAdded resp1)
              lift $ do
                rest `shouldBe` M.empty
                sstore2 `shouldBe` (ServerStore {serverStoreItems = items})
              -- Client A merges the response
              let cAstore2 = mergeFunc cAstore1 resp1
              lift $ cAstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
              -- Client B makes sync request 2
              let req2 = makeSyncRequest cBstore1
              -- The server processes sync request 2
              (resp2, sstore3) <- processServerSync genD sstore2 req2
              lift $ do
                resp2 `shouldBe` (emptySyncResponse {syncResponseServerAdded = items})
                sstore3 `shouldBe` sstore2
              -- Client B merges the response
              let cBstore2 = mergeFunc cBstore1 resp2
              lift $ cBstore2 `shouldBe` (initialClientStore {clientStoreSyncedItems = items})
              -- Client A and Client B now have the same store
              lift $ cAstore2 `shouldBe` cBstore2
      it "succesfully syncs deletions across to a second client" $
        forAllValid $
          \items ->
            forAllValid $ \time1 ->
              evalDM $ do
                let syncedItems = M.map (\i -> Timed i time1) items
                    itemTimes = M.map (const time1) items
                    itemIds = M.keysSet items
                let cAstore1 = initialClientStore {clientStoreSyncedItems = syncedItems}
                -- Client A has synced items
                -- Client B had synced the same items, but has since deleted them.
                let cBstore1 = initialClientStore {clientStoreDeletedItems = itemTimes}
                -- The server still has the undeleted item
                let sstore1 = ServerStore {serverStoreItems = syncedItems}
                -- Client B makes sync request 1
                let req1 = makeSyncRequest cBstore1
                -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                lift $ do
                  resp1 `shouldBe` emptySyncResponse {syncResponseClientDeleted = itemIds}
                  sstore2 `shouldBe` initialServerStore
                -- Client B merges the response
                let cBstore2 = mergeFunc cBstore1 resp1
                lift $ cBstore2 `shouldBe` initialClientStore
                -- Client A makes sync request 2
                let req2 = makeSyncRequest cAstore1
                -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe` emptySyncResponse {syncResponseServerDeleted = itemIds}
                  sstore3 `shouldBe` sstore2
                -- Client A merges the response
                let cAstore2 = mergeFunc cAstore1 resp2
                lift $ cAstore2 `shouldBe` initialClientStore
                -- Client A and Client B now have the same store
                lift $ cAstore2 `shouldBe` cBstore2
      it "does not run into a conflict if two clients both try to sync a deletion" $
        forAllValid $
          \items ->
            forAllValid $ \time1 ->
              evalDM $ do
                let cAstore1 =
                      initialClientStore {clientStoreDeletedItems = M.map (const time1) items}
                -- Both client a and client b delete their items.
                let cBstore1 =
                      initialClientStore {clientStoreDeletedItems = M.map (const time1) items}
                -- The server still has the undeleted items
                let sstore1 = ServerStore {serverStoreItems = M.map (\i -> Timed i time1) items}
                -- Client A makes sync request 1
                let req1 = makeSyncRequest cAstore1
                -- The server processes sync request 1
                (resp1, sstore2) <- processServerSync genD sstore1 req1
                lift $ do
                  resp1 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
                  sstore2 `shouldBe` (ServerStore {serverStoreItems = M.empty}) -- TODO will probably need some sort of tombstoning.
                  -- Client A merges the response
                let cAstore2 = mergeFunc cAstore1 resp1
                lift $ cAstore2 `shouldBe` initialClientStore
                -- Client B makes sync request 2
                let req2 = makeSyncRequest cBstore1
                -- The server processes sync request 2
                (resp2, sstore3) <- processServerSync genD sstore2 req2
                lift $ do
                  resp2 `shouldBe` (emptySyncResponse {syncResponseClientDeleted = M.keysSet items})
                  sstore3 `shouldBe` sstore2
                -- Client B merges the response
                let cBstore2 = mergeFunc cBstore1 resp2
                lift $ do
                  cBstore2 `shouldBe` initialClientStore
                  -- Client A and Client B now have the same store
                  cAstore2 `shouldBe` cBstore2

noDataLossSpec ::
  forall a.
  (Show a, Ord a, GenValid a) =>
  ItemMergeStrategy a ->
  Spec
noDataLossSpec strat = do
  let mergeFunc = mergeSyncResponseUsingStrategy strat
  it "does not lose data after a conflict occurs" $
    forAllValid $
      \uuid ->
        forAllValid $ \time1 ->
          forAllValid $ \i1 ->
            forAllValid $ \i2 ->
              forAllValid $ \i3 ->
                evalDM $ do
                  let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed i1 time1)}
                  -- The server has an item
                  -- The first client has synced it, and modified it.
                  let cAstore1 =
                        initialClientStore
                          { clientStoreSyncedButChangedItems = M.singleton uuid (Timed i2 time1)
                          }
                  -- The second client has synced it too, and modified it too.
                  let cBstore1 =
                        initialClientStore
                          { clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)
                          }
                  -- Client A makes sync request 1
                  let req1 = makeSyncRequest @ClientId @UUID cAstore1
                  -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let time2 = incrementServerTime time1
                  -- The server updates the item accordingly
                  lift $ do
                    resp1
                      `shouldBe` (emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2})
                    sstore2
                      `shouldBe` (ServerStore {serverStoreItems = M.singleton uuid (Timed i2 time2)})
                  -- Client A merges the response
                  let cAstore2 = mergeFunc cAstore1 resp1
                  lift $
                    cAstore2
                      `shouldBe` (initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed i2 time2)})
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  -- The server reports a conflict and does not change its store
                  lift $ do
                    resp2
                      `shouldBe` (emptySyncResponse {syncResponseConflicts = M.singleton uuid (Timed i2 time2)})
                    sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                  let cBstore2 = mergeSyncResponseFromClient @ClientId @UUID cBstore1 resp2
                  -- Client does not update, but keeps its conflict
                  -- Client A and Client B now *do not* have the same store
                  lift $
                    cBstore2
                      `shouldBe` ( initialClientStore
                                     { clientStoreSyncedButChangedItems = M.singleton uuid (Timed i3 time1)
                                     }
                                 )

noDivergenceSpec ::
  forall a.
  (Show a, Ord a, GenValid a) =>
  ItemMergeStrategy a ->
  Spec
noDivergenceSpec strat = do
  let mergeFunc = mergeSyncResponseUsingStrategy strat
  it "does not diverge after a conflict occurs" $
    forAllValid $
      \uuid ->
        forAllValid $ \time1 ->
          forAllValid $ \iS ->
            forAllValid $ \iA ->
              forAllValid $ \iB ->
                evalDM $ do
                  let sstore1 = ServerStore {serverStoreItems = M.singleton uuid (Timed iS time1)}
                  -- The server has an item
                  -- The first client has synced it, and modified it.
                  let cAstore1 =
                        initialClientStore
                          { clientStoreSyncedButChangedItems = M.singleton uuid (Timed iA time1)
                          }
                  -- The second client has synced it too, and modified it too.
                  let cBstore1 =
                        initialClientStore
                          { clientStoreSyncedButChangedItems = M.singleton uuid (Timed iB time1)
                          }
                  -- Client A makes sync request 1
                  let req1 = makeSyncRequest @ClientId @UUID cAstore1
                  -- The server processes sync request 1
                  (resp1, sstore2) <- processServerSync genD sstore1 req1
                  let time2 = incrementServerTime time1
                  -- The server updates the item accordingly
                  lift $ do
                    resp1
                      `shouldBe` (emptySyncResponse {syncResponseClientChanged = M.singleton uuid time2})
                    sstore2
                      `shouldBe` (ServerStore {serverStoreItems = M.singleton uuid (Timed iA time2)})
                  -- Client A merges the response
                  let cAstore2 = mergeFunc cAstore1 resp1
                  -- Client A has the item from the server because there was no conflict.
                  lift $
                    cAstore2
                      `shouldBe` initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed iA time2)}
                  -- Client B makes sync request 2
                  let req2 = makeSyncRequest cBstore1
                  -- The server processes sync request 2
                  (resp2, sstore3) <- processServerSync genD sstore2 req2
                  -- The server reports a conflict and does not change its store
                  lift $ do
                    resp2
                      `shouldBe` (emptySyncResponse {syncResponseConflicts = M.singleton uuid (Timed iA time2)})
                    sstore3 `shouldBe` sstore2
                  -- Client B merges the response
                  let cBstore2 = mergeFunc cBstore1 resp2
                  lift $ do
                    let expected = case itemMergeStrategyMergeChangeConflict strat iB iA of
                          KeepLocal -> initialClientStore {clientStoreSyncedButChangedItems = M.singleton uuid (Timed iB time1)}
                          TakeRemote -> initialClientStore {clientStoreSyncedItems = M.singleton uuid (Timed iA time2)}
                          Merged im -> initialClientStore {clientStoreSyncedButChangedItems = M.singleton uuid (Timed im time2)}
                    cBstore2
                      `shouldBe` expected
                  -- In case of a previous merge, the synced item will still be changed, so we need to sync again with B and then with A
                  let req3 = makeSyncRequest cBstore2
                  (resp3, sstore4) <- processServerSync genD sstore3 req3
                  let cBstore3 = mergeFunc cBstore2 resp3
                  let req4 = makeSyncRequest cAstore2
                  (resp4, _) <- processServerSync genD sstore4 req4
                  let cAstore3 = mergeFunc cAstore2 resp4
                  lift $
                    cBstore3 `shouldBe` cAstore3

emptyResponseSpec ::
  forall a.
  (Show a, Eq a, Ord a, GenValid a) =>
  ItemMergeStrategy a ->
  Spec
emptyResponseSpec strat = do
  let mergeFunc = mergeSyncResponseUsingStrategy strat
  it "is returns an empty response on the second sync with no modifications" $
    forAllValid $
      \cstore1 ->
        forAllValid $ \sstore1 ->
          evalDM $ do
            let req1 = makeSyncRequest @ClientId @UUID cstore1
            (resp1, sstore2) <- processServerSync genD sstore1 req1
            let cstore2 = mergeFunc cstore1 resp1
                req2 = makeSyncRequest cstore2
            (resp2, _) <- processServerSync genD sstore2 req2
            lift $ resp2 `shouldBe` emptySyncResponse

noDifferentExceptForConflicts ::
  forall a.
  (Show a, Eq a, Ord a, GenValid a) =>
  ItemMergeStrategy a ->
  ItemMergeStrategy a ->
  Spec
noDifferentExceptForConflicts strat1 strat2 = do
  let mergeFunc1 = mergeSyncResponseUsingStrategy strat1
      mergeFunc2 = mergeSyncResponseUsingStrategy strat2
  describe "mergeSyncResponseFromServer" $
    it "only differs from mergeSyncResponseFromClient on conflicts" $
      forAllValid $
        \cstore ->
          forAllValid $ \sresp@SyncResponse {..} -> do
            let cstoreA = mergeFunc1 (cstore :: ClientStore ClientId UUID a) sresp
                cstoreB = mergeFunc2 cstore sresp
            if cstoreA == cstoreB
              then pure ()
              else
                unless
                  ( or
                      [ not (M.null syncResponseConflicts),
                        not (M.null syncResponseConflictsClientDeleted),
                        not (S.null syncResponseConflictsServerDeleted)
                      ]
                  )
                  $ expectationFailure
                  $ unlines
                    [ "There was a difference between mergeFunc1 and mergeFunc2 that was somehow unrelated to the conflicts:",
                      "syncResponseConflicts:",
                      ppShow syncResponseConflicts,
                      "syncResponseConflictsClientDeleted:",
                      ppShow syncResponseConflictsClientDeleted,
                      "syncResponseConflictsServerDeleted:",
                      ppShow syncResponseConflictsServerDeleted,
                      "client store after mergeFunc1:",
                      ppShow cstoreA,
                      "client store after mergeFunc2:",
                      ppShow cstoreB
                    ]

newtype D m a = D
  { unD :: StateT StdGen m a
  }
  deriving (Generic, Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO)

evalD :: D Identity a -> a
evalD = runIdentity . evalDM

-- runD :: D Identity a -> StdGen -> (a, StdGen)
-- runD = runState . unD
evalDM :: Functor m => D m a -> m a
evalDM d = fst <$> runDM d (mkStdGen 42)

runDM :: D m a -> StdGen -> m (a, StdGen)
runDM = runStateT . unD

genD :: Monad m => D m UUID
genD = do
  r <- get
  let (u, r') = random r
  put r'
  pure u
