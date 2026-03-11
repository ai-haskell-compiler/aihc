{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Appendful.Collection where

import Control.Monad
import Data.Appendful
import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Time ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Test.QuickCheck

instance GenValid ClientId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid ci, GenValid si, GenValid a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => GenValid (ClientStore ci si a) where
  genValid =
    sized $ \n -> do
      (a, b) <- genSplit n
      s <- resize a genValid
      clientStoreAdded <- resize b genValid
      clientStoreSynced <- mapWithIds s
      pure ClientStore {..}
  shrinkValid = shrinkValidStructurally

instance (GenValid ci, GenValid si, GenValid a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => GenValid (SyncRequest ci si a) where
  genValid = do
    syncRequestAdded <- genValid
    syncRequestMaximumSynced <- genValid
    pure SyncRequest {..}
  shrinkValid = shrinkValidStructurally

instance (GenValid ci, GenValid si, GenValid a, Show ci, Show si, Show a, Ord ci, Ord si, Ord a) => GenValid (SyncResponse ci si a) where
  genValid = do
    (s1, s2) <- genValid >>= splitSet
    syncResponseClientAdded <-
      fmap M.fromList $
        forM (S.toList s1) $
          \i -> do
            cid <- genValid -- TODO maybe we can find a way to not generate duplicate client ids and speed up this generator, but it's fine for now.
            pure (cid, i)
    syncResponseServerAdded <- mapWithIds s2
    pure SyncResponse {..}
  shrinkValid = shrinkValidStructurally

splitSet :: Ord i => Set i -> Gen (Set i, Set i)
splitSet s =
  if S.null s
    then pure (S.empty, S.empty)
    else do
      a <- elements $ S.toList s
      pure $ S.split a s

mapWithIds :: (Ord i, GenValid a) => Set i -> Gen (Map i a)
mapWithIds = sequenceA . M.fromSet (const genValid)

instance (GenValid si, GenValid a, Show si, Show a, Ord si, Ord a) => GenValid (ServerStore si a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

genServerStoreFromSet :: (Ord si, GenValid v) => Set si -> Gen (ServerStore si v)
genServerStoreFromSet s = ServerStore <$> mapWithIds s

genUnsyncedStore ::
  forall ci si a.
  (Show ci, Ord ci, Ord si, Ord a, GenValid ci, GenValid si, GenValid a) =>
  Gen (ClientStore ci si a)
genUnsyncedStore = do
  as <- genValid
  pure $ emptyClientStore {clientStoreAdded = as}

genClientStoreFromSet :: (Show ci, Ord ci, Ord si, GenValid ci, GenValid v) => Set si -> Gen (ClientStore ci si v)
genClientStoreFromSet s = do
  clientStoreAdded <- genValid
  clientStoreSynced <- mapWithIds s
  pure ClientStore {..}
