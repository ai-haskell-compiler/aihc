{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.Mergeful.Collection where

import Data.GenValidity
import Data.GenValidity.Containers ()
import Data.GenValidity.Mergeful.Item ()
import Data.GenValidity.Time ()
import Data.Map (Map)
import qualified Data.Map as M
import Data.Mergeful
import Data.Set (Set)
import qualified Data.Set as S
import Data.Traversable
import Test.QuickCheck

instance GenValid ClientId where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance
  (GenValid ci, GenValid si, Show ci, Show si, Ord ci, Ord si, GenValid a) =>
  GenValid (ClientStore ci si a)
  where
  genValid =
    (`suchThat` isValid) $ do
      identifiers <- genValid
      (s1, s2) <- splitSet identifiers
      (s3, s4) <- splitSet s1
      clientStoreAddedItems <- genValid
      clientStoreSyncedItems <- mapWithIds s2
      clientStoreSyncedButChangedItems <- mapWithIds s3
      clientStoreDeletedItems <- mapWithIds s4
      pure ClientStore {..}
  shrinkValid = shrinkValidStructurally

instance (GenValid si, Show si, Ord si, GenValid a) => GenValid (ServerStore si a) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance
  (GenValid ci, GenValid si, Show ci, Show si, Ord ci, Ord si, GenValid a) =>
  GenValid (SyncRequest ci si a)
  where
  genValid =
    (`suchThat` isValid) $ do
      identifiers <- genValid
      (s1, s2) <- splitSet identifiers
      (s3, s4) <- splitSet s1
      syncRequestNewItems <- genValid
      syncRequestKnownItems <- mapWithIds s2
      syncRequestKnownButChangedItems <- mapWithIds s3
      syncRequestDeletedItems <- mapWithIds s4
      pure SyncRequest {..}
  shrinkValid = shrinkValidStructurally

instance GenValid si => GenValid (ClientAddition si) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance
  (GenValid ci, GenValid si, Show ci, Show si, Ord ci, Ord si, GenValid a) =>
  GenValid (SyncResponse ci si a)
  where
  genValid =
    (`suchThat` isValid) $ do
      identifiers <- scale (* 2) genValid
      (s01, s02) <- splitSet identifiers
      (s03, s04) <- splitSet s01
      (s05, s06) <- splitSet s02
      (s07, s08) <- splitSet s03
      (s09, s10) <- splitSet s04
      (s11, s12) <- splitSet s05
      (s13, s14) <- splitSet s06
      (s15, s16) <- splitSet s07
      syncResponseClientAdded <-
        do
          m <- genValid :: Gen (Map ci ())
          if S.null s08
            then pure M.empty
            else forM m $ \() -> ClientAddition <$> elements (S.toList s08) <*> genValid
      syncResponseClientChanged <- mapWithIds s09
      let syncResponseClientDeleted = s10
      syncResponseServerAdded <- mapWithIds s11
      syncResponseServerChanged <- mapWithIds s12
      let syncResponseServerDeleted = s13
      syncResponseConflicts <- mapWithIds s14
      syncResponseConflictsClientDeleted <- mapWithIds s15
      let syncResponseConflictsServerDeleted = s16
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
mapWithIds s = fmap M.fromList $ for (S.toList s) $ \i -> (,) i <$> genValid
