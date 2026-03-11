{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Declares the abstract @'Handle'@ and combinators used to load and save keyed values.
-}
module KeyedVals.Handle (
  -- * 'Handle' and related types and functions
  Handle (),
  HandleErr (..),
  countKVs,
  loadVal,
  saveVal,
  loadKVs,
  saveKVs,
  updateKVs,
  loadSlice,
  loadFrom,
  saveTo,
  deleteKeys,
  deleteKeysFrom,
  deleteMatches,
  deleteMatchesFrom,
  deleteSelected,
  deleteSelectedFrom,
  close,

  -- * 'Selection' and 'Glob'
  Selection (..),
  Glob,
  mkGlob,
  globPattern,
  isIn,

  -- * aliases used by the 'Handle' functions
  Key,
  Val,
  ValsByKey,
) where

import Data.List.NonEmpty (NonEmpty)
import KeyedVals.Handle.Internal
import Numeric.Natural (Natural)


-- | Loads the saved 'Val' corresponding to a 'Key'.
loadVal :: Handle m -> Key -> m (Either HandleErr (Maybe Val))
loadVal = hLoadVal


-- | Saves a 'Val' for 'Key'.
saveVal :: Handle m -> Key -> Val -> m (Either HandleErr ())
saveVal = hSaveVal


-- | Loads a @'ValsByKey'@.
loadKVs :: Handle m -> Key -> m (Either HandleErr ValsByKey)
loadKVs = hLoadKVs


-- | Saves a @'ValsByKey'@ .
saveKVs :: Handle m -> Key -> ValsByKey -> m (Either HandleErr ())
saveKVs = hSaveKVs


-- | Loads a @'Val'@ from a @'ValsByKey'@.
loadFrom :: Handle m -> Key -> Key -> m (Either HandleErr (Maybe Val))
loadFrom = hLoadFrom


-- | Saves a @'Val'@ in a @'ValsByKey'@.
saveTo :: Handle m -> Key -> Key -> Val -> m (Either HandleErr ())
saveTo = hSaveTo


-- | Loads a @'ValsByKey'@ that only includes @'Val's@ whose keys match a @Selection@.
loadSlice :: Handle m -> Key -> Selection -> m (Either HandleErr ValsByKey)
loadSlice = hLoadSlice


-- | Updates the stored @'ValsByKey'@ from the given @'ValsByKey'@.
updateKVs :: Handle m -> Key -> ValsByKey -> m (Either HandleErr ())
updateKVs = hUpdateKVs


-- | Deletes @'Val's@ stored with the given @'Key's@.
deleteKeys :: Handle m -> NonEmpty Key -> m (Either HandleErr ())
deleteKeys h keys = deleteSelected h $ AllOf keys


-- | Deletes @'Val's@ that match a @'Selection'@.
deleteSelected :: Handle m -> Selection -> m (Either HandleErr ())
deleteSelected = hDeleteSelected


-- | Deletes @'Val's@ whose @Keys@ match a @Glob@.
deleteMatches :: Handle m -> Glob -> m (Either HandleErr ())
deleteMatches h g = deleteSelected h $ Match g


-- | Deletes @'Val's@ for the given @Keys@ from a @'ValsByKey'@.
deleteKeysFrom :: Handle m -> Key -> NonEmpty Key -> m (Either HandleErr ())
deleteKeysFrom h key ks = deleteSelectedFrom h key $ AllOf ks


-- | Deletes @'Val's@ whose @Keys@  match a @Selection@ from a @'ValsByKey'@
deleteSelectedFrom :: Handle m -> Key -> Selection -> m (Either HandleErr ())
deleteSelectedFrom = hDeleteSelectedKVs


-- | Deletes @'Val's@ whose @Keys@  match a @Glob@ from a @'ValsByKey'@
deleteMatchesFrom :: Handle m -> Key -> Glob -> m (Either HandleErr ())
deleteMatchesFrom h key g = deleteSelectedFrom h key $ Match g


-- | Determines the number of @'Val's@ in a @'ValsByKey'@.
countKVs :: Handle m -> Key -> m (Either HandleErr Natural)
countKVs = hCountKVs


close :: Handle m -> m ()
close = hClose
