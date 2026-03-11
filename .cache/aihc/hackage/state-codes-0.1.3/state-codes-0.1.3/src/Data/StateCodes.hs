-- | This module defines codes for the names of the principal subdivisions for
-- the United States according to ISO 3166-2:US.

module Data.StateCodes
  ( StateCode(..)
  , allNames
  , allStates
  , stateList
  , districtList
  , outlyingAreasList
  , fromMName
  , fromMText
  , fromName
  , fromText
  , toName
  , toText
  ) where

import           Control.Arrow              ((&&&))
import           Data.Text                  (Text)

import           Data.StateCodes.ISO31662US

-- | List all codes with names

allNames :: [(StateCode, Text)]
allNames = map (id &&& toName) $ enumFrom minBound


-- | List all states with codes. This is ready to be used in a yesod
-- selectField, for example

allStates :: [(Text, StateCode)]
allStates = map (toName &&& id) $ enumFrom minBound
