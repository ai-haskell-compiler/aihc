{- |
Module      : Data.Time.Zones.DB
Copyright   : (C) 2014 Mihaly Barasz
License     : Apache-2.0, see LICENSE
Maintainer  : Janus Troelsen <ysangkok@gmail.com>
Stability   : experimental
-}

module Data.Time.Zones.Files (
  timeZonePathFromDB,
  ) where

import Paths_tzdata

-- | Return the file path corresponding to a time zone location.
--
-- Note: doesn't check whether the location is valid (ie. the returned
-- path might not exist).
timeZonePathFromDB :: String -> IO FilePath
timeZonePathFromDB tzName =
  -- TODO(klao): this probably won't work on Windows.
  getDataFileName $ tzName ++ ".zone"
