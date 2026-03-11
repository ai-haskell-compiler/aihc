{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StringAndUtcTime where

import Data.Time
import Data.Time.Format.ISO8601
import LawfulConversions.Classes
import LawfulConversions.Prelude

-- | Implements ISO-8601.
instance IsSome String UTCTime where
  to = iso8601Show
  maybeFrom = iso8601ParseM
