{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.TextAndUtcTime where

import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Relations.StringAndUtcTime ()

-- | Implements ISO-8601.
instance IsSome Text UTCTime where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
