{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndText where

import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.DayAndString ()
import LawfulConversions.Relations.StringAndText ()

-- | Implements ISO-8601.
instance IsSome Text Day where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
