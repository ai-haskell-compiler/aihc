{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndLazyText where

import qualified Data.Text.Lazy
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.DayAndString ()
import LawfulConversions.Relations.LazyTextAndString ()

-- | Implements ISO-8601.
instance IsSome Data.Text.Lazy.Text Day where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
