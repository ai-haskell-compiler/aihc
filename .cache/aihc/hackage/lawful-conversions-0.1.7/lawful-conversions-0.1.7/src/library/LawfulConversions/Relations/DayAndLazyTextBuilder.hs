{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndLazyTextBuilder where

import qualified Data.Text.Lazy.Builder
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.DayAndString ()
import LawfulConversions.Relations.LazyTextBuilderAndString ()

-- | Implements ISO-8601.
instance IsSome Data.Text.Lazy.Builder.Builder Day where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
