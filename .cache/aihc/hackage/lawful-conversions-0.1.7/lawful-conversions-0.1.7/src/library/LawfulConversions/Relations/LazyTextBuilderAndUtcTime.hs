{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextBuilderAndUtcTime where

import qualified Data.Text.Lazy.Builder
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.LazyTextBuilderAndString ()
import LawfulConversions.Relations.StringAndUtcTime ()

-- | Implements ISO-8601.
instance IsSome Data.Text.Lazy.Builder.Builder UTCTime where
  to = fromString . to
  maybeFrom = maybeFrom @String . to
