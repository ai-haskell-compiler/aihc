{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextAndUuid where

import qualified Data.Text.Lazy
import qualified Data.UUID.Types
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Lazy.Text UUID where
  to = Data.Text.Lazy.fromStrict . Data.UUID.Types.toText
  maybeFrom = Data.UUID.Types.fromText . Data.Text.Lazy.toStrict
