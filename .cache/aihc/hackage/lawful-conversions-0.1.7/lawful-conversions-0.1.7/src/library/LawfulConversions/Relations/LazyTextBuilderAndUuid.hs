{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.LazyTextBuilderAndUuid where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.UUID.Types
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Lazy.Builder.Builder UUID where
  to = Data.Text.Lazy.Builder.fromText . Data.UUID.Types.toText
  maybeFrom = Data.UUID.Types.fromText . Data.Text.Lazy.toStrict . Data.Text.Lazy.Builder.toLazyText
