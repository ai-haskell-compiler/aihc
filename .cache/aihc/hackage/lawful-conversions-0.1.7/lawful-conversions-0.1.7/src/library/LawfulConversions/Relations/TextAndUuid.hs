{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.TextAndUuid where

import qualified Data.UUID.Types as Uuid
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Text UUID where
  to = Uuid.toText
  maybeFrom = Uuid.fromText
