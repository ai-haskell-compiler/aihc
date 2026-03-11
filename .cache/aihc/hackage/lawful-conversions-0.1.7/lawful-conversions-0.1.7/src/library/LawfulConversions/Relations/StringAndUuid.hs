{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StringAndUuid where

import qualified Data.UUID.Types
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome String UUID where
  to = Data.UUID.Types.toString
  maybeFrom = Data.UUID.Types.fromString
