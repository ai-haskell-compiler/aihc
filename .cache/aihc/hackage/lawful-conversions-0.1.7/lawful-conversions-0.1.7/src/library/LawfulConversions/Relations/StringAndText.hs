{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StringAndText where

import qualified Data.Text as Text
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome String Text where
  to = Text.unpack
  maybeFrom string =
    -- FIXME: Optimize.
    let text = Text.pack string
     in if string == Text.unpack text
          then Just text
          else Nothing

instance IsMany String Text where
  from = Text.pack
