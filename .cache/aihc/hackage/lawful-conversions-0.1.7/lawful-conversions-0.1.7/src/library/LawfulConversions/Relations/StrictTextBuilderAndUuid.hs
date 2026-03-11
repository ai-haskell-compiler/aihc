{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.StrictTextBuilderAndUuid where

#if MIN_VERSION_text(2,1,2)

import qualified Data.UUID.Types
import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Encoding.StrictTextBuilder UUID where
  to = Data.Text.Encoding.textToStrictBuilder . Data.UUID.Types.toText
  maybeFrom = Data.UUID.Types.fromText . Data.Text.Encoding.strictBuilderToText

#elif MIN_VERSION_text(2,0,2)

import qualified Data.UUID.Types
import qualified Data.Text.Encoding
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome Data.Text.Encoding.StrictBuilder UUID where
  to = Data.Text.Encoding.textToStrictBuilder . Data.UUID.Types.toText
  maybeFrom = Data.UUID.Types.fromText . Data.Text.Encoding.strictBuilderToText

#endif
