{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.DayAndStrictTextBuilder where

#if MIN_VERSION_text(2,1,2)

import qualified Data.Text.Encoding
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.StrictTextBuilderAndString ()
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Relations.DayAndString ()

-- | Implements ISO-8601.
instance IsSome Data.Text.Encoding.StrictTextBuilder Day where
  to = from . to @String
  maybeFrom = maybeFrom @String . to

#elif MIN_VERSION_text(2,0,2)

import qualified Data.Text.Encoding
import Data.Time
import LawfulConversions.Classes
import LawfulConversions.Prelude
import LawfulConversions.Relations.StrictTextBuilderAndString ()
import LawfulConversions.Relations.StringAndText ()
import LawfulConversions.Relations.DayAndString ()

-- | Implements ISO-8601.
instance IsSome Data.Text.Encoding.StrictBuilder Day where
  to = from . to @String
  maybeFrom = maybeFrom @String . to

#endif
