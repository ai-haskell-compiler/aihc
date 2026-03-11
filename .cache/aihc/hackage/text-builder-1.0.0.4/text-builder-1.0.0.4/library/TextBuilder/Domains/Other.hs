module TextBuilder.Domains.Other where

import qualified Data.Text as Text
import TextBuilder.Prelude
import TextBuilderCore

-- * Destructors

-- | Convert builder to string.
{-# INLINE toString #-}
toString :: TextBuilder -> String
toString = Text.unpack . toText
