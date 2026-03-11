{-# LANGUAGE OverloadedStrings #-}

-- | Some standard replacements for text
module Tophat.Text
  (
    literalNewlines,
    stripNewlines,
    stripAroundNewlines
  ) where

import Data.Text (Text)
import qualified Data.Text as T


-- | Replace "\n" with an actual newline
literalNewlines :: Text -> Text
literalNewlines = T.replace "\\n" "\n"

-- | Remove newlines
stripNewlines :: Text -> Text
stripNewlines = T.replace "\n" ""

-- | Remove newlines, and whitespace around them
stripAroundNewlines :: Text -> Text
stripAroundNewlines = T.concat . fmap T.strip . T.lines
