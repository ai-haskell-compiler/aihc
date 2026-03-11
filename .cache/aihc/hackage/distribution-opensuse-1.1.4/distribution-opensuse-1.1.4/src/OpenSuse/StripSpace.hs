{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenSuse.StripSpace ( stripSpace ) where

import OpenSuse.Prelude

import qualified Data.Text as Text

-- | A (quite possibly inefficient) re-implementation of @git stripspace@. This
-- function normalizes a 'Text' buffer to conform to the following rules:
--
-- * All trailing white space is stripped.
--
-- * Empty lines at the beginning or at the end of the buffer are stripped.
--
-- * Consecutive empty lines between paragraphs are collapsed into one.
--
-- * @\\r\\n@ line endings are normalized into @\\n@.
--
-- * If the buffer is not empty, then its last line is terminated by @\\n@.
--
-- * If the buffer is empty (i.e. it contains only white space), then it comes
--   out as the empty string.

stripSpace :: Text -> Text
stripSpace = Text.unlines
           . normalizeEndOfText
           . normalizeEmptyLines Skip
           . map Text.stripEnd
           . Text.lines

data Mode = Skip | Keep

normalizeEmptyLines :: Mode -> [Text] -> [Text]
normalizeEmptyLines  _     []    = []
normalizeEmptyLines Skip ("":ls) = normalizeEmptyLines Skip ls
normalizeEmptyLines Keep ("":ls) = "" : normalizeEmptyLines Skip ls
normalizeEmptyLines _    (l:ls)  = l  : normalizeEmptyLines Keep ls

normalizeEndOfText :: [Text] -> [Text]
normalizeEndOfText = reverse . dropWhile Text.null . reverse
