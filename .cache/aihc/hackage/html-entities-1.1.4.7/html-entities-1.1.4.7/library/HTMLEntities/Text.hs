module HTMLEntities.Text where

import qualified Data.Text as Text
import HTMLEntities.Prelude

-- |
-- HTML-encodes the given char.
char :: Char -> Text
char =
  \case
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    '\'' -> "&#39;"
    x -> Text.singleton x

-- |
-- HTML-encodes the given text.
--
-- >>> Data.Text.IO.putStrLn $ text "<a href=\"\">"
-- &lt;a href=&quot;&quot;&gt;
text :: Text -> Text
text =
  Text.concatMap char
