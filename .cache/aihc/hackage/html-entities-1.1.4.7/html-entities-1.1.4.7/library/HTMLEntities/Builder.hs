module HTMLEntities.Builder (char, text) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import HTMLEntities.Prelude

-- |
-- HTML-encodes the given char into a text builder.
char :: Char -> Text.Builder
char c =
  fromMaybe (Text.Builder.singleton c)
    $ lookup c entitiesTable

-- |
-- HTML-encodes the given text into a text builder.
--
-- >>> Data.Text.Lazy.IO.putStrLn $ Data.Text.Lazy.Builder.toLazyText $ text "<a href=\"\">"
-- &lt;a href=&quot;&quot;&gt;
text :: Text -> Text.Builder
text =
  Text.foldr (\c b -> char c <> b) mempty

{-# NOINLINE entitiesTable #-}
entitiesTable :: [(Char, Text.Builder)]
entitiesTable =
  [ ('<', "&lt;"),
    ('>', "&gt;"),
    ('&', "&amp;"),
    ('"', "&quot;"),
    ('\'', "&#39;")
  ]
