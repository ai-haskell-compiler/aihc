-- |
-- Utilities, which execute the parser.
module HTMLEntities.Decoder where

import qualified Data.Attoparsec.Text as P
import qualified Data.Text.Lazy.Builder as TLB
import qualified HTMLEntities.Parser as P
import HTMLEntities.Prelude

-- |
-- A decoder of a single entity.
--
-- >>> mapM_ Data.Text.IO.putStrLn $ htmlEntity "&#169;"
-- ©
htmlEntity :: Text -> Either String Text
htmlEntity =
  P.parseOnly
    $ P.htmlEntity
    <* P.endOfInput

-- |
-- A decoder of a single entity.
--
-- >>> mapM_ Data.Text.IO.putStrLn $ htmlEntityBody "#169"
-- ©
htmlEntityBody :: Text -> Either String Text
htmlEntityBody =
  P.parseOnly
    $ P.htmlEntityBody
    <* P.endOfInput

-- |
-- A decoder of a text with entities.
--
-- Produces a text builder,
-- which you can then convert into a text or a lazy text,
-- using the <http://hackage.haskell.org/package/text "text"> or
-- <http://hackage.haskell.org/package/conversion-text "conversion-text"> library.
--
-- >>> Data.Text.Lazy.IO.putStrLn $ Data.Text.Lazy.Builder.toLazyText $ htmlEncodedText "&euro;5 &cent;2"
-- €5 ¢2
htmlEncodedText :: Text -> TLB.Builder
htmlEncodedText =
  fmap (either (error "HTMLEntities.Decoder: impossible happened") id)
    $ P.parseOnly
    $ fmap fold
    $ many
    $ fmap TLB.fromText P.htmlEntity
    <|> fmap TLB.singleton P.anyChar
