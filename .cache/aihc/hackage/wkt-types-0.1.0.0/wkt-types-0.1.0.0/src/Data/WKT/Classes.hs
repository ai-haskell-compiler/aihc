module Data.WKT.Classes (module Data.WKT.Classes) where
import Data.Attoparsec.Text (Parser, parseOnly)
import Data.Text (Text)

-- |Used to check if a given geometry is valid.
class Valid a where
    isValid :: a -> Bool

-- |Used to convert a geometry to WKT.
class ToWKT a where
    toWKT :: a -> Text

-- |Used to convert a WKT `Text` to a geometry.
class ParseableFromWKT a => FromWKT a where
    fromWKT :: Text -> a Double
    fromWKT = either (error . show) id . parseOnly wktParser

-- |WKT Attoparsec `Parser` for a given geometry. Always parses to `Double` for simplicity.
class ParseableFromWKT a where
    wktParser :: Parser (a Double)

