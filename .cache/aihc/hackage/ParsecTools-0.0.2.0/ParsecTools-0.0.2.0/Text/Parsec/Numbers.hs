
-- | Parsing of numbers.

module Text.Parsec.Numbers where

import Numeric (readSigned, readFloat)
import Text.Parsec hiding ((<|>))
import Text.Parsec.String
import Control.Applicative


-- | Parse a double value. This is exactly the same code as in Real World
-- Haskell, p. 400.
--
-- TODO There are some strange 'floating point numbers' running around in the
-- wild that can not be parsed using this code. (eg.: +.5) or (+0.5)

parseFloat :: GenParser Char st Double
parseFloat = do
  s <- getInput
  case readSigned readFloat s of
    [(n,s')]  -> n <$ setInput s'
    _         -> empty



-- | This parser should capture floating point numbers beginning with a '+'.

parseExtFloat :: GenParser Char st Double
parseExtFloat = (char '+' <|> pure ' ') *> parseFloat



-- | Parse an integral value.

parseIntegral :: (Integral a, Read a) => GenParser Char st a
parseIntegral = read <$> ((:) <$> parseSignum <*> many1 digit)



-- | Parse the potential +/- before a number, returning ' ' for a '+'

parseSignum :: GenParser Char st Char
parseSignum = (' ' <$ char '+') <|> char '-' <|> pure ' '
