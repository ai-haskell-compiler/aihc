module Data.BCP47.Internal.Parser
  ( complete
  , asciiLetterDigit
  , asciiLetter
  , asciiDigit
  ) where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Char (ord)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, eof, lookAhead, noneOf, satisfy, (<?>))
import Text.Megaparsec.Char (char)

-- | Ensure a subtag extends to the next '-' or end of input
--
-- Used for subtags that can match some prefix of another subtag.
-- For example, a @'Script'@ or @'Region'@ can accidentally be parsed
-- from the prefix of a @'Variant'@
--
-- The alternative would be to use @'notFollowedBy'@ with knowledge of
-- the legal characters in the next valid subtag.
complete :: Parsec Void Text a -> Parsec Void Text a
complete parser =
  parser <* lookAhead (void (char '-') <|> eof <|> void (noneOf tagChars))

tagChars :: String
tagChars = '-' : ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']
{-# NOINLINE tagChars #-}

-- | Parse a single case-insensitive ASCII letter or digit
asciiLetterDigit :: Parsec Void Text Char
asciiLetterDigit = satisfy isAsciiLetterDigit <?> "ascii letter or digit"
 where
  isAsciiLetterDigit c = do
    let code = ord c
    isCodeAsciiUpper code || isCodeAsciiLower code || isCodeAsciiDigit code
{-# INLINE asciiLetterDigit #-}

-- | Parse a single case-insensitive ASCII letter
asciiLetter :: Parsec Void Text Char
asciiLetter = satisfy isAsciiLetter <?> "ascii letter"
 where
  isAsciiLetter c = do
    let code = ord c
    isCodeAsciiUpper code || isCodeAsciiLower code
{-# INLINE asciiLetter #-}

-- | Parse a single ASCII digit
asciiDigit :: Parsec Void Text Char
asciiDigit = satisfy (isCodeAsciiDigit . ord) <?> "ascii digit"
{-# INLINE asciiDigit #-}

isCodeAsciiUpper :: Int -> Bool
isCodeAsciiUpper code = ord 'A' <= code && code <= ord 'Z'
{-# INLINE isCodeAsciiUpper #-}

isCodeAsciiLower :: Int -> Bool
isCodeAsciiLower code = ord 'a' <= code && code <= ord 'z'
{-# INLINE isCodeAsciiLower #-}

isCodeAsciiDigit :: Int -> Bool
isCodeAsciiDigit code = ord '0' <= code && code <= ord '9'
{-# INLINE isCodeAsciiDigit #-}
