{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Data.Aeson.JSONPath.Parser.Name
  ( pSingleQuotted
  , pDoubleQuotted
  )
  where

import qualified Text.ParserCombinators.Parsec  as P

import Data.Functor                  (($>))
import Data.Char                     (ord, chr)
import Text.ParserCombinators.Parsec ((<|>))

import Prelude

pSingleQuotted :: P.Parser String
pSingleQuotted = P.char '\'' *> P.many inQuote <* P.char '\''
  where
    inQuote = P.try pUnescaped
           <|> P.try (P.char '\"')
           <|> P.try (P.string "\\\'" $> '\'')
           <|> P.try pEscaped

pDoubleQuotted :: P.Parser String
pDoubleQuotted = P.char '\"' *> P.many inQuote <* P.char '\"'
  where
    inQuote = P.try pUnescaped
           <|> P.try (P.char '\'')
           <|> P.try (P.string "\\\"" $> '\"')
           <|> P.try pEscaped

pUnescaped :: P.Parser Char
pUnescaped = P.satisfy inRange
  where
    inRange c = let code = ord c in
      (code >= 0x20 && code <= 0x21) ||
      (code >= 0x23 && code <= 0x26) ||
      (code >= 0x28 && code <= 0x5B) ||
      (code >= 0x5D && code <= 0xD7FF) ||
      (code >= 0xE000 && code <= 0x10FFFF)

pEscaped :: P.Parser Char
pEscaped = do
  P.char '\\'
  P.try pEscapees <|> P.try pHexUnicode
  where
    pEscapees = P.try (P.char 'b' $> '\b')
            <|> P.try (P.char 'f' $> '\f')
            <|> P.try (P.char 'n' $> '\n')
            <|> P.try (P.char 'r' $> '\r')
            <|> P.try (P.char 't' $> '\t')
            <|> P.try (P.char '/')
            <|> P.try (P.char '\\')

pHexUnicode :: P.Parser Char
pHexUnicode = P.try pNonSurrogate <|> P.try pSurrogatePair
  where
    pNonSurrogate = P.try pNonSurrogateFirst <|> P.try pNonSurrogateSecond
      where
        pNonSurrogateFirst = do
          P.char 'u'
          c1 <- P.digit <|> P.oneOf "AaBbCcEeFf"
          c2 <- P.hexDigit
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return $ chr (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

        pNonSurrogateSecond = do
          P.char 'u'
          c1 <- P.oneOf "Dd"
          c2 <- P.oneOf "01234567"
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return $ chr (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

    pSurrogatePair = do
      P.char 'u'
      high <- pHighSurrogate
      P.char '\\'
      P.char 'u'
      fromHighAndLow high <$> pLowSurrogate
      where
        pHighSurrogate = do
          c1 <- P.oneOf "Dd"
          c2 <- P.oneOf "89AaBb"
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

        pLowSurrogate = do
          c1 <- P.oneOf "Dd"
          c2 <- P.oneOf "CcDdEeFf"
          c3 <- P.hexDigit
          c4 <- P.hexDigit
          return (read ("0x" ++ [c1,c2,c3,c4]) :: Int)

        fromHighAndLow hi lo = chr $ ((hi - 0xD800) * 0x400) + (lo - 0xDC00) + 0x10000
