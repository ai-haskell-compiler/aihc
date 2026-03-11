module XmlParser.Attoparsec where

import Data.Attoparsec.Text
import qualified Data.Text as Text
import XmlParser.Prelude hiding (takeWhile)

parseStripped :: Parser a -> Text -> Either Text a
parseStripped p = first fromString . parseOnly (stripped p)
  where
    stripped :: Parser a -> Parser a
    stripped p = skipSpace *> p <* skipSpace <* endOfInput

qName :: Parser (Maybe Text, Text)
qName =
  {-
  Ref (from https://en.wikipedia.org/wiki/QName):

      QName            ::=   PrefixedName | UnprefixedName
      PrefixedName     ::=   Prefix ':' LocalPart
      UnprefixedName   ::=   LocalPart
      Prefix           ::=   NCName
      LocalPart        ::=   NCName
  -}
  do
    a <- ncName
    asum
      [ do
          char ':'
          b <- ncName
          return (Just a, b),
        return (Nothing, a)
      ]

{-# NOINLINE ncName #-}
ncName :: Parser Text
ncName =
  {-
  Ref (from https://en.wikipedia.org/wiki/QName):

      NCName           ::=   Name - (Char* ':' Char*)  (* An XML Name, minus the ":" *)
      Name             ::=   NameStartChar (NameChar)*
      NameStartChar    ::=   ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6] | [#xD8-#xF6]
                             | [#xF8-#x2FF] | [#x370-#x37D] | [#x37F-#x1FFF]
                             | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF]
                             | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
                             | [#x10000-#xEFFFF]
      NameChar         ::=   NameStartChar | "-" | "." | [0-9]
                             | #xB7 | [#x0300-#x036F] | [#x203F-#x2040]
      Char             ::=   (* any Unicode char, excluding surrogate blocks FFFE and FFFF. *)
                             #x9 | #xA | #xD | [#x20-#xD7FF]
                             | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
  -}
  do
    a <- satisfy nameStartCharPredicate
    b <- takeWhile nameCharPredicate
    return (Text.cons a b)
  where
    nameStartCharPredicate x =
      or
        [ x >= 'A' && x <= 'Z',
          x == '_',
          x >= 'a' && x <= 'z',
          x >= '\xC0' && x <= '\xD6',
          x >= '\xD8' && x <= '\xF6',
          x >= '\xF8' && x <= '\x2FF',
          x >= '\x370' && x <= '\x37D',
          x >= '\x37F' && x <= '\x1FFF',
          x >= '\x200C' && x <= '\x200D',
          x >= '\x2070' && x <= '\x218F',
          x >= '\x2C00' && x <= '\x2FEF',
          x >= '\x3001' && x <= '\xD7FF',
          x >= '\xF900' && x <= '\xFDCF',
          x >= '\xFDF0' && x <= '\xFFFD',
          x >= '\x10000' && x <= '\xEFFFF'
        ]
    nameCharPredicate x =
      or
        [ x == '-',
          x == '.',
          x >= '0' && x <= '9',
          x == '\xB7',
          x >= '\x0300' && x <= '\x036F',
          x >= '\x203F' && x <= '\x2040',
          nameStartCharPredicate x
        ]
