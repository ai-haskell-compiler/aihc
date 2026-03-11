{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Various <https://hackage.haskell.org/package/flatparse flatparse> helpers and combinators.
--
-- This module is exposed only for testing via doctest-parallel and is not intended to form part of the stable API.
module MarkupParse.Internal.FlatParse
  ( -- * Parsers
    isWhitespace,
    ws_,
    ws,
    nota,
    isa,
    sq,
    dq,
    wrappedDq,
    wrappedSq,
    wrappedQ,
    wrappedQNoGuard,
    eq,
    bracketed,
    bracketedSB,
    wrapped,
  )
where

import Data.Bool
import Data.ByteString (ByteString)
import Data.Char hiding (isDigit)
import FlatParse.Basic hiding (cut, take)
import Prelude hiding (replicate)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XQuasiQuotes
-- >>> :set -XOverloadedStrings
-- >>> import MarkupParse.Internal.FlatParse
-- >>> import FlatParse.Basic

-- | Consume whitespace.
--
-- >>> runParser ws_ " \nx"
-- OK () "x"
--
-- >>> runParser ws_ "x"
-- OK () "x"
ws_ :: Parser e ()
ws_ =
  $( switch
       [|
         case _ of
           " " -> ws_
           "\n" -> ws_
           "\t" -> ws_
           "\r" -> ws_
           "\f" -> ws_
           _ -> pure ()
         |]
   )
{-# INLINE ws_ #-}

-- | \\n \\t \\f \\r and space
isWhitespace :: Char -> Bool
isWhitespace ' ' = True -- \x20 space
isWhitespace '\x0a' = True -- \n linefeed
isWhitespace '\x09' = True -- \t tab
isWhitespace '\x0c' = True -- \f formfeed
isWhitespace '\x0d' = True -- \r carriage return
isWhitespace _ = False
{-# INLINE isWhitespace #-}

-- | single whitespace
--
-- >>> runParser ws " \nx"
-- OK ' ' "\nx"
ws :: Parser e Char
ws = satisfy isWhitespace

-- | Single quote
--
-- >>> runParser sq "'"
-- OK () ""
sq :: ParserT st e ()
sq = $(char '\'')

-- | Double quote
--
-- >>> runParser dq "\""
-- OK () ""
dq :: ParserT st e ()
dq = $(char '"')

-- | Parse whilst not a specific character
--
-- >>> runParser (nota 'x') "abcxyz"
-- OK "abc" "xyz"
nota :: Char -> Parser e ByteString
nota c = withSpan (skipMany (satisfy (/= c))) (\() s -> unsafeSpanToByteString s)
{-# INLINE nota #-}

-- | Parse whilst satisfying a predicate.
--
-- >>> runParser (isa (=='x')) "xxxabc"
-- OK "xxx" "abc"
isa :: (Char -> Bool) -> Parser e ByteString
isa p = withSpan (skipMany (satisfy p)) (\() s -> unsafeSpanToByteString s)
{-# INLINE isa #-}

-- | A single-quoted string.
wrappedSq :: Parser b ByteString
wrappedSq = $(char '\'') *> nota '\'' <* $(char '\'')
{-# INLINE wrappedSq #-}

-- | A double-quoted string.
wrappedDq :: Parser b ByteString
wrappedDq = $(char '"') *> nota '"' <* $(char '"')
{-# INLINE wrappedDq #-}

-- | A single-quoted or double-quoted string.
--
-- >>> runParser wrappedQ "\"quoted\""
-- OK "quoted" ""
--
-- >>> runParser wrappedQ "'quoted'"
-- OK "quoted" ""
wrappedQ :: Parser e ByteString
wrappedQ =
  wrappedDq
    <|> wrappedSq
{-# INLINE wrappedQ #-}

-- | A single-quoted or double-quoted wrapped parser.
--
-- >>> runParser (wrappedQNoGuard (many $ satisfy (/= '"'))) "\"name\""
-- OK "name" ""
--
-- Will consume quotes if the underlying parser does.
--
-- >>> runParser (wrappedQNoGuard (many anyChar)) "\"name\""
-- Fail
wrappedQNoGuard :: Parser e a -> Parser e a
wrappedQNoGuard p = wrapped dq p <|> wrapped sq p

-- | xml production [25]
--
-- >>> runParser eq " = "
-- OK () ""
--
-- >>> runParser eq "="
-- OK () ""
eq :: Parser e ()
eq = ws_ *> $(char '=') <* ws_
{-# INLINE eq #-}

-- | Parser bracketed by two other parsers.
--
-- >>> runParser (bracketed ($(char '[')) ($(char ']')) (many (satisfy (/= ']')))) "[bracketed]"
-- OK "bracketed" ""
bracketed :: Parser e b -> Parser e b -> Parser e a -> Parser e a
bracketed o c p = o *> p <* c
{-# INLINE bracketed #-}

-- | Parser bracketed by square brackets.
--
-- >>> runParser bracketedSB "[bracketed]"
-- OK "bracketed" ""
bracketedSB :: Parser e [Char]
bracketedSB = bracketed $(char '[') $(char ']') (many (satisfy (/= ']')))

-- | Parser wrapped by another parser.
--
-- >>> runParser (wrapped ($(char '"')) (many (satisfy (/= '"')))) "\"wrapped\""
-- OK "wrapped" ""
wrapped :: Parser e () -> Parser e a -> Parser e a
wrapped x p = bracketed x x p
{-# INLINE wrapped #-}
