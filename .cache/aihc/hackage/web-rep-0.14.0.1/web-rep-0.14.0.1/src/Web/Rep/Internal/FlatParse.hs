{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | flatparse parsing helpers
--
-- This module is exposed only for testing via doctest-parallel and is not intended to form part of the stable API.
module Web.Rep.Internal.FlatParse
  ( -- * Parsing
    runParserMaybe,
    runParserEither,
    runParser_,

    -- * Flatparse re-exports
    runParser,
    Parser,
    Result (..),

    -- * Parsers
    isWhitespace,
    ws_,
    ws,
    wss,
    nota,
    isa,
    sq,
    dq,
    wrappedDq,
    wrappedSq,
    wrappedQ,
    wrappedQNoGuard,
    eq,
    sep,
    bracketed,
    bracketedSB,
    wrapped,
    digit,
    digits,
    int,
    double,
    minus,
    signed,
    byteStringOf',
    comma,
  )
where

import Data.Bool
import Data.ByteString (ByteString)
import Data.Char hiding (isDigit)
import FlatParse.Basic hiding (cut, take)
import GHC.Exts
import Prelude hiding (replicate)

-- $setup
-- >>> :set -XTemplateHaskell
-- >>> :set -XOverloadedStrings
-- >>> import Web.Rep.Internal.FlatParse
-- >>> import FlatParse.Basic

-- | Run a Parser, throwing away leftovers. Nothing on 'Fail' or 'Err'.
--
-- >>> runParserMaybe ws "x"
-- Nothing
--
-- >>> runParserMaybe ws " x"
-- Just ' '
runParserMaybe :: Parser e a -> ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

-- | Run a Parser, throwing away leftovers. Returns Left on 'Fail' or 'Err'.
--
-- >>> runParserEither ws " x"
-- Right ' '
runParserEither :: (IsString e) => Parser e a -> ByteString -> Either e a
runParserEither p bs = case runParser p bs of
  Err e -> Left e
  OK a _ -> Right a
  Fail -> Left "uncaught parse error"

---- | Run parser, discards leftovers & throws an error on failure.
--
-- >>> runParser_ ws " "
-- ' '
--
-- >>> runParser_ ws "x"

-- *** Exception: uncaught parse error

-- ...
runParser_ :: Parser String a -> ByteString -> a
runParser_ p bs = case runParser p bs of
  Err e -> error e
  OK a "" -> a
  OK _ _ -> error "leftovers"
  Fail -> error "uncaught parse error"

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

-- | multiple whitespace
--
-- >>> runParser wss " \nx"
-- OK " \n" "x"
--
-- >>> runParser wss "x"
-- Fail
wss :: Parser e ByteString
wss = byteStringOf $ some ws

-- | Single quote
--
-- >>> runParserMaybe sq "'"
-- Just ()
sq :: ParserT st e ()
sq = $(char '\'')

-- | Double quote
--
-- >>> runParserMaybe dq "\""
-- Just ()
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

-- | 'byteStringOf' but using withSpan internally. Doesn't seems faster...
byteStringOf' :: Parser e a -> Parser e ByteString
byteStringOf' p = withSpan p (\_ s -> unsafeSpanToByteString s)
{-# INLINE byteStringOf' #-}

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
-- >>> runParserMaybe wrappedQ "\"quoted\""
-- Just "quoted"
--
-- >>> runParserMaybe wrappedQ "'quoted'"
-- Just "quoted"
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
-- >>> runParserMaybe eq " = "
-- Just ()
--
-- >>> runParserMaybe eq "="
-- Just ()
eq :: Parser e ()
eq = ws_ *> $(char '=') <* ws_
{-# INLINE eq #-}

-- | Some with a separator.
--
-- >>> runParser (sep ws (many (satisfy (/= ' ')))) "a b c"
-- OK ["a","b","c"] ""
sep :: Parser e s -> Parser e a -> Parser e [a]
sep s p = (:) <$> p <*> many (s *> p)

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

-- | A single digit
--
-- >>> runParserMaybe digit "5"
-- Just 5
digit :: Parser e Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | An (unsigned) 'Int' parser
--
-- >>> runParserMaybe int "567"
-- Just 567
int :: Parser e Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser e (Int, Int)
digits = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure (place, n)

-- | A 'Double' parser. uiua does not parse .1 as a double.
--
-- >>> runParser double "1.234x"
-- OK 1.234 "x"
--
-- >>> runParser double "."
-- Fail
--
-- >>> runParser double "123"
-- OK 123.0 ""
--
-- >>> runParser double ".123"
-- Fail
--
-- >>> runParser double "123."
-- OK 123.0 "."
double :: Parser e Double
double = do
  (placel, nl) <- digits
  withOption
    ($(char '.') *> digits)
    ( \(placer, nr) ->
        case placel of
          1 -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

minus :: Parser e ()
minus = $(char '-') <|> byteString "¯"

-- | Parser for a signed prefix to a number. Unlike uiua, this parses '-' as a negative number prefix.
--
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
--
-- >>> runParser (signed double) "¯1.234x"
-- OK (-1.234) "x"
signed :: (Num b) => Parser e b -> Parser e b
signed p = do
  m <- optional minus
  case m of
    Nothing -> p
    Just () -> negate <$> p

-- | Comma parser
--
-- >>> runParserMaybe comma ","
-- Just ()
comma :: Parser e ()
comma = $(char ',')
