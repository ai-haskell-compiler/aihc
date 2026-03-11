{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | TH stage restriction guff for flatparsing
module DotParse.FlatParse.TH where

import Data.ByteString hiding (head, length, reverse)
import Data.ByteString.Char8 qualified as B
import Data.Char hiding (isDigit)
import Data.Functor
import FlatParse.Basic
import Language.Haskell.TH

-- | Consume whitespace.
ws :: Parser e ()
ws =
  $( switch
       [|
         case _ of
           -- order matters
           " " -> ws
           "\\\n" -> ws
           "\\\\\\n" -> ws
           "\n" -> ws
           "\t" -> ws
           "\r" -> ws
           ";" -> ws
           "//" -> lineComment
           "/*" -> multilineComment
           _ -> pure ()
         |]
   )

-- | Consume whitespace after running a parser.
token :: Parser e a -> Parser e a
token p = p <* ws
{-# INLINE token #-}

-- | Parse a line comment.
lineComment :: Parser e ()
lineComment =
  withOption
    anyWord8
    ( \case
        10 -> ws
        _ -> lineComment
    )
    (pure ())

-- | Parse a potentially nested multiline comment.
multilineComment :: Parser e ()
multilineComment = go (1 :: Int)
  where
    go 0 = ws
    go n =
      $( switch
           [|
             case _ of
               "*/" -> go (n - 1)
               "/*" -> go (n + 1)
               _ -> branch anyWord8 (go n) (pure ())
             |]
       )

-- | Parse a HTML-Like string by counting the angle brackets
htmlLike :: Parser e String
htmlLike = ws *> $(char '<') *> go (1 :: Int) "<"
  where
    go 0 acc = ws $> reverse acc
    go n acc =
      ($(char '>') *> go (n - 1) ('>' : acc))
        <|> ($(char '<') *> go (n + 1) ('<' : acc))
        <|> (anyChar >>= (\c -> go n (c : acc)))

-- | First character of a dot identifier.
isValidStartChar :: Char -> Bool
isValidStartChar c =
  isAsciiUpper c
    || isAsciiLower c
    || ('\200' <= c && c <= '\377')
    || (c == '_')

-- | character of a dot identifier.
isValidChar :: Char -> Bool
isValidChar c = isValidStartChar c || isDigit c

-- | Read a starting character of an identifier.
identStartChar :: Parser e Char
identStartChar = satisfy isValidStartChar

-- | Read a non-starting character of an identifier.
identChar :: Parser e Char
identChar = satisfy isValidChar

-- | Parse a non-keyword string.
symbol :: String -> Q Exp
symbol str = [|token $(string str)|]

-- | Parse a keyword string.
keyword :: String -> Q Exp
keyword str = [|token ($(string str) `notFollowedBy` identChar)|]

-- | Parser a non-keyword string, throw precise error on failure.
symbol' :: String -> Q Exp
symbol' str = [|$(symbol str) `cut'` strToUtf8 str|]

-- | Parse a keyword string, throw precise error on failure.
keyword' :: String -> Q Exp
keyword' str = [|$(keyword str) `cut'` strToUtf8 str|]

-- | Parse an identifier.
ident :: Parser e ByteString
ident =
  token $
    byteStringOf $
      identStartChar *> skipMany identChar

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser Error ByteString
ident' = ident `cut'` "identifier"

-- | A parsing error.
data Error
  = -- | A precisely known error, like leaving out "in" from "let".
    Precise Pos ByteString
  | -- | An imprecise error, when we expect a number of different things,
    --   but parse something else.
    Imprecise Pos [ByteString]
  deriving (Eq, Show)

-- | position of error
errorPos :: Error -> Pos
errorPos (Precise p _) = p
errorPos (Imprecise p _) = p

-- | Merge two errors. Inner errors (which were thrown at points with more consumed inputs)
--   are preferred. If errors are thrown at identical input positions, we prefer precise errors
--   to imprecise ones.
--
--   The point of prioritizing inner and precise errors is to suppress the deluge of "expected"
--   items, and instead try to point to a concrete issue to fix.
merge :: Error -> Error -> Error
merge e e' = case (errorPos e, errorPos e') of
  (p, p') | p < p' -> e'
  (p, p') | p > p' -> e
  (p, _) -> case (e, e') of
    (Precise {}, _) -> e
    (_, Precise {}) -> e'
    (Imprecise _ es, Imprecise _ es') -> Imprecise p (es <> es')
{-# NOINLINE merge #-} -- merge is "cold" code, so we shouldn't inline it.

-- | Pretty print an error. The `ByteString` input is the source file. The offending line from the
--   source is displayed in the output.
prettyError :: ByteString -> Error -> ByteString
prettyError b e =
  let pos :: Pos
      pos = case e of
        Imprecise pos _ -> pos
        Precise pos _ -> pos
      ls = B.lines b
      (l, c) = head $ posLineCols b [pos]
      line = if l < length ls then ls !! l else ""
      linum = strToUtf8 $ show l
      lpad = B.replicate (B.length linum) ' '

      err (Precise _ e) = e
      err (Imprecise _ es) = imprec es

      imprec :: [ByteString] -> ByteString
      imprec [] = error "impossible"
      imprec [e] = e
      imprec (e : es) = e <> go es
        where
          go [] = ""
          go [e] = " or " <> e
          go (e : es) = ", " <> e <> go es
   in strToUtf8 (show l)
        <> ":"
        <> strToUtf8 (show c)
        <> ":\n"
        <> lpad
        <> "|\n"
        <> linum
        <> "| "
        <> line
        <> "\n"
        <> lpad
        <> "| "
        <> B.replicate c ' '
        <> "^\n"
        <> "parse error: expected "
        <> err e

-- | Imprecise cut: we slap a list of items on inner errors.
cut :: Parser Error a -> [ByteString] -> Parser Error a
cut p es = do
  pos <- getPos
  cutting p (Imprecise pos es) merge

-- | Precise cut: we propagate at most a single error.
cut' :: Parser Error a -> ByteString -> Parser Error a
cut' p e = do
  pos <- getPos
  cutting p (Precise pos e) merge
