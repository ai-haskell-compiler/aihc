{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Lower-level flatparse parsers
module DotParse.FlatParse
  ( Error (..),
    prettyError,
    keyword,
    keyword',
    symbol,
    symbol',
    ws,
    token,
    ident,
    cut,
    cut',
    testParser,
    runParser_,
    int,
    double,
    signed,
    quoted,
    htmlLike,
    sepP,
    wrapSquareP,
    wrapSquarePrint,
    wrapCurlyP,
    wrapCurlyPrint,
    wrapQuotePrint,
    pointP,
    Spline (..),
    splineP,
    rectP,
    boolP,
    nonEmptyP,
  )
where

import Data.Bool
import Data.ByteString hiding (empty, head, length, map, zip, zipWith)
import Data.ByteString.Char8 qualified as B
import Data.Char hiding (isDigit)
import Data.List.NonEmpty
import DotParse.FlatParse.TH hiding (merge)
import FlatParse.Basic hiding (cut)
import GHC.Generics
import NumHask.Space
import Prelude hiding (replicate)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import DotParse
-- >>> import FlatParse.Basic

-- | Run parser, print pretty error on failure.
testParser :: (Show a) => Parser Error a -> ByteString -> IO ()
testParser p b =
  case runParser p b of
    Err e -> B.putStrLn $ prettyError b e
    OK a _ -> print a
    Fail -> B.putStrLn "uncaught parse error"

-- | run a Parser, erroring on leftovers, Fail or Err
runParser_ :: Parser Error a -> ByteString -> a
runParser_ p b = case runParser p b of
  OK r "" -> r
  OK _ x -> error $ utf8ToStr $ "leftovers: " <> x
  Fail -> error "Fail"
  Err e -> error $ utf8ToStr $ prettyError b e

-- * parsing

digit :: Parser Error Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | (unsigned) Int parser
int :: Parser Error Int
int = token do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser Error (Int, Int)
digits = chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))

-- |
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
-- OK 0.123 ""
--
-- >>> runParser double "123."
-- OK 123.0 ""
double :: Parser Error Double
double = token do
  (placel, nl) <- digits
  withOption
    ($(char '.') *> digits)
    ( \(placer, nr) ->
        case (placel, placer) of
          (1, 1) -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

-- |
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: (Num b) => Parser e b -> Parser e b
signed p = withOption ($(char '-')) (const (((-1) *) <$> p)) p

-- | Looks ahead for a "/"" that may be in the quoted string.
-- >>> runParser quoted (strToUtf8 "\"hello\"")
-- OK "hello" ""
--
-- >>> runParser quoted (strToUtf8 "\"hello/\"\"")
-- OK "hello\"" ""
quoted :: Parser Error String
quoted =
  $(symbol "\"") *> many unquoteQuote <* $(symbol' "\"")

unquoteQuote :: Parser Error Char
unquoteQuote = do
  next <- satisfy (/= '"')
  case next of
    '/' -> branch (lookahead $(char '"')) ('"' <$ $(char '"')) (pure '/')
    x -> pure x

-- | optional separators
sepP :: Parser e ()
sepP =
  token
    $( switch
         [|
           case _ of
             ";" -> pure ()
             "," -> pure ()
           |]
     )

-- | parse wrapping square brackets
wrapSquareP :: Parser Error a -> Parser Error a
wrapSquareP p =
  $(symbol "[") *> p <* $(symbol' "]")

-- | print wrapping square brackets
wrapSquarePrint :: ByteString -> ByteString
wrapSquarePrint b = "[" <> b <> "]"

-- | print wrapping quotes
wrapQuotePrint :: ByteString -> ByteString
wrapQuotePrint b = "\"" <> b <> "\""

-- | parse wrapping square brackets
wrapCurlyP :: Parser Error a -> Parser Error a
wrapCurlyP p = $(symbol "{") *> p <* $(symbol' "}")

-- | print wrapping curly brackets
wrapCurlyPrint :: ByteString -> ByteString
wrapCurlyPrint b = "{" <> b <> "}"

-- | comma separated Point
pointP :: Parser Error (Point Double)
pointP = token $ Point <$> double <*> ($(symbol ",") *> double)

-- | dot specification of a cubic spline (and an arrow head which is ignored here)
data Spline = Spline {splineEnd :: Maybe (Point Double), splineStart :: Maybe (Point Double), splineP1 :: Point Double, splineTriples :: [(Point Double, Point Double, Point Double)]} deriving (Eq, Show, Generic)

-- |
-- http://www.graphviz.org/docs/attr-types/splineType/
splineP :: Parser Error Spline
splineP =
  Spline
    <$> optional ($(symbol "e,") *> pointP)
    <*> optional ($(symbol "s") *> pointP)
    <*> pointP
    <*> some ((,,) <$> pointP <*> pointP <*> pointP)

-- | comma separated rectangle or bounding box
rectP :: Parser Error (Rect Double)
rectP = token $ do
  x <- double
  _ <- $(symbol ",")
  y <- double
  _ <- $(symbol ",")
  z <- double
  _ <- $(symbol ",")
  w <- double
  pure $ Rect x z y w

-- | true | false
boolP :: Parser Error Bool
boolP =
  (True <$ $(symbol "true"))
    <|> (False <$ $(symbol "false"))

-- | NonEmpty version of many
nonEmptyP :: Parser e a -> Parser e () -> Parser e (NonEmpty a)
nonEmptyP p sep = token $ do
  s <- p
  xs <- many (optional sep *> p)
  pure (s :| xs)
