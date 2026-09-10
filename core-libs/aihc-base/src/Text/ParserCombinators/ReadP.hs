{-# HLINT ignore "Use camelCase" #-}

-- | Backtracking parsers over a 'String'.
--
-- The parser is a list of results, so a combinator that offers a choice
-- returns every way the input can be read. The two exceptions are the
-- combinators GHC documents as deterministic: 'munch', 'munch1',
-- 'skipSpaces' and 'string' commit to the longest match and return at
-- most one result.
module Text.ParserCombinators.ReadP
  ( -- * The parser
    ReadP,
    ReadS,
    readP_to_S,
    readS_to_P,

    -- * Primitives
    get,
    look,
    (+++),
    (<++),
    gather,
    pfail,

    -- * Combinators
    eof,
    satisfy,
    char,
    string,
    munch,
    munch1,
    skipSpaces,
    choice,
    count,
    between,
    option,
    optional,
    many,
    many1,
    skipMany,
    skipMany1,
    sepBy,
    sepBy1,
    endBy,
    endBy1,
    chainr,
    chainl,
    chainl1,
    chainr1,
    manyTill,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), void)
import Data.Char (isSpace)
import Prelude
  ( Applicative (..),
    Bool (..),
    Char,
    Eq (..),
    Functor (..),
    Int,
    Monad (..),
    MonadFail (..),
    Num (..),
    Ord (..),
    ReadS,
    String,
    const,
    foldr,
    length,
    otherwise,
    splitAt,
    (++),
    (.),
    (<$>),
  )

newtype ReadP a = ReadP (ReadS a)

instance Functor ReadP where
  fmap function (ReadP parser) =
    ReadP (readPMapResults function . parser)

instance Applicative ReadP where
  pure value = ReadP (\input -> [(value, input)])

  ReadP functionParser <*> ReadP valueParser =
    ReadP (readPApplyResults valueParser . functionParser)

instance Monad ReadP where
  ReadP parser >>= next =
    ReadP (readPBindResults next . parser)

  ReadP first >> ReadP second =
    ReadP (readPThenResults second . first)

  return = pure

instance MonadFail ReadP where
  fail _ = pfail

instance Alternative ReadP where
  empty = pfail
  (<|>) = (+++)

instance MonadPlus ReadP

readPMapResults :: (a -> b) -> [(a, String)] -> [(b, String)]
readPMapResults _ [] = []
readPMapResults function ((value, rest) : results) =
  (function value, rest) : readPMapResults function results

readPApplyResults :: ReadS a -> [(a -> b, String)] -> [(b, String)]
readPApplyResults _ [] = []
readPApplyResults parser ((function, rest) : results) =
  readPMapResults function (parser rest) ++ readPApplyResults parser results

readPBindResults :: (a -> ReadP b) -> [(a, String)] -> [(b, String)]
readPBindResults _ [] = []
readPBindResults next ((value, rest) : results) =
  readP_to_S (next value) rest ++ readPBindResults next results

readPThenResults :: ReadS b -> [(a, String)] -> [(b, String)]
readPThenResults _ [] = []
readPThenResults parser ((_, rest) : results) =
  parser rest ++ readPThenResults parser results

readP_to_S :: ReadP a -> ReadS a
readP_to_S (ReadP parser) = parser

readS_to_P :: ReadS a -> ReadP a
readS_to_P = ReadP

-- | Read one character. Fails at the end of the input.
get :: ReadP Char
get = ReadP readPGet

readPGet :: ReadS Char
readPGet [] = []
readPGet (character : rest) = [(character, rest)]

-- | The rest of the input, without consuming any of it.
look :: ReadP String
look = ReadP (\input -> [(input, input)])

-- | Both parsers, symmetrically: the results of the one followed by the
-- results of the other.
(+++) :: ReadP a -> ReadP a -> ReadP a
ReadP left +++ ReadP right =
  ReadP (\input -> left input ++ right input)

infixr 5 +++

-- | The left parser if it reads the input at all, and the right one only
-- if it does not.
(<++) :: ReadP a -> ReadP a -> ReadP a
ReadP left <++ ReadP right =
  ReadP
    ( \input ->
        case left input of
          [] -> right input
          results -> results
    )

infixr 5 <++

-- | A parser paired with the input it consumed.
gather :: ReadP a -> ReadP (String, a)
gather (ReadP parser) =
  ReadP (\input -> readPGatherResults input (parser input))

readPGatherResults :: String -> [(a, String)] -> [((String, a), String)]
readPGatherResults _ [] = []
readPGatherResults input ((value, rest) : results) =
  ((consumed, value), rest) : readPGatherResults input results
  where
    (consumed, _) = splitAt (length input - length rest) input

-- | A parser that never succeeds.
pfail :: ReadP a
pfail = ReadP (const [])

-- | Succeeds only at the end of the input.
eof :: ReadP ()
eof = ReadP readPEof

readPEof :: ReadS ()
readPEof [] = [((), [])]
readPEof _ = []

-- | Read one character that satisfies a predicate.
satisfy :: (Char -> Bool) -> ReadP Char
satisfy predicate = ReadP (readPSatisfy predicate)

readPSatisfy :: (Char -> Bool) -> ReadS Char
readPSatisfy predicate (character : rest)
  | predicate character = [(character, rest)]
readPSatisfy _ _ = []

-- | Read the given character.
char :: Char -> ReadP Char
char wanted = satisfy (wanted ==)

-- | Read the given string. Deterministic.
string :: String -> ReadP String
string wanted = ReadP (readPString wanted wanted)

readPString :: String -> String -> ReadS String
readPString wanted [] input = [(wanted, input)]
readPString wanted (character : rest) input =
  case input of
    actual : remaining | actual == character -> readPString wanted rest remaining
    _ -> []

-- | The longest prefix of characters that satisfy a predicate, possibly
-- empty. Deterministic.
munch :: (Char -> Bool) -> ReadP String
munch predicate = ReadP (readPMunch predicate)

-- | Like 'munch', but fails on an empty prefix. Deterministic.
munch1 :: (Char -> Bool) -> ReadP String
munch1 predicate =
  ReadP
    ( \input ->
        case readPMunch predicate input of
          [([], _)] -> []
          results -> results
    )

readPMunch :: (Char -> Bool) -> ReadS String
readPMunch predicate input = [readPSpan predicate input]

readPSpan :: (Char -> Bool) -> String -> (String, String)
readPSpan predicate input =
  case input of
    character : rest
      | predicate character ->
          let (matched, remaining) = readPSpan predicate rest
           in (character : matched, remaining)
    _ -> ([], input)

-- | Skip the leading whitespace. Deterministic.
skipSpaces :: ReadP ()
skipSpaces = ReadP (\input -> [((), readPDropSpaces input)])

readPDropSpaces :: String -> String
readPDropSpaces input =
  case input of
    character : rest | isSpace character -> readPDropSpaces rest
    _ -> input

-- | The first parser of the list that succeeds, symmetrically.
choice :: [ReadP a] -> ReadP a
choice = foldr (+++) pfail

-- | Run a parser a fixed number of times.
count :: Int -> ReadP a -> ReadP [a]
count times parser
  | times <= 0 = return []
  | otherwise = do
      value <- parser
      values <- count (times - 1) parser
      return (value : values)

-- | A parser between an opening and a closing bracket.
between :: ReadP open -> ReadP close -> ReadP a -> ReadP a
between open close parser = do
  _ <- open
  value <- parser
  _ <- close
  return value

-- | A parser, or a default value if it does not succeed.
option :: a -> ReadP a -> ReadP a
option fallback parser = parser +++ return fallback

-- | A parser whose result is discarded, and which need not succeed.
optional :: ReadP a -> ReadP ()
optional parser = void parser +++ return ()

-- | Zero or more of a parser.
many :: ReadP a -> ReadP [a]
many parser = return [] +++ many1 parser

-- | One or more of a parser.
many1 :: ReadP a -> ReadP [a]
many1 parser = do
  value <- parser
  values <- many parser
  return (value : values)

-- | Like 'many', discarding the results.
skipMany :: ReadP a -> ReadP ()
skipMany parser = void (many parser)

-- | Like 'many1', discarding the results.
skipMany1 :: ReadP a -> ReadP ()
skipMany1 parser = parser >> skipMany parser

-- | Zero or more of a parser, separated by another.
sepBy :: ReadP a -> ReadP sep -> ReadP [a]
sepBy parser separator = sepBy1 parser separator +++ return []

-- | One or more of a parser, separated by another.
sepBy1 :: ReadP a -> ReadP sep -> ReadP [a]
sepBy1 parser separator = do
  value <- parser
  values <- many (separator >> parser)
  return (value : values)

-- | Zero or more of a parser, each followed by another.
endBy :: ReadP a -> ReadP sep -> ReadP [a]
endBy parser separator = many (readPBefore parser separator)

-- | One or more of a parser, each followed by another.
endBy1 :: ReadP a -> ReadP sep -> ReadP [a]
endBy1 parser separator = many1 (readPBefore parser separator)

readPBefore :: ReadP a -> ReadP sep -> ReadP a
readPBefore parser separator = do
  value <- parser
  _ <- separator
  return value

-- | Zero or more of a parser, separated by a right-associative operator,
-- or a default value.
chainr :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainr parser operator fallback = chainr1 parser operator +++ return fallback

-- | Zero or more of a parser, separated by a left-associative operator,
-- or a default value.
chainl :: ReadP a -> ReadP (a -> a -> a) -> a -> ReadP a
chainl parser operator fallback = chainl1 parser operator +++ return fallback

-- | One or more of a parser, separated by a left-associative operator.
chainl1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainl1 parser operator = parser >>= rest
  where
    rest left =
      ( do
          combine <- operator
          right <- parser
          rest (combine left right)
      )
        +++ return left

-- | One or more of a parser, separated by a right-associative operator.
chainr1 :: ReadP a -> ReadP (a -> a -> a) -> ReadP a
chainr1 parser operator = scan
  where
    scan = parser >>= rest
    rest left =
      ( do
          combine <- operator
          combine left <$> scan
      )
        +++ return left

-- | Repeat a parser until another one succeeds.
manyTill :: ReadP a -> ReadP end -> ReadP [a]
manyTill parser end = scan
  where
    scan =
      (end >> return [])
        <++ ( do
                value <- parser
                values <- scan
                return (value : values)
            )
