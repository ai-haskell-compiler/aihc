module Text.Read
  ( Read (..),
    ReadS,
    read,
    reads,
    readParen,
    readEither,
    readMaybe,
    Lexeme (..),
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
    ReadPrec,
    Prec,
    minPrec,
    lift,
    prec,
    step,
    reset,
    get,
    look,
    (+++),
    (<++),
    pfail,
    choice,
    readPrec_to_S,
    readS_to_Prec,
  )
where

import Data.Either (Either (..))
import Data.Maybe (Maybe (..))
import GHC.Read
  ( Read (..),
    ReadS,
    lexP,
    parens,
    readListDefault,
    readListPrecDefault,
    readParen,
  )
import GHC.Read.Lex (Lexeme (..))
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadPrec
  ( Prec,
    ReadPrec,
    choice,
    get,
    lift,
    look,
    minPrec,
    pfail,
    prec,
    readPrec_to_S,
    readS_to_Prec,
    reset,
    step,
    (+++),
    (<++),
  )
import Prelude (Monad (..), String, null, otherwise, read, reads)

-- | Parse a value, and say why if the string does not hold exactly one.
readEither :: (Read a) => String -> Either String a
readEither input =
  case fullParses (readPrec_to_S readWhole minPrec input) of
    [value] -> Right value
    [] -> Left "Prelude.read: no parse"
    _ -> Left "Prelude.read: ambiguous parse"
  where
    readWhole = do
      value <- readPrec
      lift skipSpaces
      return value

-- | Parse a value, or 'Nothing' if the string does not hold exactly one.
readMaybe :: (Read a) => String -> Maybe a
readMaybe input =
  case readEither input of
    Right value -> Just value
    Left _ -> Nothing

-- | The results of a parse that consumed the whole input.
fullParses :: [(a, String)] -> [a]
fullParses [] = []
fullParses ((value, rest) : results)
  | null rest = value : fullParses results
  | otherwise = fullParses results
