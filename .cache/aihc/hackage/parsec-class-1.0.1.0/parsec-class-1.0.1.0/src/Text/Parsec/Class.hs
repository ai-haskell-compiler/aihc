{-# LANGUAGE FlexibleContexts, RankNTypes #-}   -- for 'CharParser'
{-# LANGUAGE CPP #-}

-- | 'HasParser' can be considered a dual to 'Pretty' like 'Read' is to 'Show'.
-- The class provides "Data.Parsec" parsers for its instances that construct
-- the type from its textual representation. Combined with the 'parseM' and
-- 'parse' convenience functions, this class makes parsing simple. Unlike
-- 'Read', Parsec parsers return reasonable error messages in case of failure.
-- Also, there is a rich set of combinators and additional libraries available
-- for re-use.

module Text.Parsec.Class
  ( CharParser, HasParser(parser), ErrorContext
  , parseM, parse
  , -- * Re-exports from Text.Parsec
    module Text.Parsec
  )
  where

import Prelude hiding ( fail )

#if !MIN_VERSION_parsec(3,1,17)
import Text.Parsec.Class.Orphans ( )
#endif

import Control.Exception ( throw )
import Control.Monad.Fail
import Data.Functor.Identity
import Numeric.Natural ( Natural )
import Text.Parsec hiding ( parse )

-- | A simplified 'ParsecT' parser that consumes some kind of character stream
-- without requiring any particular state state.

type CharParser st input m a = Stream st m Char => ParsecT st input m a

-- | Types that are instances of this class can be parsed and constructed from
-- some character based text representation.

class HasParser a where
  parser :: CharParser st input m a

-- | Parsers functions like 'parse' or 'parseM' use this type to provide a
-- helpful context in case the parser failes. Parsec uses the synonym
-- 'SourceName' for the same purpose, but in fact this type doesn't necessarily
-- have to be a file name. It can be any name or identifier. Oftentimes, it
-- it's useful to pass the name of the type that the parser attempted to parse.

type ErrorContext = String

-- | Convenience wrapper around 'runParserT' that uses the 'HasParser' class to
-- determine the desired parser for the given result type. The function reports
-- syntax errors via 'fail'.
--
-- >>> parseM "Natural" "987654321" :: IO Natural
-- 987654321
-- >>> parseM "Natural" "123456789" :: Maybe Natural
-- Just 123456789
--
-- Please note that parsers run this way do not ignore any white space:
--
-- >>> parseM "Natural" " 1" :: Maybe Natural
-- Nothing
-- >>> parseM "Natural" "1 " :: Maybe Natural
-- Nothing

parseM :: (MonadFail m, Stream input m Char, HasParser a) => ErrorContext -> input -> m a
parseM ctx x = runParserT (parser <* eof) () ctx x >>= either (fail . show) return

-- | Convenience wrapper around 'runParser' that uses the 'HasParser' class to
-- determine the desired parser for the given result type. The function reports
-- syntax errors by 'throw'ing 'ParseError'. This approach is inherently impure
-- and complicates error handling greatly. Use this function only on occasions
-- where parser errors are fatal errors that your code cannot recover from. In
-- almost all cases, 'parseM' is the better choice.
--
-- >>> parse "Natural" "12345" :: Natural
-- 12345
--
-- Like 'parseM', this function does not skip over any white space. Use
-- Parsec's primitive 'runParser' or 'runParserT' functions if you don't like
-- this behavior:
--
-- >>> runParser (spaces >> parser) () "Natural" "  1  " :: Either ParseError Natural
-- Right 1

parse :: (Stream input Identity Char, HasParser a) => ErrorContext -> input -> a
parse ctx = either throw id . runParser (parser <* eof) () ctx


----- Useful HasParser instances ----------------------------------------------

instance HasParser Natural where
  parser = read <$> many1 digit
