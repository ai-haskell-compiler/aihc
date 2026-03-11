{-# LANGUAGE FlexibleContexts #-}

-- | This package provides a simple range parser.
--
-- This range parser was designed to be a useful tool for CLI programs. For example, by
-- default, this example depicts how the parser works:
--
-- >>> parseRanges "-5,8-10,13-15,20-" :: Either ParseError [Range Integer]
-- Right [UpperBoundRange 5,SpanRange 8 10,SpanRange 13 15,LowerBoundRange 20]
-- (0.01 secs, 681,792 bytes)
--
-- And the * character translates to an infinite range. This is very useful for accepting
-- ranges as input in CLI programs, but not as useful for parsing .cabal or package.json files.
--
-- To handle more complex parsing cases it is recommended that you use the ranges library
-- in conjunction with parsec or Alex/Happy and convert the versions that you find into
-- ranges.
module Data.Range.Parser
   ( parseRanges
   , customParseRanges
   , RangeParserArgs(..)
   , defaultArgs
   , ranges
   , ParseError
   ) where

import Text.Parsec
import Text.Parsec.String

import Data.Range

-- | These are the arguments that will be used when parsing a string as a range.
data RangeParserArgs = Args
   { unionSeparator :: String -- ^ A separator that represents a union.
   , rangeSeparator :: String -- ^ A separator that separates the two halves of a range.
   , wildcardSymbol :: String -- ^ A separator that implies an unbounded range.
   }
   deriving(Show)

-- | These are the default arguments that are used by the parser. Please feel free to use
-- the default arguments for you own parser and modify it from the defaults at will.
defaultArgs :: RangeParserArgs
defaultArgs = Args
   { unionSeparator = ","
   , rangeSeparator = "-"
   , wildcardSymbol = "*"
   }

-- | Given a string, this function will either return a parse error back to the user or the
-- list of ranges that are represented by the parsed string. Very useful for CLI programs
-- that need to load ranges from a single-line string.
parseRanges :: (Read a) => String -> Either ParseError [Range a]
parseRanges = parse (ranges defaultArgs) "(range parser)"

-- | If you disagree with the default characters for separating ranges then this function can
-- be used to customise them, up to a point.
customParseRanges :: Read a => RangeParserArgs -> String -> Either ParseError [Range a]
customParseRanges args = parse (ranges args) "(range parser)"

string_ :: Stream s m Char => String -> ParsecT s u m ()
string_ x = string x >> return ()

-- | Given the parser arguments this returns a parsec parser that is capable of parsing a list of
-- ranges.
ranges :: (Read a) => RangeParserArgs -> Parser [Range a]
ranges args = range `sepBy` (string $ unionSeparator args)
   where
      range :: (Read a) => Parser (Range a)
      range = choice
         [ infiniteRange
         , spanRange
         , singletonRange
         ]

      infiniteRange :: (Read a) => Parser (Range a)
      infiniteRange = do
         string_ $ wildcardSymbol args
         return InfiniteRange

      spanRange :: (Read a) => Parser (Range a)
      spanRange = try $ do
         first <- readSection
         string_ $ rangeSeparator args
         second <- readSection
         case (first, second) of
            (Just x, Just y)  -> return $ SpanRange (Bound x Inclusive) (Bound y Inclusive)
            (Just x, _)       -> return $ LowerBoundRange (Bound x Inclusive)
            (_, Just y)       -> return $ UpperBoundRange (Bound y Inclusive)
            _                 -> parserFail ("Range should have a number on one end: " ++ rangeSeparator args)

      singletonRange :: (Read a) => Parser (Range a)
      singletonRange = fmap (SingletonRange . read) $ many1 digit

readSection :: (Read a) => Parser (Maybe a)
readSection = fmap (fmap read) $ optionMaybe (many1 digit)
