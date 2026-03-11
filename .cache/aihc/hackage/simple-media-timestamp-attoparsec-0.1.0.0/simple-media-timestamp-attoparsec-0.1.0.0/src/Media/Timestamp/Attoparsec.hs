{-# LANGUAGE OverloadedStrings #-}

module Media.Timestamp.Attoparsec
  ( parseTime,
    parseTimec,
    parseRange,
    parseRangeA,
  )
where

import Data.Attoparsec.Text
import Media.Timestamp

intChar :: Char -> Parser Int
intChar x = decimal <* char x

-- | Parse a `Time` of the form `00:00:00.000`. This is ffmpeg's format.
--
-- @since 0.1.0.0
parseTime :: Parser Time
parseTime = Time <$> intChar ':' <*> intChar ':' <*> intChar '.' <*> decimal

-- | Parse a `Time` of the form `00:00:00,000`. This is srt format.
--
-- @since 0.1.0.0
parseTimec :: Parser Time
parseTimec = Time <$> intChar ':' <*> intChar ':' <*> intChar ',' <*> decimal

-- | Parse a `Range` of the form `(X,Y)`
--
-- @since 0.1.0.0
parseRange :: Parser Time -> Parser Range
parseRange x = Range <$ char '(' <*> x <* "," <*> x <* char ')'

-- | Parse a `Range` of the form `X --> Y`. This is srt format. A
-- parser for an SRT range would then be `parseRangeA parseTimec`.
--
-- @since 0.1.0.0
parseRangeA :: Parser Time -> Parser Range
parseRangeA x = Range <$> x <* string (" --> ") <*> x
