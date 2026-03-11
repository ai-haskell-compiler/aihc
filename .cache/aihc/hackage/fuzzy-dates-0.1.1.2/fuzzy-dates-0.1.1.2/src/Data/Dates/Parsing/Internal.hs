{-# LANGUAGE FlexibleContexts #-}

module Data.Dates.Parsing.Internal where

import Data.Char           (digitToInt, isDigit, toUpper, toLower, toUpper)
import Data.Hourglass
import Data.List           (isPrefixOf, lookup)
import Data.Maybe          (fromJust, catMaybes)
import Text.Parsec
import Text.Read (readMaybe)

-- | Parsers the parser at least once, but no more than n times.
takeN1 :: Stream s m Char => Int -> ParsecT s st m a -> ParsecT s st m [a]
takeN1 0 _ = error "n must not be 0!"
takeN1 1 parser = (:[]) <$> parser
takeN1 n parser = do
    v <- parser
    rest <- takeN1 (n - 1) parser <|> pure []
    pure $ v : rest

-- | Parse natural number of N digits which is not greater than M
number :: (Stream s m Char, Integral a, Show a)
       => Int -- ^ Number of digits
       -> a   -- ^ Maximum value
       -> ParsecT s st m a
number n m = do
  maybeT <- readMaybe <$> takeN1 n digit
  case fromIntegral <$> maybeT of
    Just t | t <= m -> pure t
    _ -> fail $ "Couldn't parse into number with parameters: " ++ show n ++ "," ++ show m

pYear :: Stream s m Char => ParsecT s st m Int
pYear = do
    n <- try pYearNormal <|> pYearAny
        -- Assume two digit years are after 2000.
        -- TODO: Update in 82 years (2018-05-03).
    pure $ if n < 2000 && n < 100 && n >= 10 then n + 2000 else n

pYearNormal :: Stream s m Char => ParsecT s st m Int
pYearNormal = do
    n <- read <$> many1 digit

    notFollowedBy (try (spaces >> yearAbbreviations) <|> (digit >> pure ""))

    pure n

readNum :: (Num a, Stream s m Char) => ParsecT s st m a
readNum = do
    isNegative <- optionMaybe $ char '-'
    digits <- many1 digit

    let sign = maybe 1 (const (-1)) isNegative

    pure $ sign * fromInteger (read digits)

yearAbbreviations :: Stream s m Char => ParsecT s st m String
yearAbbreviations = choice $ map (try . abbParser) ["BCE", "AD", "CE", "BC"]
    where
        abbParser :: Stream s m Char => String -> ParsecT s st m String
        abbParser abbr = parseAs (concatMap casings $ makeAbbr abbr) abbr

makeAbbr :: String -> [String]
makeAbbr abb = [abb, foldl (\cur n -> cur ++ [n] ++ ".") "" abb]

pYearAny :: Stream s m Char => ParsecT s st m Int
pYearAny = do
    spaces
    -- Allow abbreviations before or after.
    prefix <- optionMaybe yearAbbreviations

    spaces
    n <- readNum
    spaces

    suffix <- optionMaybe yearAbbreviations

    let isBC = case catMaybes [prefix, suffix] of
                (abb:_) -> abb == "BC" || abb == "BCE"
                [] -> False

    pure $ if isBC then -n else n

monthAssoc :: [(String, Month)]
monthAssoc = [("january", January), ("jan", January), ("february", February), ("feb", February),
              ("march", March), ("mar", March), ("april", April), ("apr", April),
              ("may", May), ("june", June), ("jun", June), ("july", July), ("july", July),
              ("august", August), ("aug", August), ("september", September), ("sept", September),
              ("october", October), ("oct", October), ("november", November), ("nov", November),
              ("december", December), ("dec", December)]

casings :: String -> [String]
casings [] = []
casings str@(f:rest) = [str, map toLower str, map toUpper str, toUpper f : rest]

-- | Parse various capitalizations of the given string, but always return the same string on success.
parseAs :: Stream s m Char => [String] -> String -> ParsecT s st m String
parseAs options str = do
    _ <- choice $ map (try . string) options

    optional $ char '.'

    pure str

pMonthName :: Stream s m Char => ParsecT s st m Month
pMonthName = do
    monthName <- choice $ map (\(name,_) -> try $ parseAs (casings name) name) monthAssoc

    -- Safe because month names come from monthAssoc
    return $ fromJust $ lookup monthName monthAssoc

pMonth :: Stream s m Char => ParsecT s st m Month
pMonth = try (toEnum . pred <$> number 2 12) <|>
         pMonthName

pDay :: Stream s m Char => ParsecT s st m Int
pDay = number 2 31

uppercase :: String -> String
uppercase = map toUpper

-- | Case-insensitive version of 'isPrefixOf'
isPrefixOfI :: String -> String -> Bool
p `isPrefixOfI` s = uppercase p `isPrefixOf` uppercase s

-- | Use a data type's Bounded, Enum and Show instances to determine if the
-- given string uniquely matches a constructor. The comparison is
-- case-insensitive and starts from the beginning of the strings (so a partial
-- constructor name can still match if there are enough characters for a
-- unique match)
--
-- For example:
--
-- @
--  data Things = Foo | Bar | Baz deriving (Bounded, Enum, Show)
--
--  -- Right Foo
--  uniqFuzzyMatch "f" :: Either [Things] Things
--
--  -- Left [Bar, Baz]
--  uniqFuzzyMatch "ba" :: Either [Things] Things
-- @
uniqFuzzyMatch :: (Bounded a, Enum a, Show a)
               => String
               -> Either [a] a -- ^ Either collection of matches or the unique match
uniqFuzzyMatch n =
    case matches of
        [match] -> Right match
        _ -> Left matches
  where
    possibilities = [minBound..maxBound]
    matches = filter (isPrefixOfI n . show) possibilities

