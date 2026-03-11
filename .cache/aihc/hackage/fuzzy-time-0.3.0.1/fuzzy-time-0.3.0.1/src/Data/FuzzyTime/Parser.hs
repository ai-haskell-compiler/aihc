{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.FuzzyTime.Parser
  ( fuzzyLocalTimeP,
    fuzzyTimeOfDayP,
    atHourP,
    atMinuteP,
    atExactP,
    hourSegmentP,
    minuteSegmentP,
    twoDigitsSegmentP,
    fuzzyDayP,
    fuzzyDayOfTheWeekP,
    Parser,
  )
where

import Control.Monad (guard, msum, void)
import Data.Char as Char (toLower)
import Data.Fixed (Pico)
import Data.FuzzyTime.Types (FuzzyDay (..), FuzzyLocalTime (..), FuzzyTimeOfDay (AtExact, AtHour, AtMinute, Evening, HoursDiff, Midnight, MinutesDiff, Morning, Noon, SecondsDiff))
import Data.List (find)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text (Text)
import Data.Time (DayOfWeek (..), TimeOfDay (TimeOfDay), defaultTimeLocale, parseTimeM)
import Data.Tree (Forest, Tree (Node), rootLabel, subForest)
import Data.Validity (isValid)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (Parsec, empty, eof, label, oneOf, optional, some, try, (<|>))
import Text.Megaparsec.Char as Char (char, digitChar, letterChar, space1, string)
import Text.Megaparsec.Char.Lexer as Lexer (decimal)
import Text.Read (readMaybe)

type Parser = Parsec Void Text

fuzzyLocalTimeP :: Parser FuzzyLocalTime
fuzzyLocalTimeP =
  label "FuzzyLocalTime" $
    choice''
      [ do
          a <- fuzzyDayP
          space1
          b <- fuzzyTimeOfDayP
          pure $ FuzzyLocalTimeBoth a b,
        FuzzyLocalTimeDay <$> fuzzyDayP,
        FuzzyLocalTimeTimeOfDay <$> fuzzyTimeOfDayP
      ]

fuzzyTimeOfDayP :: Parser FuzzyTimeOfDay
fuzzyTimeOfDayP =
  label "FuzzyTimeOfDay" $
    choice'
      [ recTreeParser
          [ ("midnight", Midnight),
            ("midday", Noon),
            ("noon", Noon),
            ("morning", Morning),
            ("evening", Evening)
          ],
        atExactP,
        atMinuteP,
        atHourP,
        diffP
      ]

atHourP :: Parser FuzzyTimeOfDay
atHourP =
  label "AtHour" $ do
    h <- hourSegmentP
    pure $ AtHour h

atMinuteP :: Parser FuzzyTimeOfDay
atMinuteP =
  label "AtMinute" $ do
    h <- hourSegmentP
    m <- minuteSegmentP
    pure $ AtMinute h m

atExactP :: Parser FuzzyTimeOfDay
atExactP =
  label "AtExact" $ do
    h <- hourSegmentP
    m <- minuteSegmentP
    void $ char ':'
    s <- readSimplePico
    pure $ AtExact $ TimeOfDay h m s

readSimplePico :: Parser Pico
readSimplePico = do
  let d = oneOf ['0' .. '9']
  beforeDot <- some d :: Parser String
  afterDot <-
    optional $ do
      dot <- char '.'
      r <- some d
      pure $ dot : r
  pure $ read $ beforeDot <> fromMaybe "" afterDot

diffP :: Parser FuzzyTimeOfDay
diffP =
  label "Diff" $ do
    n <- signed' decimal
    mc <- optional $ choice' [char 'h', char 'm', char 's']
    f <-
      case mc of
        Nothing -> pure HoursDiff
        Just 'h' -> pure HoursDiff
        Just 'm' -> pure MinutesDiff
        Just 's' -> pure (\i -> SecondsDiff $ fromIntegral i)
        _ -> fail "should not happen."
    pure $ f n

hourSegmentP :: Parser Int
hourSegmentP =
  label "hour segment" $ do
    h <- twoDigitsSegmentP
    guard $ h >= 0 && h < 24
    void $ optional $ char ':'
    pure h

minuteSegmentP :: Parser Int
minuteSegmentP =
  label "minute segment" $ do
    m <- twoDigitsSegmentP
    guard $ m >= 0 && m < 60
    pure m

twoDigitsSegmentP :: (Num a, Read a) => Parser a
twoDigitsSegmentP =
  label "two digit segment" $ do
    d1 <- digit
    md2 <- optional digit
    pure $
      case md2 of
        Nothing -> d1
        Just d2 -> 10 * d1 + d2

digit :: (Read a) => Parser a
digit =
  label "digit" $ do
    let l = ['0' .. '9']
    c <- oneOf l
    case readMaybe [c] of
      Nothing -> fail "Shouldn't happen."
      Just d -> pure d

-- | Can handle:
--
-- - yesterday
-- - now
-- - today
-- - tomorrow
-- - "%Y-%m-%d"
--
-- and all non-ambiguous prefixes
fuzzyDayP :: Parser FuzzyDay
fuzzyDayP =
  label "FuzzyDay" $
    choice'
      [ recTreeParser
          [("yesterday", Yesterday), ("now", Now), ("today", Today), ("tomorrow", Tomorrow)],
        fmap ExactDay (some (digitChar <|> char '-') >>= parseTimeM True defaultTimeLocale "%Y-%m-%d"),
        dayInMonthP,
        dayOfTheMonthP,
        fuzzyDayOfTheWeekP,
        diffDayP
      ]

dayOfTheMonthP :: Parser FuzzyDay
dayOfTheMonthP = do
  dayNo <- twoDigitsSegmentP
  let v = OnlyDay dayNo
  guard $ isValid v
  pure v

dayInMonthP :: Parser FuzzyDay
dayInMonthP = do
  m <-
    choice'
      [ do
          m <- twoDigitsSegmentP
          guard (m >= 1)
          guard (m <= 12)
          pure m,
        namedMonthP
      ]
  void $ string "-"
  d <- twoDigitsSegmentP
  let v = DayInMonth m d
  guard $ isValid v
  pure v

namedMonthP :: Parser Word8
namedMonthP =
  recTreeParser
    [ ("january", 1),
      ("february", 2),
      ("march", 3),
      ("april", 4),
      ("may", 5),
      ("june", 6),
      ("july", 7),
      ("august", 8),
      ("september", 9),
      ("october", 10),
      ("november", 11),
      ("december", 12)
    ]

diffDayP :: Parser FuzzyDay
diffDayP = do
  d <- signed' decimal
  mc <- optional $ oneOf ['d', 'w', 'm']
  let f =
        case mc of
          Nothing -> DiffDays
          Just 'd' -> DiffDays
          Just 'w' -> DiffWeeks
          Just 'm' -> DiffMonths
          _ -> DiffDays -- Should not happen.
  pure $ f d

fuzzyDayOfTheWeekP :: Parser FuzzyDay
fuzzyDayOfTheWeekP = do
  dow <- dayOfTheWeekP
  mExtraDiff <- optional $ signed' decimal
  pure $ DayOfTheWeek dow (fromMaybe 0 mExtraDiff)

-- | Can handle:
--
-- - monday
-- - tuesday
-- - wednesday
-- - thursday
-- - friday
-- - saturday
-- - sunday
--
-- and all non-ambiguous prefixes
dayOfTheWeekP :: Parser DayOfWeek
dayOfTheWeekP =
  recTreeParser
    [ ("monday", Monday),
      ("tuesday", Tuesday),
      ("wednesday", Wednesday),
      ("thursday", Thursday),
      ("friday", Friday),
      ("saturday", Saturday),
      ("sunday", Sunday)
    ]

recTreeParser :: [(String, a)] -> Parser a
recTreeParser tups = do
  let pf = makeParseForest tups
  s <- some letterChar
  case lookupInParseForest s pf of
    Nothing ->
      fail $ "Could not parse any of these recursively unambiguously: " ++ show (map fst tups)
    Just f -> pure f

lookupInParseForest :: [Char] -> Forest (Char, Maybe a) -> Maybe a
lookupInParseForest = gof
  where
    gof :: [Char] -> Forest (Char, Maybe a) -> Maybe a
    gof cs = msum . map (got cs)
    got :: [Char] -> Tree (Char, Maybe a) -> Maybe a
    got [] _ = Nothing
    got (c : cs) Node {..} =
      let (tc, tma) = rootLabel
       in if Char.toLower tc == Char.toLower c
            then case cs of
              [] -> tma
              _ -> gof cs subForest
            else Nothing

makeParseForest :: (Eq c) => [([c], a)] -> Forest (c, Maybe a)
makeParseForest = foldl insertf []
  where
    insertf :: (Eq c) => Forest (c, Maybe a) -> ([c], a) -> Forest (c, Maybe a)
    insertf for ([], _) = for
    insertf for (c : cs, a) =
      case find ((== c) . fst . rootLabel) for of
        Nothing ->
          let got [] = Nothing
              got (c_ : cs_) = Just $ Node (c_, Just a) $ maybeToList $ got cs_
           in case got (c : cs) of
                Nothing -> for -- Should not happen, but is fine
                Just t -> t : for
        Just n ->
          flip map for $ \t ->
            let (tc, _) = rootLabel t
             in if tc == c
                  then n {rootLabel = (tc, Nothing), subForest = insertf (subForest n) (cs, a)}
                  else t

signed' :: (Num a) => Parser a -> Parser a
signed' p = sign <*> p
  where
    sign = (id <$ char '+') <|> (negate <$ char '-')

choice' :: [Parser a] -> Parser a
choice' [] = empty
choice' [x] = x
choice' (a : as) = try a <|> choice' as

choice'' :: [Parser a] -> Parser a
choice'' = choice' . map (<* eof)
