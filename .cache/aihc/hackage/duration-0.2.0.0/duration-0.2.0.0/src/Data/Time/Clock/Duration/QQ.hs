{-# LANGUAGE TemplateHaskell #-}

{-|
Copyright   : (c) Ryota Kameoka, 2018
License     : BSD-3
Maintainer  : kameoka.ryota@gmail.com
Stability   : experimental

All quasiquoters defined in this module are re-exported from @Data.Time.Clock.Duration@.
-}

module Data.Time.Clock.Duration.QQ
    (
    -- * Quasiquoters
      t
    , s
    , ms
    , µs
    , ns
    , ps
    ) where

import Control.Applicative ((<|>), liftA2)
import Data.Bool (bool)
import Data.Char (isAlpha, toLower, toUpper)
import Data.Fixed (E0, E3, E6, E9, E12)
import Data.Proxy (Proxy (Proxy))
import Data.Time.Clock.Duration.Types
import Language.Haskell.TH (Exp (AppE, ConE, SigE, VarE), Name, Q, Type (AppT, ConT))
import Language.Haskell.TH.Quote (QuasiQuoter (QuasiQuoter, quoteExp))
import Text.Parsec (char, choice, digit, eof, many1, option, optional, runParser, spaces, try)
import Text.Parsec.String (Parser)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Data.Time.Clock (DiffTime, NominalDiffTime)
-- >>> import Foreign.C.Types (CSUSeconds, CUSeconds)

-- | A quasiquoter to denote a duration.
--
-- >>> [t| 42s |] :: DiffTime
-- 42s
--
-- >>> [t| 1day |] :: DiffTime
-- 86400s
--
-- The expression has the type of @'AbsoluteDuration' a => a@.
--
-- >>> [t| 1ms |] :: NominalDiffTime
-- 0.001s
--
-- >>> [t| 1ms |] :: CSUSeconds
-- 1000
--
-- >>> [t| 1ms |] :: CUSeconds
-- 1000
--
-- You can use various expressions inside the quasiquoter. (See 'Data.Time.Clock.Duration.Types.Time' for details.)
--
-- >>> [t| 1ms |] :: DiffTime
-- 0.001s
--
-- >>> [t| 1s |] :: DiffTime
-- 1s
--
-- >>> [t| 1m |] :: DiffTime
-- 60s
--
-- >>> [t| 1h |] :: DiffTime
-- 3600s
--
-- >>> [t| 1d |] :: DiffTime
-- 86400s
--
-- >>> [t| 1w |] :: DiffTime
-- 604800s
--
-- >>> [t| 1y |] :: DiffTime
-- 31536000s
t :: QuasiQuoter
t = QuasiQuoter { quoteExp = buildExp Nothing }

-- | A quasiquoter to denote a duration in seconds. Its behavior varies according to what you give
-- to the quasiquoter.
--
-- When a unitless number (like @42@) is supplied to 's', the expression has the type of @'AbsoluteDuration' a => a@.
--
-- >>> [s| 42 |] :: DiffTime
-- 42s
--
-- When you pass a string with a number and a valid unit (like @42s@), the expression has the type of
-- @'RelativeDuration' a => a@, and it represents how long the given duration is in seconds.
--
-- For example, how long is 42 minutes in seconds?
--
-- >>> [s| 42m |] :: Int
-- 2520
--
-- Note that short durations can be rounded to zero when treated as an integer.
--
-- >>> [s| 1ms |] :: Int
-- 0
-- >>> [s| 1ms |] :: Integer
-- 0
--
-- To avoid this, use 'Data.Ratio.Ratio' or 'Prelude.Float' instead.
--
-- >>> [s| 1ms |] :: Rational
-- 1 % 1000
-- >>> [s| 1ms |] :: Float
-- 1.0e-3
s :: QuasiQuoter
s = QuasiQuoter { quoteExp = buildExp (Just (Second, ''E0)) }

-- | A quasiquoter to denote a duration in milliseconds. See 's' for detailed usage.
--
-- >>> [ms| 42 |] :: DiffTime
-- 0.042s
--
-- >>> [ms| 42s |] :: Integer
-- 42000
ms :: QuasiQuoter
ms = QuasiQuoter { quoteExp = buildExp (Just (Millisec, ''E3)) }

-- | A quasiquoter to denote a duration in microseconds. See 's' for detailed usage.
--
-- >>> [µs| 42 |] :: DiffTime
-- 0.000042s
--
-- >>> [µs| 42s |] :: Integer
-- 42000000
µs :: QuasiQuoter
µs = QuasiQuoter { quoteExp = buildExp (Just (Microsec, ''E6)) }

-- | A quasiquoter to denote a duration in nanoseconds. See 's' for detailed usage.
--
-- >>> [ns| 42 |] :: DiffTime
-- 0.000000042s
--
-- >>> [ns| 42s |] :: Integer
-- 42000000000
ns :: QuasiQuoter
ns = QuasiQuoter { quoteExp = buildExp (Just (Nanosec, ''E9)) }

-- | A quasiquoter to denote a duration in picoseconds. See 's' for detailed usage.
--
-- >>> [ps| 42 |] :: DiffTime
-- 0.000000000042s
--
-- >>> [ps| 42s |] :: Integer
-- 42000000000000
ps :: QuasiQuoter
ps = QuasiQuoter { quoteExp = buildExp (Just (Picosec, ''E12)) }

buildExp :: Maybe ((Rational -> Time), Name) -> String -> Q Exp
buildExp munit str =
    let duration = runParser durationP () "" str `catch` (error . show) in
    case (munit, duration) of
        -- [s| 42m |]
        (Just (_, name), WithUnit time) -> do
           timeE <- [|time|]
           pure $ AppE (AppE (VarE 'toRelativeDuration) (SigE (ConE 'Proxy) (AppT (ConT ''Proxy) (ConT name)))) timeE
        -- [s| 42 |]
        (Just (constructor, _), WithoutUnit time) ->
           let time' = constructor time
            in [|toAbsoluteDuration time'|]
        -- [t| 42m |]
        (Nothing, WithUnit time) -> [|toAbsoluteDuration time|]
        -- [t| 42 |]
        (Nothing, WithoutUnit _) -> error "not defined"
  where
    catch (Left err) f = f err
    catch (Right x) _ = x

digits :: Int -> Maybe Int
digits n
    | n > 0 = Just . length . show $ n
    | otherwise = Nothing

decimalP :: Parser Rational
decimalP = do
    isNegative <- option False $ const True <$> char '-'
    int <- read <$> option "0" (many1 digit)
    frac <- option 0 $ do
        _ <- char '.'
        n <- many1 digit
        pure $ read n

    let int' = fromIntegral int
        frac' = fromIntegral frac

    pure . bool id negate isNegative $ case digits frac of
        Nothing -> int'
        Just n -> int' + (frac' / 10^n)

withSuffix :: String -> Parser Rational
withSuffix suffix = do
    optional spaces
    decimal <- decimalP
    optional spaces
    _ <- stringCI suffix
    optional spaces
    eof
    pure decimal

withSuffixes :: [String] -> Parser Rational
withSuffixes = choice . map (try . withSuffix)

stringCI :: String -> Parser String
stringCI = foldr (liftA2 (:) . charCI) $ pure ""
  where
    charCI :: Char -> Parser Char
    charCI c
        | isAlpha c = char (toUpper c) <|> char (toLower c)
        | otherwise = char c

relativeP :: Parser Rational
relativeP = withSuffix ""

millisecondsP, secondsP, minutesP, hoursP, daysP, weeksP, yearsP :: Parser Time
millisecondsP = Millisec <$> withSuffixes ["ms", "msec", "msecs", "millisecond", "milliseconds"]
secondsP      = Second   <$> withSuffixes ["s", "sec", "secs", "second", "seconds"]
minutesP      = Minute   <$> withSuffixes ["m", "min", "mins", "minute", "minutes"]
hoursP        = Hour     <$> withSuffixes ["h", "hr", "hrs", "hour", "hours"]
daysP         = Day      <$> withSuffixes ["d", "day", "days"]
weeksP        = Week     <$> withSuffixes ["w", "week", "weeks"]
yearsP        = Year     <$> withSuffixes ["y", "yr", "yrs", "year", "years"]

-- | The underlying 'parsec' parser.
absoluteP :: Parser Time
absoluteP = choice $ map try parsers
  where
    parsers =
        [ millisecondsP
        , secondsP
        , minutesP
        , hoursP
        , daysP
        , weeksP
        , yearsP
        ]

data Duration
   = WithoutUnit Rational
   | WithUnit Time

durationP :: Parser Duration
durationP = (WithUnit <$> absoluteP) <|> (WithoutUnit <$> relativeP)
