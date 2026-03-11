-- SPDX-FileCopyrightText: 2022 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE CPP #-}

module Data.Time.TZTime.Internal where

import Control.Applicative (optional)
import Control.DeepSeq (NFData)
import Control.Exception.Safe (Exception(..), MonadThrow, throwM)
import Control.Monad.Except (MonadError, throwError)
import Data.Data (Data)
import Data.Fixed (Fixed(..), Pico)
import Data.Function ((&))
import Data.Functor (void, (<&>))
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Text qualified as T
import Data.Time (UTCTime, addUTCTime, secondsToNominalDiffTime)
import Data.Time qualified as Time
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Time.Compat (pattern YearMonthDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Time.LocalTime
import Data.Time.TZInfo (TZIdentifier, TZInfo(..), fromIdentifier)
import Data.Time.Zones (LocalToUTCResult(..))
import Data.Time.Zones qualified as TZ
import GHC.Generics (Generic)
import GHC.Records (HasField(..))
import GHC.Stack (HasCallStack)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadP qualified as P

import Language.Haskell.TH.Syntax (Q, liftTyped)
#if MIN_VERSION_template_haskell(2,17,0)
import Language.Haskell.TH.Syntax (Code, Quote)
#else
import Language.Haskell.TH.Syntax (TExp)
#endif

{-# ANN module ("HLint: ignore Use fewer imports" :: String) #-}

----------------------------------------------------------------------------
-- TZTime
----------------------------------------------------------------------------

-- | A valid and unambiguous point in time in some time zone.
data TZTime = UnsafeTZTime
  { tztLocalTime :: LocalTime
  , tztTZInfo :: TZInfo
  , tztOffset :: Time.TimeZone
  }
  deriving stock (Eq, Data, Generic)
  deriving anyclass NFData

{- |
@yyyy-mm-dd hh:mm:ss[.sss] ±hh:mm [time zone]@.
Example: @2022-03-04 02:02:01 +01:00 [Europe/Rome]@.
-}
instance Show TZTime where
  show (UnsafeTZTime lt tzi offset) =
    show lt <> " " <> iso8601Show offset <> " [" <> tzIdent <> "]"
    where
      tzIdent = T.unpack $ tziIdentifier tzi

----------------------------------------------------------------------------
-- TZTime fields
----------------------------------------------------------------------------
{-
Note: We do not want users to be able to unsafely modify `TZTime`'s fields.

For that reason, the `Data.Time.TZTime` module does
not export its fields`; it exports functions like `tzTimeLocalTime` instead.

We also export `HasField` instances for compatibility with `OverloadedRecordDot`.

>>> import Data.Time.TZInfo as TZInfo
>>> tz = fromPOSIXTime (TZInfo.fromLabel TZInfo.Europe__Rome) 0
>>>
>>> :set -XOverloadedRecordDot
>>> tz.tzTimeLocalTime
1970-01-01 01:00:00

## WARNING! ##

According to <https://gitlab.haskell.org/ghc/ghc/-/wikis/records/overloaded-record-fields>,
there are plans to add `setField` to the `HasField` class, which could then
be used to implement `OverloadedRecordUpdate`.
This conflicts with our intent: we only want to support `OverloadedRecordDot`,
but NOT `OverloadedRecordUpdate`!

There are also proposals to split the `HasField` class in two.

If `setField` is indeed added to the `HasField` class, we'll have to drop these instances.
If the `HasField` class is split in two, that's not a problem.
-}

-- | The local time of this `TZTime`.
tzTimeLocalTime :: TZTime -> LocalTime
tzTimeLocalTime = tztLocalTime

-- | The time zone of this `TZTime`.
tzTimeTZInfo :: TZTime -> TZInfo
tzTimeTZInfo = tztTZInfo

-- | The offset observed in this time zone at this moment in time.
tzTimeOffset :: TZTime -> TimeZone
tzTimeOffset = tztOffset

-- | @since 0.1.1.0
instance HasField "tzTimeLocalTime" TZTime LocalTime where getField = tzTimeLocalTime
-- | @since 0.1.1.0
instance HasField "tzTimeTZInfo" TZTime TZInfo where getField = tzTimeTZInfo
-- | @since 0.1.1.0
instance HasField "tzTimeOffset" TZTime TimeZone where getField = tzTimeOffset

----------------------------------------------------------------------------
-- Constructors
----------------------------------------------------------------------------

-- | Converts a `UTCTime` to the given time zone.
fromUTC :: TZInfo -> UTCTime -> TZTime
fromUTC tzi utct =
  UnsafeTZTime
    { tztLocalTime = TZ.utcToLocalTimeTZ (tziRules tzi) utct
    , tztTZInfo = tzi
    , tztOffset = TZ.timeZoneForUTCTime (tziRules tzi) utct
    }

-- | Converts a `POSIXTime` to the given time zone.
fromPOSIXTime :: TZInfo -> POSIXTime -> TZTime
fromPOSIXTime tzi = fromUTC tzi . posixSecondsToUTCTime

-- | Converts a `ZonedTime` to UTC and then to the given time zone.
fromZonedTime :: TZInfo -> ZonedTime -> TZTime
fromZonedTime tzi = fromUTC tzi . zonedTimeToUTC

-- | Attempted to construct a `TZTime` from an invalid or ambiguous `LocalTime`.
data TZError
  = TZOverlap
      LocalTime
      -- ^ The `LocalTime` is ambiguous.
      -- This usually happens when the clocks are set back in
      -- autumn and a local time happens twice.
      ~TZTime -- ^ The first occurrence of the given `LocalTime`, at the earliest offset.
      ~TZTime -- ^ The second occurrence of the given `LocalTime`, at the latest offset.
  | TZGap
      LocalTime
      -- ^ The `LocalTime` is invalid.
      -- This usually happens when the clocks are set forward in
      -- spring and a local time is skipped.
      ~TZTime -- ^ The given `LocalTime` adjusted back by the length of the gap.
      ~TZTime -- ^ The given `LocalTime` adjusted forward by the length of the gap.
  deriving stock (Eq, Data, Generic)
  deriving anyclass (NFData)

instance Show TZError where
  show = displayException

instance Exception TZError where
  displayException = \case
    TZGap lt tzt1 _ ->
      "The local time "
      <> show lt
      <> " is invalid in the time zone "
      <> show (tziIdentifier $ tztTZInfo tzt1)
      <> "."
    TZOverlap lt tzt1 tzt2 ->
      "The local time "
      <> show lt
      <> " is ambiguous in the time zone "
      <> show (tziIdentifier $ tztTZInfo tzt1)
      <> ": it is observed at the offsets "
      <> iso8601Show (tzTimeOffset tzt1)
      <> " and "
      <> iso8601Show (tzTimeOffset tzt2)
      <> "."

-- | Similar to `fromLocalTime`, but returns a `TZError`
-- if the local time is ambiguous/invalid.
fromLocalTimeStrict :: MonadError TZError m => TZInfo -> LocalTime -> m TZTime
fromLocalTimeStrict tzi lt =
  case TZ.localTimeToUTCFull (tziRules tzi) lt of
    LTUUnique _utc namedOffset ->
      pure $ UnsafeTZTime lt tzi namedOffset
    LTUAmbiguous _utc1 _utc2 namedOffset1 namedOffset2 ->
      throwError $ TZOverlap lt
        (UnsafeTZTime lt tzi namedOffset1)
        (UnsafeTZTime lt tzi namedOffset2)
    -- Note: LTUNone means the given `LocalTime` is invalid and lands on a "gap".
    -- The constructor contains:
    -- 1. The `UTCTime` corresponding to the `LocalTime` shifted forward by the duration of the gap.
    --    E.g., if it's a 1-hour gap, this will be the same as "toUTC (localTime + 1 hour)"
    -- 2. The offset observed in that time zone before the clocks changed.
    --
    -- From these 2 pieces of information, we can figure out the rest.
    --
    -- This approach works but is inefficient.
    -- TODO: reimplement parts of `localTimeToUTCFull` to make this more efficient.
    LTUNone utcAfter offsetBefore ->
      let
        offsetAfter = TZ.timeZoneForUTCTime (tziRules tzi) utcAfter
        gap = secondsToNominalDiffTime $ 60 *
          fromIntegral @Int @Pico (timeZoneMinutes offsetAfter - timeZoneMinutes offsetBefore)
        utcBefore = addUTCTime (- gap) utcAfter
      in
        throwError $ TZGap lt
          (UnsafeTZTime (TZ.utcToLocalTimeTZ (tziRules tzi) utcBefore) tzi offsetBefore)
          (UnsafeTZTime (TZ.utcToLocalTimeTZ (tziRules tzi) utcAfter) tzi offsetAfter)

-- | Constructs a `TZTime` from a local time in the given time zone.
--
-- * If the local time lands on a "gap" (e.g. when the clocks are set forward in spring and a local time is skipped),
--   we shift the time forward by the duration of the gap.
-- * If it lands on an "overlap" (e.g. when the clocks are set back in autumn and a local time happens twice),
--   we use the earliest offset.
fromLocalTime :: TZInfo -> LocalTime -> TZTime
fromLocalTime tzi lt =
  case fromLocalTimeStrict tzi lt of
    Right tzt -> tzt
    Left (TZGap _ _ after) -> after
    Left (TZOverlap _ atEarliestOffset _) -> atEarliestOffset

-- | Similar to `fromLocalTime`, but throws a `TZError` in `MonadThrow`
-- if the local time is ambiguous/invalid.
fromLocalTimeThrow :: MonadThrow m => TZInfo -> LocalTime -> m TZTime
fromLocalTimeThrow tzi =
  either throwM pure . fromLocalTimeStrict tzi

-- | Similar to `fromLocalTime`, but throws an `error`
-- if the local time is ambiguous/invalid.
unsafeFromLocalTime :: HasCallStack => TZInfo -> LocalTime -> TZTime
unsafeFromLocalTime tzi lt =
  case fromLocalTimeStrict tzi lt of
    Right tzt -> tzt
    Left err -> error $ "unsafeFromLocalTime: " <> displayException err

----------------------------------------------------------------------------
-- Conversions
----------------------------------------------------------------------------

-- | Converts this moment in time to the universal time-line.
toUTC :: TZTime -> UTCTime
toUTC tzt =
  localTimeToUTC (tzTimeOffset tzt) (tzTimeLocalTime tzt)

-- | Converts this moment in time to a POSIX timestamp.
toPOSIXTime :: TZTime -> POSIXTime
toPOSIXTime = utcTimeToPOSIXSeconds . toUTC

-- | Converts this moment in time to a `ZonedTime` (discarding time zone rules).
toZonedTime :: TZTime -> ZonedTime
toZonedTime tzt = ZonedTime (tzTimeLocalTime tzt) (tzTimeOffset tzt)

-- | Converts this moment in time to some other time zone.
inTZ :: TZInfo -> TZTime -> TZTime
inTZ tzi = fromUTC tzi . toUTC

----------------------------------------------------------------------------
-- Modifying a TZTime
----------------------------------------------------------------------------

-- | Modify this moment in time along the universal time-line.
modifyUniversalTimeLine :: (UTCTime -> UTCTime) -> TZTime -> TZTime
modifyUniversalTimeLine f tzt =
  fromUTC (tzTimeTZInfo tzt) . f . toUTC $ tzt

-- | Modify this moment in time along the local time-line.
modifyLocalTimeLine :: MonadError TZError m => (LocalTime -> LocalTime) -> TZTime -> m TZTime
modifyLocalTimeLine f tzt =
  fromLocalTimeStrict (tzTimeTZInfo tzt) . f . tzTimeLocalTime $ tzt

----------------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------------

{- |
@yyyy-mm-dd hh:mm:ss[.sss] [±hh:mm] [time zone]@.
Example: @2022-03-04 02:02:01 +01:00 [Europe/Rome]@.

The offset is optional, except when the local time is ambiguous
(i.e. when the clocks are set forward around that time in that time zone).

The offset can also be expressed using [military time zone abbreviations](https://www.timeanddate.com/time/zones/military),
and these time zones abbreviations as per RFC 822 section 5:
\"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\".

Note: the time zone's rules are loaded from the embedded database using `fromIdentifier`.
-}
instance Read TZTime where
  readsPrec _ input = do
    ((lt, offsetMaybe, ident), input) <- P.readP_to_S readComponentsP input
    case getValidTZTimes lt ident >>= checkOffset offsetMaybe of
      Nothing -> []
      Just (tzt :| []) -> [(tzt, input)]
      Just (tzts) -> NE.toList tzts <&> \tzt -> (tzt, input)

readComponentsP :: ReadP (LocalTime, Maybe Time.TimeZone, TZIdentifier)
readComponentsP =
  (,,)
    <$> (P.readS_to_P $ reads @LocalTime)
    <*> (optional $ P.readS_to_P $ reads @Time.TimeZone)
    <*> readTZIdentP

readTZIdentP :: ReadP TZIdentifier
readTZIdentP = do
  P.skipSpaces
  void $ P.char '['
  fromString @TZIdentifier <$> P.manyTill P.get (P.char ']')

-- | Try to construct a `TZTime` from the given components.
getValidTZTimes :: MonadFail m => LocalTime -> TZIdentifier -> m (NonEmpty TZTime)
getValidTZTimes lt ident = do
  tzi <- case fromIdentifier ident of
    Nothing -> fail $ "Unknown time zone: '" <> T.unpack ident <> "'"
    Just tzi -> pure tzi
  case fromLocalTimeStrict tzi lt of
    Right tzt -> pure $ tzt :| []
    Left (TZOverlap _ tzt1 tzt2) -> pure $ tzt1 :| [tzt2]
    Left (TZGap _ tzt1 tzt2) ->
      fail $ "Invalid time: the clocks are set forward around this time.\n" <> mkSuggestions (tzt1 :| [tzt2])

-- | If the user specified an offset, check that it matches at least one of the valid `TZTime`s.
checkOffset :: MonadFail m => Maybe Time.TimeZone -> NonEmpty TZTime -> m (NonEmpty TZTime)
checkOffset offsetMaybe tzts =
  case offsetMaybe of
    Nothing -> pure tzts
    Just offset ->
      tzts
        & NE.filter (\tzt -> timeZoneMinutes offset == timeZoneMinutes (tzTimeOffset tzt))
        & NE.nonEmpty
        & \case
            Just validTzts -> pure validTzts
            Nothing -> fail $ "Invalid offset: " <> iso8601Show offset <> "\n" <> mkSuggestions tzts

mkSuggestions :: NonEmpty TZTime -> String
mkSuggestions tzts =
  "      Did you mean any of the following?" <> foldMap (\tzt -> "\n      - " <> show tzt) tzts

----------------------------------------------------------------------------
-- Template Haskell
----------------------------------------------------------------------------

readP_to_Q :: String -> ReadP a -> Q a
readP_to_Q input parser =
  case P.readP_to_S (parser <* P.eof) input of
    [] -> fail $ "Failed to parse: '" <> input <> "'"
    [(res, _)] -> pure res
    _ -> fail $ "Parsing is ambiguous: '" <> input <> "'"

#if MIN_VERSION_template_haskell(2,17,0)
liftTZTime :: Quote m => TZTime -> Code m TZTime
liftLocalTime :: Quote m => LocalTime -> Code m LocalTime
liftTimeZone :: Quote m => Time.TimeZone -> Code m Time.TimeZone
#else
liftTZTime :: TZTime -> Q (TExp TZTime)
liftTimeZone :: Time.TimeZone -> Q (TExp Time.TimeZone)
liftLocalTime :: LocalTime -> Q (TExp LocalTime)
#endif

-- | NOTE: this assumes the time zone identifier used to construct `TZTime` exists in the
-- embedded time zone database, i.e. it can be loaded using `fromIdentifier`.
liftTZTime tzt =
  [e||
    UnsafeTZTime
      $$(liftLocalTime $ tzTimeLocalTime tzt)
      (fromJust $ fromIdentifier $$(liftTyped ident))
      $$(liftTimeZone $ tzTimeOffset tzt)
  ||]
  where
    ident = tziIdentifier $ tzTimeTZInfo tzt

liftLocalTime (LocalTime (YearMonthDay yy mm dd) (TimeOfDay hh mmm (MkFixed ss))) =
  [e||
    LocalTime
      (YearMonthDay $$(liftTyped yy) $$(liftTyped mm) $$(liftTyped dd))
      (TimeOfDay $$(liftTyped hh) $$(liftTyped mmm) (MkFixed $$(liftTyped ss)))
  ||]

liftTimeZone (TimeZone tzMins tzSummer tzName) =
  [e|| TimeZone $$(liftTyped tzMins) $$(liftTyped tzSummer) $$(liftTyped tzName) ||]
