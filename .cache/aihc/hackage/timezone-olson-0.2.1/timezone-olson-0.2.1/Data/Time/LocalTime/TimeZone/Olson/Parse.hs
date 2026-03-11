{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Olson.Parse
-- Copyright   :  Yitzchak Gale 2019
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- A parser for binary Olson timezone files whose format is specified
-- in RFC 8536. Functions are provided for converting the parsed data
-- into 'TimeZoneSeries' objects.

{- Copyright (c) 2019 Yitzchak Gale. All rights reserved.
For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file. -}

module Data.Time.LocalTime.TimeZone.Olson.Parse
(
 -- * Parsing Olson timezone files
 getTimeZoneSeriesFromOlsonFile,
 getOlsonFromFile,
 olsonToTimeZoneSeries,
 getOlson,
 OlsonError
)
where

import Data.Time.LocalTime.TimeZone.Olson.Types
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(..))
import Data.Time (TimeZone(TimeZone))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Binary.Get (Get, runGet, getWord8, getWord32be, getWord64be,
                        getByteString, getRemainingLazyByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid (mappend)
import Data.List (sortBy, groupBy)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Word (Word8)
import Data.Int (Int32, Int64)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Typeable (Typeable)
import Control.Monad (guard, replicateM, replicateM_, when)
import Control.Exception.Extensible (try, throw, Exception, ErrorCall)

-- | An exception indicating that the binary data being parsed was not
-- valid Olson timezone data
data OlsonError = OlsonError String
  deriving Typeable
instance Show OlsonError where
  show (OlsonError msg) = msg
instance Exception OlsonError

-- | Convert parsed Olson timezone data into a @TimeZoneSeries@.
olsonToTimeZoneSeries :: OlsonData -> Maybe TimeZoneSeries
olsonToTimeZoneSeries (OlsonData ttimes ttinfos@(dflt0:_) _ _) =
    fmap (TimeZoneSeries $ mkTZ dflt) . mapM (lookupTZ ttinfos) .
    uniqTimes . sortBy futureToPast $ ttimes
  where
    dflt = fromMaybe dflt0 . listToMaybe $ filter isStd ttinfos
    isStd (TtInfo _ isdst _ _) = not isdst
    mkTZ (TtInfo utoff isdst _ abbr) =
      TimeZone ((utoff + 30) `div` 60) isdst abbr
    lookupTZ ttinfos ttime = fmap (((,) $ toUTC ttime) . mkTZ) . listToMaybe $
                             drop (transIndex ttime) ttinfos
    toUTC = posixSecondsToUTCTime . fromIntegral . transTime
    uniqTimes = map last . groupBy ((==) `on` transTime)
    futureToPast = comparing $ negate . transTime
olsonToTimeZoneSeries _ = Nothing

-- | Read timezone data from a binary Olson timezone file and convert
-- it into a @TimeZoneSeries@ for use together with the types and
-- fucntions of "Data.Time". This is the function from this module
-- for which you are most likely to have use.
--
-- If the values in the Olson timezone file exceed the standard size
-- limits (see 'defaultLimits'), this function throws an
-- exception. For other behavior, use 'getOlson' and
-- 'Data.Binary.Get.runGet' directly.
getTimeZoneSeriesFromOlsonFile :: FilePath -> IO TimeZoneSeries
getTimeZoneSeriesFromOlsonFile fp = getOlsonFromFile fp >>=
  maybe (throwOlson fp "no timezone found in OlsonData") return .
    olsonToTimeZoneSeries

-- | Parse a binary Olson timezone file.
--
-- If the values in the Olson timezone file exceed the standard size
-- limits (see 'defaultLimits'), this function throws an
-- exception. For other behavior, use 'getOlson' and
-- 'Data.Binary.Get.runGet' directly.
getOlsonFromFile :: FilePath -> IO OlsonData
getOlsonFromFile fp = do
    e <- try . fmap (runGet $ getOlson defaultLimits) $ L.readFile fp
    either (formatError fp) return e

formatError :: FilePath -> ErrorCall -> IO a
formatError fp e = throwOlson fp $ show e

-- | A binary parser for binary Olson timezone files
getOlson :: SizeLimits -> Get OlsonData
getOlson limits = do
    (version, part1) <- getOlsonPart True limits get32bitInteger
    -- There is one part for Version 1 format data, and two parts and a POSIX
    -- TZ string for Version 2 or 3 format data
    case () of
      _ | version == 0 -> return part1
        | version == 50 || version == 51 -> do
            (_, part2) <- getOlsonPart False limits get64bitInteger
            posixTZ <- getPosixTZ
            return $ part1 `mappend` part2 `mappend` posixTZ
        | otherwise -> do
            let msg = "getOlson: invalid tzfile version " ++ toASCII [version]
            verify (const False) msg undefined

-- Parse the part of an Olson file that contains timezone data

-- We are lenient about invalid values of @isstdcnt@ and @isutcnt@.
-- Before RFC 8536, we ignored transitions that did not have a
-- corresponding value for both @isstd@ and @isut@.  Staring with RFC
-- 8536, when zero became a valid value for @isstdcont@ and @isutcnt@,
-- we extend @isstds@ and @isuts@ with default values if they are too
-- short. Thanks to Github user @mniip@ for suggesting this change.
getOlsonPart :: Integral a => Bool -> SizeLimits -> Get a ->
                Get (Word8, OlsonData)
getOlsonPart verifyMagic limits getTime = do
    magic <- fmap (toASCII . B.unpack) $ getByteString 4
    when verifyMagic $ verify_ (== "TZif") "missing magic number" magic
    version <- getWord8
    replicateM_ 15 getWord8 -- padding nulls
    tzh_ttisutcnt <- get32bitInt
    tzh_ttisstdcnt <- get32bitInt
    tzh_leapcnt <- get32bitInt
      >>= verify (withinLimit maxLeaps) "too many leap second specifications"
    tzh_timecnt <- get32bitInt
      >>= verify (withinLimit maxTimes) "too many timezone transitions"
    tzh_typecnt <- get32bitInt
      >>= verify (withinLimit maxTypes) "too many timezone type specifications"
    verify (withinLimit maxTypes) "too many isut specifiers" tzh_ttisutcnt
    verify (withinLimit maxTypes) "too many isstd specifiers" tzh_ttisstdcnt
    tzh_charcnt <- get32bitInt
      >>= verify (withinLimit maxAbbrChars) "too many tilezone specifiers"
    times <- fmap (map toInteger) $ replicateM tzh_timecnt getTime
    indexes <- replicateM tzh_timecnt get8bitInt
    ttinfos <- replicateM tzh_typecnt getTtInfo
    abbr_chars <- fmap (toASCII . B.unpack) $ getByteString tzh_charcnt
    leaps <- replicateM tzh_leapcnt $ getLeapInfo getTime
    isstds <- replicateM tzh_ttisstdcnt getBool
    isuts <- replicateM tzh_ttisutcnt getBool
    return
      (version,
       OlsonData
         (zipWith Transition times indexes)
         (map (flip lookupAbbr abbr_chars) . zipWith setTtype ttinfos $
           zipWith boolsToTType
             (isstds ++ repeat False) (isuts ++ repeat False)
         )
         leaps
         Nothing
      )
  where
    withinLimit limit value = maybe True (value <=) $ limit limits
    lookupAbbr (TtInfo utoff isdst ttype abbrind) =
      TtInfo utoff isdst ttype . takeWhile (/= '\NUL') . drop abbrind
    setTtype ttinfo ttype = ttinfo {tt_ttype = ttype}
    boolsToTType _     isut | isut      = UTC
    boolsToTType isstd _
                            | isstd     = Std
                            | otherwise = Wall

-- Parse a POSIX-style TZ string.
-- We don't try to understand the TZ string, we just pass it along whole.
getPosixTZ :: Get OlsonData
getPosixTZ = do
    getWord8 >>= verify (== 10)
                 "POSIX TZ string not preceded by newline"
    posixTZ <- fmap (L.takeWhile (/= 10)) getRemainingLazyByteString
    -- We don't check for the trailing newline, in order to keep it lazy
    return . OlsonData [] [] [] $ do
      guard (not $ L.null posixTZ)
      Just . toASCII $ L.unpack posixTZ

-- Parse a ttinfo struct. Each ttinfo struct corresponds to a single
-- Data.Time.TimeZone object.
getTtInfo :: Get (TtInfo Int)
getTtInfo = do
    utoff <- get32bitInt
    isdst <- getBool
    abbrind <- get8bitInt
    return $ TtInfo utoff isdst Wall abbrind

-- Parse leap second info. (usually not used)
getLeapInfo :: Integral a => Get a -> Get LeapInfo
getLeapInfo getTime = do
    lTime <- fmap toInteger getTime
    lOffset <- get32bitInt
    return $ LeapInfo lTime lOffset

-- Our 8-bit ints are unsigned, so we can convert them directly
get8bitInt :: Get Int
get8bitInt = fmap fromIntegral getWord8

getInt32 :: Get Int32
getInt32 = fmap fromIntegral getWord32be

get32bitInt :: Get Int
get32bitInt = fmap fromIntegral getInt32
                  -- via Int32 to get the sign right
                  -- in case we are on a 64-bit platform

get32bitInteger :: Get Integer
get32bitInteger = fmap fromIntegral getInt32
                  -- via Int32 to get the sign right

getInt64 :: Get Int64
getInt64 = fmap fromIntegral getWord64be

get64bitInteger :: Get Integer
get64bitInteger = fmap fromIntegral getInt64
                  -- via Int64 to get the sign right

getBool :: Get Bool
getBool = fmap (/= 0) getWord8

toASCII :: [Word8] -> String
toASCII = map (toEnum . fromIntegral)

verify :: Monad m => (a -> Bool)  -> String -> a -> m a
verify pred msg val
 | pred val  = return val
 | otherwise = error msg

verify_ :: Monad m => (a -> Bool)  -> String -> a -> m ()
verify_ pred msg val
 | pred val  = return ()
 | otherwise = error msg

throwOlson :: FilePath -> String -> IO a
throwOlson fp msg = throw . OlsonError $
                    fp ++ ": invalid timezone file: " ++ msg
