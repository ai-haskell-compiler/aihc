{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Time.LocalTime.TimeZone.Olson.Render
-- Copyright   :  Yitzchak Gale 2019
--
-- Maintainer  :  Yitzchak Gale <gale@sefer.org>
-- Portability :  portable
--
-- Render Olson timezone data in the standard binary format used in
-- Olson timezone files, as specified in RFC 8536.

{- Copyright (c) 2019 Yitzchak Gale. All rights reserved.
For licensing information, see the BSD3-style license in the file
LICENSE that was originally distributed by the author together with
this file. -}

module Data.Time.LocalTime.TimeZone.Olson.Render
(
 -- * Rendering Olson timezone files
 -- | If any of the transition times or leap second times specified
 -- require more than a 32-bit integer to represent as a Unix
 -- timestamp, or if a POSIX-style TZ string is specified, timezone
 -- data is rendered using Version 2 format. Otherwise, the timezone data
 -- is rendered using Version 1 format.
 renderTimeZoneSeriesToOlsonFile,
 timeZoneSeriesToOlson,
 renderOlsonToFile,
 verifyOlsonLimits,
 putOlson,
 extractOlsonV1
)
where

import Data.Time.LocalTime.TimeZone.Olson.Types
import Data.Time.LocalTime.TimeZone.Series (TimeZoneSeries(TimeZoneSeries))
import Data.Time (TimeZone(TimeZone, timeZoneSummerOnly, timeZoneName))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.Binary.Put (Put, runPut, putByteString, putWord8, flush,
                        putWord32be, putWord64be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (partition, sortBy, sort, group)
import Data.Ord (comparing)
import Data.Word (Word8)
import Data.Maybe (listToMaybe, maybeToList, isNothing, fromMaybe, catMaybes)
import Data.Monoid (mempty)
import Control.Monad (guard, replicateM_, unless)

-- | Render a @TimeZoneSeries@ as a binary Olson timezone file.
--
-- If the values in the Olson timezone data exceed the standard size
-- limits (see 'defaultLimits'), this function throws an
-- exception. For other behavior, use 'timeZoneSeriesToOlson',
-- 'verifyOlsonLimits', 'putOlson' and 'Data.Binary.Put.runPut'
-- directly.
renderTimeZoneSeriesToOlsonFile :: FilePath -> TimeZoneSeries -> IO ()
renderTimeZoneSeriesToOlsonFile fp = renderOlsonToFile fp .
  fromMaybe (error "Cannot render TimeZoneSeries: default is summer time") .
  timeZoneSeriesToOlson

-- | Convert a @TimeZoneSeries@ to @OlsonData@ for rendering.
timeZoneSeriesToOlson :: TimeZoneSeries -> Maybe OlsonData
timeZoneSeriesToOlson (TimeZoneSeries dflt pairs)
 | timeZoneSummerOnly dflt && not (all timeZoneSummerOnly $ map snd pairs)
             = Nothing
 | otherwise = Just $
    OlsonData
      [Transition secs ttinfo |
         (t, tz) <- reverse pairs,
         let secs = round $ utcTimeToPOSIXSeconds t,
         ttinfo <- maybeToList $ lookup (mkTT tz) ttAssocs]
      ttinfos
      []
      Nothing
  where
    mkTT (TimeZone offset isdst abbr) =
      TtInfo (offset*60) isdst Wall abbr
    dfltTT = mkTT dflt
    ttAssocs = (dfltTT, 0) :
      zip (uniq . sort . filter (/= dfltTT) $ map (mkTT . snd) pairs) [1..]
    ttinfos = map fst ttAssocs

-- | Check whether @OlsonData@ is within size limits.
verifyOlsonLimits :: SizeLimits -> OlsonData -> Bool
verifyOlsonLimits limits (OlsonData transs ttinfos leaps _) =
    withinLimit maxTimes transs &&
    withinLimit maxTypes ttinfos &&
    withinLimit maxLeaps leaps &&
    withinLimit maxAbbrChars abbrChars
  where
    withinLimit limit items = maybe True (null . flip drop items) $
                              limit limits
    abbrChars = concat abbrStrs ++ map (const '\NUL') abbrStrs
    abbrStrs = map tt_abbr ttinfos

-- | Render Olson timezone data as a binary Olson timezone file
--
-- If the values in the Olson timezone data exceed the standard size
-- limits (see 'defaultLimits'), this function throws an
-- exception. For other behavior, use 'verifyOlsonLimits', 'putOlson'
-- and 'Data.Binary.Put.runPut' directly.
renderOlsonToFile :: FilePath -> OlsonData -> IO ()
renderOlsonToFile fp olson = do
  unless (verifyOlsonLimits defaultLimits olson) $
    error "Olson timezone data exceeds size limits"
  L.writeFile fp . runPut . putOlson $ olson

-- | Render Olson timezone data in binary Olson timezone file format
-- as a lazy @ByteString@
putOlson olson@(OlsonData _ _ _ posix)
  | fitsInVersion1 =    putOlsonPart 0  put32bitIntegral olson
  | otherwise      = do putOlsonPart 50 put32bitIntegral olson1
                        putOlsonPart 50 put64bitIntegral olson
                        putPosixTZ posix
  where
    olson1 = extractOlsonV1 olson
    fitsInVersion1 = fmap null posix /= Just False &&
                     olson1 == olson {olsonPosixTZ = Nothing}

-- | Extract Olson timezone data that can be rendered using Version 1 format
extractOlsonV1 :: OlsonData -> OlsonData
extractOlsonV1 (OlsonData transs  ttinfos  leaps  _)
  | allV1     = OlsonData transs  ttinfos  leaps  Nothing
  | otherwise = OlsonData transs1 ttinfos1 leaps1 Nothing
  where
    cutoff = 0x80000000 -- 2^31
    fitsIn32bits x = x < cutoff && x >= negate cutoff
    leaps1   = filter (fitsIn32bits .  leapTime)  leaps
    transs1' = filter (fitsIn32bits . transTime) transs
    allV1 = length leaps1 == length leaps && length transs1' == length transs
    assoc1 = zip
      (sortBy (comparing $ fmap tt_ttype . listToMaybe . flip drop ttinfos) .
        uniq . sort . (0 :) $ map transIndex transs1')
      [0..]
    transs1 = [t {transIndex = i} |
      t <- transs1', i <- maybeToList $ lookup (transIndex t) assoc1]
    ttinfos1 = map snd . dropWhile (isNothing . fst) .
      sortBy (comparing fst) $ zip (map (flip lookup assoc1) [0..]) ttinfos

putOlsonPart :: Word8 -> (Integer -> Put) -> OlsonData -> Put
putOlsonPart version putTime (OlsonData transs ttinfos leaps _) = do
    putASCII "magic number" "TZif"
    putWord8 version
    putByteString . B.pack $ replicate 15 0 -- padding nulls
    replicateM_ 2 $ putCount ttinfosWithTtype
                               -- tzh_ttisutcnt
                               -- tzh_ttisstdcnt
    putCount leaps             -- tzh_leapcnt
    putCount transs            -- tzh_timecnt
    putCount ttinfos           -- tzh_typecnt
    put32bitIntegral abbrChars -- tzh_charcnt
    mapM_ (putTime         . transTime ) transs
    mapM_ (put8bitIntegral . transIndex) transs
    mapM_ putTtInfo ttinfosIndexed
    mapM_ putAbbr abbrStrings
    mapM_ (putLeapInfo putTime) leaps
    mapM_ (putBool . (== Std) . tt_ttype) ttinfosWithTtype -- isstd
    mapM_ (putBool . (== UTC) . tt_ttype) ttinfosWithTtype -- isut
  where
    putCount = put32bitIntegral . length
    ttinfosWithTtype = takeWhile ((<= UTC) . tt_ttype) ttinfosIndexed
    abbrStrings = uniq . sort $ map tt_abbr ttinfos
    abbrChars = sum (map length abbrStrings) + length abbrStrings
    putAbbr abbr = putASCII "time zone abbreviation" abbr >> putWord8 0
    abbrAssocs = zip abbrStrings . scanl (+) 0 $
                 map ((+ 1) . length) abbrStrings
    ttinfosIndexed = [TtInfo utoff isdst ttype i |
      TtInfo utoff isdst ttype abbr <- ttinfos,
      i <- maybeToList $ lookup abbr abbrAssocs]

putPosixTZ :: Maybe String -> Put
putPosixTZ posix = do
  putWord8 10
  putASCII "POSIX TZ string"$ fromMaybe "" posix
  putWord8 10

putTtInfo :: TtInfo Int -> Put
putTtInfo tt = do
    put32bitIntegral $ tt_utoff tt
    putBool $ tt_isdst tt
    put8bitIntegral $ tt_abbr tt

putLeapInfo :: Integral a => (a -> Put) -> LeapInfo -> Put
putLeapInfo putTime leap = do
    putTime . fromIntegral $ leapTime leap
    put32bitIntegral $ leapOffset leap

-- Converting signed integrals to unsigned can be done directly,
-- without the care needed for the opposite direction when parsing.

put8bitIntegral :: Integral a => a -> Put
put8bitIntegral = putWord8 . fromIntegral

put32bitIntegral :: Integral a => a -> Put
put32bitIntegral = putWord32be . fromIntegral

put64bitIntegral :: Integral a => a -> Put
put64bitIntegral = putWord64be . fromIntegral

putBool :: Bool -> Put
putBool False = putWord8 0
putBool True  = putWord8 1

uniq :: Eq a => [a] -> [a]
uniq = map head . group

putASCII :: String -> String -> Put
putASCII what =
    putByteString . B.pack . map (fromIntegral . verify . fromEnum)
  where
    verify c
     | c >= 32 && c <= 126 = c
     | otherwise           = error $ "Cannot render TimeZoneSeries: " ++
                               what ++ " contains non-ASCII characters"
