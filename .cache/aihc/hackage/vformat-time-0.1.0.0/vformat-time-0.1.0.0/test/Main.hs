{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import           Control.Exception

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Text.Format
import           Text.Format.Time    hiding (formatTime)

main :: IO ()
main = do
  utc <- getCurrentTime
  zoned <- getZonedTime
  let tz = zonedTimeZone zoned
      locale = utcToLocalTime tz utc
      universal = read (show locale) :: UniversalTime
      day = localDay locale
      time = localTimeOfDay locale

  putStrLn "\n<<<<<<<<<<<<<<<<<<<<<< Test <<<<<<<<<<<<<<<<<<<<<<"

  it "UTCTime default specs" $
    format "{}" utc == formatTimeDef "%F %T%Q" utc
  it "ZonedTime default specs" $
    format "{}" zoned == formatTimeDef "%F %T%Q %Z" zoned
  it "LocalTime default specs" $
    format "{}" locale == formatTimeDef "%F %T%Q" locale
  it "UniversalTime default specs" $
    format "{}" universal == formatTimeDef "%F %T%Q" universal
  it "Day default specs" $
    format "{}" day == formatTimeDef "%F" day
  it "TimeZone default specs" $
    format "{}" tz == formatTimeDef "%Z" tz
  it "Customize specs" $
    format "{:%Y-%m-%d}" utc == formatTimeDef "%Y-%m-%d" utc

  putStrLn "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"


it :: String -> Bool -> IO ()
it = flip assert . putStrLn


formatTimeDef :: FormatTime t => String -> t -> String
formatTimeDef = formatTime defaultTimeLocale
