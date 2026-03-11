------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : hr: a horizontal rule for terminals
-- Copyright   : Copyright (c) 2019-2026 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

#if defined(MIN_VERSION_ansi_wl_pprint)
#if MIN_VERSION_ansi_wl_pprint (1,0,2)
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
#endif
#endif

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (many, optional)
import Data.Maybe (maybeToList)
import System.Timeout (timeout)

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

-- https://hackage.haskell.org/package/time
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

-- (horizontal-rule)
import qualified HR

-- (horizontal-rule:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Constants

defaultWidth :: Int
defaultWidth = 80

defaultFormat :: String
defaultFormat = "%Y-%m-%d %H:%M:%S"

defaultTimeout :: Int
defaultTimeout = 500

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { optWidth   :: !(Maybe Int)
    , optDefault :: !Int
    , optAscii   :: !Bool
    , optTime    :: !Bool
    , optFormat  :: !String
    , optInput   :: !Bool
    , optTimeout :: !Int
    , optNote    :: !(Maybe String)
    }

options :: OA.Parser Options
options =
    Options
      <$> optional widthOption
      <*> defaultOption
      <*> asciiOption
      <*> timeOption
      <*> formatOption
      <*> inputOption
      <*> timeoutOption
      <*> noteArguments

widthOption :: OA.Parser Int
widthOption = OA.option OA.auto $ mconcat
    [ OA.long "width"
    , OA.short 'w'
    , OA.metavar "CHARS"
    , OA.help "target rule width (default: terminal width)"
    ]

defaultOption :: OA.Parser Int
defaultOption = OA.option OA.auto $ mconcat
    [ OA.long "default"
    , OA.short 'd'
    , OA.metavar "CHARS"
    , OA.value defaultWidth
    , OA.showDefault
    , OA.help "default target rule width"
    ]

asciiOption :: OA.Parser Bool
asciiOption = OA.switch $ mconcat
    [ OA.long "ascii"
    , OA.short 'a'
    , OA.help "use ASCII lines (default: use Unicode lines)"
    ]

timeOption :: OA.Parser Bool
timeOption = OA.switch $ mconcat
    [ OA.long "time"
    , OA.short 't'
    , OA.help "show time"
    ]

formatOption :: OA.Parser String
formatOption = OA.strOption $ mconcat
    [ OA.long "format"
    , OA.short 'f'
    , OA.metavar "FORMAT"
    , OA.value defaultFormat
    , OA.showDefaultWith id
    , OA.help "time format"
    ]

inputOption :: OA.Parser Bool
inputOption = OA.switch $ mconcat
    [ OA.long "input"
    , OA.short 'i'
    , OA.help "read note from STDIN within MS milliseconds"
    ]

timeoutOption :: OA.Parser Int
timeoutOption = OA.option OA.auto $ mconcat
    [ OA.long "timeout"
    , OA.metavar "MS"
    , OA.value defaultTimeout
    , OA.showDefault
    , OA.help "timeout in milliseconds"
    ]

noteArguments :: OA.Parser (Maybe String)
noteArguments = fmap mjoin . many . OA.strArgument $ mconcat
    [ OA.metavar "NOTE ..."
    , OA.help "show a note"
    ]
  where
    mjoin :: [String] -> Maybe String
    mjoin [] = Nothing
    mjoin ss = Just $ unwords ss

------------------------------------------------------------------------------
-- $Main

main :: IO ()
main = do
    Options{..} <- OA.execParser pinfo
    mTime <- if optTime
      then Just . formatTime defaultTimeLocale optFormat <$> getZonedTime
      else pure Nothing
    mInput <- if optInput
      then timeout (optTimeout * 1000) getLine
      else pure Nothing
    let hr = case (optWidth, optAscii) of
          (Nothing,    False) -> HR.putAutoUnicode optDefault
          (Nothing,    True)  -> HR.putAutoAscii optDefault
          (Just width, False) -> HR.putUnicode width
          (Just width, True)  -> HR.putAscii width
    hr . map T.pack $ concat
      [ maybeToList mTime
      , maybeToList optNote
      , maybeToList mInput
      ]
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner HR.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "horizontal rule for the terminal"
          , OA.failureCode 2
          , OA.footerDoc $ Just formatHelp
          ]

    formatHelp :: LibOA.Doc
    formatHelp = LibOA.section "FORMAT codes:" $ LibOA.table_ 2
      [ ["%Y", "four-digit year"]
      , ["%y", "two-digit year"]
      , ["%m", "two-digit month"]
      , ["%d", "two-digit day"]
      , ["%H", "two-digit hour using 24-hour clock"]
      , ["%I", "two-digit hour using 12-hour clock"]
      , ["%p", "locale equivalent of AM or PM"]
      , ["%M", "two-digit minute"]
      , ["%S", "two-digit second"]
      , ["%q", "twelve-digit picosecond"]
      , ["%z", "UTC offset"]
      ]
