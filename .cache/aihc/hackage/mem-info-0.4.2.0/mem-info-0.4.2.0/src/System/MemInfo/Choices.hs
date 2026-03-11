{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : System.MemInfo.Choices
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3

This module defines the command line flags used to control the behavior of the
__printmem__ command
-}
module System.MemInfo.Choices (
  Choices (..),
  Style (..),
  PrintOrder (..),
  Power (..),
  Mem (..),
  asFloat,
  memReader,
  cmdInfo,
  getChoices,
  defaultRoot,
) where

import Data.Fixed (Deci)
import qualified Data.Text as Text
import Data.Text.Read (Reader, rational)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative (
  Parser,
  ParserInfo,
  ReadM,
  eitherReader,
  execParser,
  help,
  helper,
  info,
  infoOption,
  long,
  metavar,
  option,
  optional,
  readerError,
  short,
  showDefault,
  str,
  switch,
  value,
 )
import Options.Applicative.NonEmpty (some1)
import Paths_mem_info (version)
import System.MemInfo.Prelude


-- | Parses the command line arguments.
getChoices :: IO Choices
getChoices = execParser cmdInfo


-- | Represents the user-specified choices extracted from the command line
data Choices = Choices
  { choiceSplitArgs :: !Bool
  , choiceOnlyTotal :: !Bool
  , choiceByPid :: !Bool
  , choiceShowSwap :: !Bool
  , choiceReversed :: !Bool
  , choiceProcRoot :: !FilePath
  , choiceWatchSecs :: !(Maybe Natural)
  , choicePidsToShow :: !(Maybe (NonEmpty ProcessID))
  , choicePrintOrder :: !(Maybe PrintOrder)
  , choiceStyle :: !(Maybe Style)
  , choiceMinMemory :: !(Maybe Mem)
  }
  deriving (Eq, Show, Generic)


-- | Specifies a command line that when parsed will provide 'Choices'
cmdInfo :: ParserInfo Choices
cmdInfo = info (versionOption <*> helper <*> parseChoices) mempty


versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("printmem version " ++ showVersion version)
    (long "version" <> help "Show the version and exit")


parseChoices :: Parser Choices
parseChoices =
  Choices
    <$> parseSplitArgs
    <*> parseOnlyTotal
    <*> parseDiscriminateByPid
    <*> parseShowSwap
    <*> parseReversed
    <*> parseRoot
    <*> optional parseWatchPeriodSecs
    <*> optional parseChoicesPidsToShow
    <*> optional parsePrintOrder
    <*> optional parseStyle
    <*> optional parseMinReported


parseChoicesPidsToShow :: Parser (NonEmpty ProcessID)
parseChoicesPidsToShow =
  some1 $
    option positiveNum $
      short 'p'
        <> long "pids"
        <> metavar "<pid1> [ -p pid2 ... -p pidN ]"
        <> help "Only show memory usage of the specified PIDs"


parseSplitArgs :: Parser Bool
parseSplitArgs =
  switch $
    short 's'
      <> long "split-args"
      <> help "Show and separate by all command line arguments"


parseOnlyTotal :: Parser Bool
parseOnlyTotal =
  switch $
    short 't'
      <> long "total"
      <> help "Only show the total value"


parseReversed :: Parser Bool
parseReversed =
  switch $
    short 'r'
      <> long "reverse"
      <> help "Reverses the output order so that output descends on the sorting field"


parseDiscriminateByPid :: Parser Bool
parseDiscriminateByPid =
  switch $
    short 'd'
      <> long "discriminate-by-pid"
      <> help "Show by process rather than by program"


parseRoot :: Parser FilePath
parseRoot =
  option str $
    long "proc-root"
      <> help "the root directory of the process file hierachy"
      <> value defaultRoot
      <> showDefault


parseShowSwap :: Parser Bool
parseShowSwap =
  switch $
    short 'S'
      <> long "show_swap"
      <> help "Show swap information"


parseWatchPeriodSecs :: Parser Natural
parseWatchPeriodSecs =
  option positiveNum $
    short 'w'
      <> long "watch"
      <> metavar "N"
      <> help "Measure and show memory every N seconds (N > 0)"


positiveNum :: (Read a, Ord a, Num a) => ReadM a
positiveNum =
  let
    checkPositive i
      | i > 0 = pure i
      | otherwise = readerError "Value must be greater than 0"
   in
    autoOrNotAllowed >>= checkPositive


parsePrintOrder :: Parser PrintOrder
parsePrintOrder =
  option autoIgnoreCase $
    short 'b'
      <> long "order-by"
      <> metavar "< private | swap | shared | count >"
      <> help "Orders the output by ascending values of the given field"


-- | Determines the order in which @MemUsages@ in a report are printed out
data PrintOrder
  = Swap
  | Private
  | Shared
  | Count
  deriving (Eq, Show, Read, Generic)


parseMinReported :: Parser Mem
parseMinReported =
  option (eitherReader $ fromReader memReader) $
    short 'm'
      <> long "min-reported"
      <> metavar "<threshold>[K|M|G|T]iB, e.g 1.1KiB | 2MiB | 4.0GiB"
      <> help "Specifies a minimum below which memory values are omitted"


parseStyle :: Parser Style
parseStyle =
  option autoIgnoreCase $
    short 'y'
      <> long "output-style"
      <> metavar "< [normal] | csv >"
      <> help (Text.unpack styleHelp)


styleHelp :: Text
styleHelp =
  Text.unlines
    [ "Determines how the output report is presented;"
    , "'normal' is the default and is the same as if this option was omitted;"
    , "'csv' outputs the usage and header rows in csv format, with all values in KiB and no 'total' row."
    , "With 'csv', the --total (-t) flag is ignored"
    ]


-- | Determines the format style of the output
data Style
  = Csv
  | Normal
  deriving (Eq, Show, Read, Generic)


autoIgnoreCase :: (Read a) => ReadM a
autoIgnoreCase =
  let toTitle' = Text.unpack . Text.toTitle . Text.pack
   in eitherReader $ readOrNotAllowed toTitle'


autoOrNotAllowed :: (Read a) => ReadM a
autoOrNotAllowed = eitherReader $ readOrNotAllowed id


readOrNotAllowed :: (Read a) => (String -> String) -> String -> Either String a
readOrNotAllowed f x = case readEither $ f x of
  Left _ignored -> Left $ "value '" ++ x ++ "' is not permitted"
  right -> right


fromReader :: Reader a -> String -> Either String a
fromReader reader = fmap fst . reader . Text.pack


-- | Represents the power in memory quanity unit
data Power = Ki | Mi | Gi | Ti
  deriving
    (Eq, Read, Show, Ord, Enum, Bounded, Generic)


floatingFactor :: Power -> Double
floatingFactor Ki = 1.0
floatingFactor Mi = 1024.0
floatingFactor Gi = 1024.0 ** 2
floatingFactor Ti = 1024.0 ** 3


powerReader :: Text -> Either String (Power, Text)
powerReader x =
  let (want, extra) = Text.splitAt 3 $ Text.stripStart x
      go "Ki" = Right (Ki, extra)
      go "Mi" = Right (Mi, extra)
      go "Gi" = Right (Gi, extra)
      go "Ti" = Right (Ti, extra)
      go _other = Left "invalid Power"
   in go $ Text.take 2 want


-- | Represents an amount of memory
data Mem = Mem !Power !Deci
  deriving (Eq, Show, Ord, Generic)


asFloat :: Mem -> Double
asFloat (Mem pow x) = realToFrac x * floatingFactor pow


memReader :: Text -> Either String (Mem, Text)
memReader x = do
  (num, rest) <- rational (Text.stripStart x)
  (power, extra) <- powerReader rest
  pure (Mem power num, extra)


defaultRoot :: FilePath
defaultRoot = "/proc"
