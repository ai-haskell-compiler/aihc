{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  TLynx.Distance.Options
-- Description :  Options of tree-dist
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Aug 29 13:02:22 2019.
module TLynx.Distance.Options
  ( DistanceArguments (..),
    DistanceMeasure (..),
    distanceArguments,
    distanceFooter,
  )
where

import Data.Aeson
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString.Char8 as BS
import ELynx.Tools.Reproduction
import ELynx.Tree (Support (..), toSupportUnsafe)
import GHC.Generics
import Options.Applicative
import TLynx.Parsers
import Text.Printf

-- | Supported distance measures.
data DistanceMeasure
  = -- | Symmetric distance.
    Symmetric
  | -- | Incompatible split distance; collapse nodes
    -- with branch support below given value.
    IncompatibleSplit Support
  | -- | Branch score distance.
    BranchScore
  deriving (Eq, Generic)

instance FromJSON DistanceMeasure

instance ToJSON DistanceMeasure

instance Show DistanceMeasure where
  show Symmetric = "Symmetric"
  show (IncompatibleSplit c) = "Incompatible Split (" ++ printf "%.2f" (fromSupport c) ++ ")"
  show BranchScore = "Branch Score"

-- | Arguments needed to compute distance measures.
data DistanceArguments = DistanceArguments
  { argsDistance :: DistanceMeasure,
    argsNormalize :: Bool,
    argsIntersect :: Bool,
    argsSummaryStatistics :: Bool,
    argsMasterTreeFile :: Maybe FilePath,
    argsNewickFormat :: NewickFormat,
    argsInFiles :: [FilePath]
  }
  deriving (Eq, Show, Generic)

instance Reproducible DistanceArguments where
  inFiles a = case argsMasterTreeFile a of
    Nothing -> argsInFiles a
    Just f -> f : argsInFiles a
  outSuffixes _ = [".out"]
  getSeed _ = Nothing
  setSeed = const
  parser = distanceArguments
  cmdName = "distance"
  cmdDsc = ["Compute distances between many phylogenetic trees."]
  cmdFtr = distanceFooter

instance FromJSON DistanceArguments

instance ToJSON DistanceArguments

-- | COmmand line parser.
distanceArguments :: Parser DistanceArguments
distanceArguments =
  DistanceArguments
    <$> distanceOpt
    <*> normalizeSwitch
    <*> intersectSwitch
    <*> summaryStatisticsSwitch
    <*> masterTreeFile
    <*> newickFormat
    <*> many inFilesArg

masterTreeFile :: Parser (Maybe FilePath)
masterTreeFile =
  optional $
    strOption $
      long "master-tree-file"
        <> short 'm'
        <> metavar "MASTER-TREE-File"
        <> help "Compare all trees to the tree in the master tree file."
        <> action "file"

inFilesArg :: Parser FilePath
inFilesArg =
  strArgument $
    metavar "INPUT-FILES"
      <> help
        "Read tree(s) from INPUT-FILES; if more files are given, one tree is expected per file"
      <> action "file"

symmetric :: AC.Parser DistanceMeasure
symmetric = do
  _ <- AC.string "symmetric"
  _ <- AC.endOfInput
  pure Symmetric

incompatibleSplit :: AC.Parser DistanceMeasure
incompatibleSplit = do
  _ <- AC.string "incompatible-split"
  _ <- AC.char '['
  f <- AC.double
  _ <- AC.char ']'
  _ <- AC.endOfInput
  if (0 <= f) && (f <= 1)
    then pure $ IncompatibleSplit $ toSupportUnsafe f
    else error "Branch support has to be in [0, 1]."

branchScore :: AC.Parser DistanceMeasure
branchScore = do
  _ <- AC.string "branch-score"
  _ <- AC.endOfInput
  pure BranchScore

-- Try first the normalized one, since the normal branch score
-- parser also succeeds in this case.
distanceParser :: AC.Parser DistanceMeasure
distanceParser = symmetric <|> incompatibleSplit <|> branchScore

-- See 'eitherReader', but for an attoparsec parser.
eitherReadA :: AC.Parser a -> ReadM a
eitherReadA p = eitherReader $ \input -> AC.parseOnly p (BS.pack input)

distanceOpt :: Parser DistanceMeasure
distanceOpt =
  option (eitherReadA distanceParser) $
    long "distance"
      <> short 'd'
      <> metavar "MEASURE"
      <> help
        "Type of distance to calculate (available distance measures are listed below)"

summaryStatisticsSwitch :: Parser Bool
summaryStatisticsSwitch =
  switch $
    long "summary-statistics"
      <> short 's'
      <> help
        "Report summary statistics only"

normalizeSwitch :: Parser Bool
normalizeSwitch =
  switch $
    long "normalize"
      <> short 'n'
      <> help
        "Normalize trees before distance calculation; only affect distances depending on branch lengths"

intersectSwitch :: Parser Bool
intersectSwitch =
  switch $
    long "intersect"
      <> short 't'
      <> help
        "Compare intersections; i.e., before comparison, drop leaves that are not present in the other tree"

-- | Information about provided distance types.
distanceFooter :: [String]
distanceFooter =
  [ "Distance measures:",
    "  symmetric                Symmetric distance (Robinson-Foulds distance).",
    "  incompatible-split[VAL]  Incompatible split distance. Collapse branches with (normalized)",
    "                           support less than 0.0<=VAL<=1.0 before distance calculation;",
    "                           if, let's say, VAL>0.7, only well supported differences contribute",
    "                           to the total distance.",
    "  branch-score             Branch score distance."
  ]
