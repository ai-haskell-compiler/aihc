{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  SLynx.Filter.Options
-- Description :  ELynxSeq argument parsing
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sun Oct  7 17:29:45 2018.
module SLynx.Filter.Options
  ( FilterRowsArguments (..),
    FilterColsArguments (..),
    filterRowsArguments,
    filterColsArguments,
  )
where

import Control.Applicative
import Data.Aeson
import ELynx.Alphabet.Alphabet
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import SLynx.Tools

-- | Arguments needed for filtering sequences.
data FilterRowsArguments = FilterRowsArguments
  { frAlphabet :: Alphabet,
    frInFile :: FilePath,
    frLonger :: Maybe Int,
    frShorter :: Maybe Int,
    frStandard :: Bool
  }
  deriving (Eq, Show, Generic)

instance Reproducible FilterRowsArguments where
  inFiles = pure . frInFile
  outSuffixes _ = [".fasta"]
  getSeed _ = Nothing
  setSeed = const
  parser = filterRowsArguments
  cmdName = "filter-rows"
  cmdDsc = ["Filter rows (or sequences) found in input files."]

instance FromJSON FilterRowsArguments

instance ToJSON FilterRowsArguments

-- | Arguments needed for filtering columns of a multi sequence alignment.
data FilterColsArguments = FilterColsArguments
  { fcAlphabet :: Alphabet,
    fcInFile :: FilePath,
    fcStandard :: Maybe Double
  }
  deriving (Eq, Show, Generic)

instance Reproducible FilterColsArguments where
  inFiles = pure . fcInFile
  outSuffixes _ = [".fasta"]
  getSeed _ = Nothing
  setSeed = const
  parser = filterColsArguments
  cmdName = "filter-columns"
  cmdDsc = ["Filter columns of multi sequence alignments."]

instance FromJSON FilterColsArguments

instance ToJSON FilterColsArguments

-- | Command line parser.
filterRowsArguments :: Parser FilterRowsArguments
filterRowsArguments =
  FilterRowsArguments
    <$> alphabetOpt
    <*> inFileArg
    <*> filterLongerThanOpt
    <*> filterShorterThanOpt
    <*> filterStandardChars

filterLongerThanOpt :: Parser (Maybe Int)
filterLongerThanOpt =
  optional $
    option auto $
      long "longer-than"
        <> metavar "LENGTH"
        <> help
          "Only keep sequences longer than LENGTH"

filterShorterThanOpt :: Parser (Maybe Int)
filterShorterThanOpt =
  optional $
    option auto $
      long "shorter-than"
        <> metavar "LENGTH"
        <> help
          "Only keep sequences shorter than LENGTH"

filterStandardChars :: Parser Bool
filterStandardChars =
  switch $
    long "standard-characters"
      <> help
        "Only keep sequences containing at least one standard (i.e., non-IUPAC) character"

-- | Command line parser.
filterColsArguments :: Parser FilterColsArguments
filterColsArguments =
  FilterColsArguments <$> alphabetOpt <*> inFileArg <*> filterStandardOpt

filterStandardOpt :: Parser (Maybe Double)
filterStandardOpt =
  optional $
    option auto $
      long "standard-chars"
        <> metavar "DOUBLE"
        <> help
          "Keep columns with a proportion standard (non-IUPAC) characters larger than DOUBLE in [0,1]"

inFileArg :: Parser FilePath
inFileArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE"
