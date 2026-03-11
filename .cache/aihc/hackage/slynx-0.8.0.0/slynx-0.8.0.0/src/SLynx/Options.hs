{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  SLynx.Options
-- Description :  SLynx general options
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Sep  7 18:55:03 2019.
module SLynx.Options (CommandArguments (..)) where

import Data.Aeson
import ELynx.Alphabet.Alphabet
import ELynx.Tools.Options
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import SLynx.Concatenate.Options
import SLynx.Examine.Options
import SLynx.Filter.Options
import SLynx.Simulate.Options
import SLynx.SubSample.Options
import SLynx.Translate.Options

-- | The different SLynx commands and their arguments.
data CommandArguments
  = Concatenate ConcatenateArguments
  | Examine ExamineArguments
  | FilterCols FilterColsArguments
  | FilterRows FilterRowsArguments
  | Simulate SimulateArguments
  | SubSample SubSampleArguments
  | Translate TranslateArguments
  deriving (Eq, Show, Generic)

instance Reproducible CommandArguments where
  inFiles (Concatenate a) = inFiles a
  inFiles (Examine a) = inFiles a
  inFiles (FilterCols a) = inFiles a
  inFiles (FilterRows a) = inFiles a
  inFiles (Simulate a) = inFiles a
  inFiles (SubSample a) = inFiles a
  inFiles (Translate a) = inFiles a

  outSuffixes (Concatenate a) = outSuffixes a
  outSuffixes (Examine a) = outSuffixes a
  outSuffixes (FilterCols a) = outSuffixes a
  outSuffixes (FilterRows a) = outSuffixes a
  outSuffixes (Simulate a) = outSuffixes a
  outSuffixes (SubSample a) = outSuffixes a
  outSuffixes (Translate a) = outSuffixes a

  getSeed (Concatenate a) = getSeed a
  getSeed (Examine a) = getSeed a
  getSeed (FilterCols a) = getSeed a
  getSeed (FilterRows a) = getSeed a
  getSeed (Simulate a) = getSeed a
  getSeed (SubSample a) = getSeed a
  getSeed (Translate a) = getSeed a

  setSeed (Concatenate a) = Concatenate . setSeed a
  setSeed (Examine a) = Examine . setSeed a
  setSeed (FilterCols a) = FilterCols . setSeed a
  setSeed (FilterRows a) = FilterRows . setSeed a
  setSeed (Simulate a) = Simulate . setSeed a
  setSeed (SubSample a) = SubSample . setSeed a
  setSeed (Translate a) = Translate . setSeed a

  parser = commandArguments

  cmdName = "slynx"

  cmdDsc = ["Analyze, and simulate multi sequence alignments."]

  cmdFtr =
    [ "",
      "Available sequence file formats:"
    ]
      ++ fs
      ++ ["", "Available alphabets:"]
      ++ as
    where
      toListItem = ("  - " ++)
      fs = map toListItem ["FASTA"]
      as = map (toListItem . alphabetDescription) ([minBound ..] :: [Alphabet])

instance FromJSON CommandArguments

instance ToJSON CommandArguments

concatenateCommand :: Mod CommandFields CommandArguments
concatenateCommand = createCommandReproducible Concatenate

examineCommand :: Mod CommandFields CommandArguments
examineCommand = createCommandReproducible Examine

filterColumnsCommand :: Mod CommandFields CommandArguments
filterColumnsCommand = createCommandReproducible FilterCols

filterRowsCommand :: Mod CommandFields CommandArguments
filterRowsCommand = createCommandReproducible FilterRows

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = createCommandReproducible Simulate

subSampleCommand :: Mod CommandFields CommandArguments
subSampleCommand = createCommandReproducible SubSample

translateCommand :: Mod CommandFields CommandArguments
translateCommand = createCommandReproducible Translate

commandArguments :: Parser CommandArguments
commandArguments =
  hsubparser $
    concatenateCommand
      <> examineCommand
      <> filterColumnsCommand
      <> filterRowsCommand
      <> simulateCommand
      <> subSampleCommand
      <> translateCommand
