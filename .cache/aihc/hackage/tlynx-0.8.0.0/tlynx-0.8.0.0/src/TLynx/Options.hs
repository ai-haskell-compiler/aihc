{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  TLynx.Options
-- Description :  TLynx general options
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Sep  7 18:55:03 2019.
module TLynx.Options (CommandArguments (..)) where

import Data.Aeson
import ELynx.Tools.Options
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import TLynx.Compare.Options
import TLynx.Connect.Options
import TLynx.Distance.Options
import TLynx.Examine.Options
import TLynx.Parsers (newickHelp)
import TLynx.Shuffle.Options
import TLynx.Simulate.Options

-- | The different TLynx commands and their arguments.
data CommandArguments
  = Compare CompareArguments
  | Connect ConnectArguments
  | Distance DistanceArguments
  | Examine ExamineArguments
  | Shuffle ShuffleArguments
  | Simulate SimulateArguments
  deriving (Eq, Show, Generic)

instance Reproducible CommandArguments where
  inFiles (Compare a) = inFiles a
  inFiles (Connect a) = inFiles a
  inFiles (Distance a) = inFiles a
  inFiles (Examine a) = inFiles a
  inFiles (Shuffle a) = inFiles a
  inFiles (Simulate a) = inFiles a

  outSuffixes (Compare a) = outSuffixes a
  outSuffixes (Connect a) = outSuffixes a
  outSuffixes (Distance a) = outSuffixes a
  outSuffixes (Examine a) = outSuffixes a
  outSuffixes (Shuffle a) = outSuffixes a
  outSuffixes (Simulate a) = outSuffixes a

  getSeed (Compare a) = getSeed a
  getSeed (Connect a) = getSeed a
  getSeed (Distance a) = getSeed a
  getSeed (Examine a) = getSeed a
  getSeed (Shuffle a) = getSeed a
  getSeed (Simulate a) = getSeed a

  setSeed (Compare a) = Compare . setSeed a
  setSeed (Connect a) = Connect . setSeed a
  setSeed (Distance a) = Distance . setSeed a
  setSeed (Examine a) = Examine . setSeed a
  setSeed (Shuffle a) = Shuffle . setSeed a
  setSeed (Simulate a) = Simulate . setSeed a

  parser = commandArguments

  cmdName = "tlynx"

  cmdDsc = ["Compare, examine, and simulate phylogenetic trees."]

  cmdFtr = "" : "Available tree file formats:" : indent newickHelp
    where
      indent = map ("  " ++)

instance FromJSON CommandArguments

instance ToJSON CommandArguments

compareCommand :: Mod CommandFields CommandArguments
compareCommand = createCommandReproducible Compare

connectCommand :: Mod CommandFields CommandArguments
connectCommand = createCommandReproducible Connect

distanceCommand :: Mod CommandFields CommandArguments
distanceCommand = createCommandReproducible Distance

examineCommand :: Mod CommandFields CommandArguments
examineCommand = createCommandReproducible Examine

shuffleCommand :: Mod CommandFields CommandArguments
shuffleCommand = createCommandReproducible Shuffle

simulateCommand :: Mod CommandFields CommandArguments
simulateCommand = createCommandReproducible Simulate

commandArguments :: Parser CommandArguments
commandArguments =
  hsubparser $
    compareCommand
      <> connectCommand
      <> distanceCommand
      <> examineCommand
      <> shuffleCommand
      <> simulateCommand
