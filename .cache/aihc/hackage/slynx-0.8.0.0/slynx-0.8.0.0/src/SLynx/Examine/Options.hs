{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  SLynx.Examine.Options
-- Description :  ELynxSeq argument parsing
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sun Oct  7 17:29:45 2018.
module SLynx.Examine.Options
  ( ExamineArguments (..),
    examineArguments,
  )
where

import Data.Aeson
import ELynx.Alphabet.Alphabet
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import SLynx.Tools

-- | Arguments needed to examine sequences.
data ExamineArguments = ExamineArguments
  { exAlphabet :: Alphabet,
    exInFile :: FilePath,
    exPerSite :: Bool,
    exDivergence :: Bool
  }
  deriving (Eq, Show, Generic)

instance Reproducible ExamineArguments where
  inFiles = pure . exInFile
  outSuffixes args = if exDivergence args then [".out", ".div"] else [".out"]
  getSeed _ = Nothing

  -- XXX: Probably throw error when seed is set.
  setSeed = const
  parser = examineArguments
  cmdName = "examine"
  cmdDsc =
    [ "Examine sequences. If data is a multi sequence alignment, additionally analyze columns."
    ]

instance FromJSON ExamineArguments

instance ToJSON ExamineArguments

-- | Command line parser.
examineArguments :: Parser ExamineArguments
examineArguments =
  ExamineArguments <$> alphabetOpt <*> filePathArg <*> examinePerSiteOpt <*> examineDivergence

examinePerSiteOpt :: Parser Bool
examinePerSiteOpt =
  switch $ long "per-site" <> help "Report per site summary statistics"

examineDivergence :: Parser Bool
examineDivergence =
  switch $ long "divergence" <> help "Compute pairwise divergence matrices"

filePathArg :: Parser FilePath
filePathArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE" <> action "file"
