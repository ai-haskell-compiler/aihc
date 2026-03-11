{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  SLynx.Concatenate.Options
-- Description :  ELynxSeq argument parsing
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sun Oct  7 17:29:45 2018.
module SLynx.Concatenate.Options
  ( ConcatenateArguments (..),
    concatenateArguments,
  )
where

import Control.Applicative
import Data.Aeson
import ELynx.Alphabet.Alphabet
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import SLynx.Tools

-- | Arguments needed to concatenate multi sequence alignments.
data ConcatenateArguments = ConcatenateArguments
  { ccAlphabet :: Alphabet,
    ccInFiles :: [FilePath]
  }
  deriving (Eq, Show, Generic)

instance Reproducible ConcatenateArguments where
  inFiles = ccInFiles
  outSuffixes _ = [".fasta"]
  getSeed _ = Nothing
  setSeed = const
  parser = concatenateArguments
  cmdName = "concatenate"
  cmdDsc = ["Concatenate sequences found in input files."]

instance FromJSON ConcatenateArguments

instance ToJSON ConcatenateArguments

-- | Command line parser.
concatenateArguments :: Parser ConcatenateArguments
concatenateArguments = ConcatenateArguments <$> alphabetOpt <*> some inFileArg

inFileArg :: Parser FilePath
inFileArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE"
