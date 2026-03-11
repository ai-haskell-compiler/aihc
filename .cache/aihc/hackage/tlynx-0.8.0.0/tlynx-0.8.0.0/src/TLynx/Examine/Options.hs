{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  TLynx.Examine.Options
-- Description :  Tree analysis options
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Aug 29 08:16:45 2019.
module TLynx.Examine.Options
  ( ExamineArguments (..),
    examineArguments,
  )
where

import Data.Aeson
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import TLynx.Parsers

-- | Arguments needed to examine phylogenetic trees.
data ExamineArguments = ExamineArguments
  { argsInFile :: FilePath,
    argsNewickFormat :: NewickFormat
  }
  deriving (Eq, Show, Generic)

instance Reproducible ExamineArguments where
  inFiles = pure . argsInFile
  outSuffixes _ = [".out"]
  getSeed _ = Nothing
  setSeed = const
  parser = examineArguments
  cmdName = "examine"
  cmdDsc = ["Compute summary statistics of phylogenetic trees."]

instance FromJSON ExamineArguments

instance ToJSON ExamineArguments

-- | Command line parser.
examineArguments :: Parser ExamineArguments
examineArguments = ExamineArguments <$> inFile <*> newickFormat

inFile :: Parser FilePath
inFile =
  strArgument $ metavar "INPUT-FILE" <> help "Read trees from INPUT-FILE" <> action "file"
