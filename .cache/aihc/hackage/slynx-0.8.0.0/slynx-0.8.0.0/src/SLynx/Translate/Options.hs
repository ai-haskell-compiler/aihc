{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  SLynx.Translate.Options
-- Description :  ELynxSeq argument parsing
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sun Oct  7 17:29:45 2018.
module SLynx.Translate.Options
  ( TranslateArguments (..),
    translateArguments,
  )
where

import Data.Aeson
import Data.List
import ELynx.Alphabet.Alphabet
import ELynx.Character.Codon
import ELynx.Tools.Reproduction
import GHC.Generics
import Options.Applicative
import SLynx.Tools

-- | Arguments needed to translate sequences.
data TranslateArguments = TranslateArguments
  { trAlphabet :: Alphabet,
    trInFile :: FilePath,
    trReadingFrame :: Int,
    trUniversalCode :: UniversalCode
  }
  deriving (Eq, Show, Generic)

instance Reproducible TranslateArguments where
  inFiles = pure . trInFile
  outSuffixes _ = [".fasta"]
  getSeed _ = Nothing
  setSeed = const
  parser = translateArguments
  cmdName = "translate"
  cmdDsc = ["Translate from DNA to Protein or DNAX to ProteinX."]

instance FromJSON TranslateArguments

instance ToJSON TranslateArguments

-- | Command line parser.
translateArguments :: Parser TranslateArguments
translateArguments =
  TranslateArguments
    <$> alphabetOpt
    <*> inFileArg
    <*> readingFrameOpt
    <*> universalCodeOpt

readingFrameOpt :: Parser Int
readingFrameOpt =
  option auto $
    long "reading-frame"
      <> short 'r'
      <> metavar "INT"
      <> help
        "Reading frame [0|1|2]."

universalCodeOpt :: Parser UniversalCode
universalCodeOpt =
  option auto $
    long "universal-code"
      <> short 'u'
      <> metavar "CODE"
      <> help
        ("universal code; one of: " ++ codeStr ++ ".")
  where
    codes = [minBound ..] :: [UniversalCode]
    codeWords = map show codes
    codeStr = intercalate ", " codeWords

inFileArg :: Parser FilePath
inFileArg =
  strArgument $ metavar "INPUT-FILE" <> help "Read sequences from INPUT-FILE"
