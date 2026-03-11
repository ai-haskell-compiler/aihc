{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  SLynx.Tools
-- Description :  Common tools for sequence lynx
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Sep  7 06:24:22 2019.
module SLynx.Tools
  ( -- * SLynx.Tools
    readSeqs,

    -- * Options
    alphabetOpt,
  )
where

import Control.Monad.IO.Class
import ELynx.Alphabet.Alphabet
import ELynx.Sequence.Import.Fasta
import ELynx.Sequence.Sequence
import ELynx.Tools.InputOutput
import ELynx.Tools.Logger
import Options.Applicative

-- | Read sequences of given alphabet from file or standard input.
readSeqs ::
  (HasLock e, HasLogHandles e, HasVerbosity e) =>
  Alphabet ->
  FilePath ->
  Logger e [Sequence]
readSeqs a fp = do
  logInfoS $
    "Read sequences from file "
      <> fp
      <> "; alphabet "
      <> show a
      <> "."
  liftIO $ parseFileWith (fasta a) fp

-- | Command line option to specify the alphabet. Used by various commands.
alphabetOpt :: Parser Alphabet
alphabetOpt =
  option auto $
    long "alphabet"
      <> short 'a'
      <> metavar "NAME"
      <> help
        "Specify alphabet type NAME"
