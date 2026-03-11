{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  Analyze.Analyze
-- Description :  Parse sequence file formats and analyze them
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 08:41:05 2018.
module SLynx.Translate.Translate
  ( translateCmd,
  )
where

import Control.Monad.Trans.Reader
import ELynx.Character.Codon
import ELynx.Sequence.Export.Fasta
import ELynx.Sequence.Sequence
import ELynx.Sequence.Translate
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import SLynx.Tools
import SLynx.Translate.Options

translateSeqs :: Int -> UniversalCode -> [Sequence] -> [Sequence]
translateSeqs rf uc = map (translateSeq uc rf)

-- | Translate sequences.
translateCmd :: ELynx TranslateArguments ()
translateCmd = do
  (TranslateArguments al inFile rf uc) <- localArguments <$> ask
  logInfoS $ "  Universal code: " <> show uc <> "."
  logInfoS $ "  Reading frame: " <> show rf <> "."
  logInfoS ""
  ss <- readSeqs al inFile
  let result = sequencesToFasta $ translateSeqs rf uc ss
  out "translated sequences" result "fasta"
