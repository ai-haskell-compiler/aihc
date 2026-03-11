-- |
-- Module      :  ELynx.Sequence.Export.Fasta
-- Description :  Export Fasta sequences
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Write FASTA files.
--
-- [NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).
module ELynx.Sequence.Export.Fasta
  ( sequenceToFasta,
    sequencesToFasta,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Sequence.Sequence

fastaHeader :: BL.ByteString -> BL.ByteString -> BL.ByteString
fastaHeader n d =
  BL.singleton '>' <> n <> if BL.null d then BL.empty else BL.pack " " <> d

-- | Convert a 'Sequence' to Fasta format.
sequenceToFasta :: Sequence -> BL.ByteString
sequenceToFasta s =
  BL.unlines [fastaHeader (name s) (description s), toByteString $ characters s]

-- | Convert a list 'Sequence's to Fasta format. A newline is added between any
-- two 'Sequence's.
sequencesToFasta :: [Sequence] -> BL.ByteString
sequencesToFasta ss = BL.concat $ map sequenceToFasta ss
