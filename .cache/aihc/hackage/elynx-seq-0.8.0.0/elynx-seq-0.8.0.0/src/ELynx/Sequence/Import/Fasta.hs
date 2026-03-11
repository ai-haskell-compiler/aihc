{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  ELynx.Sequence.Import.Fasta
-- Description :  Import Fasta sequences
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Parse FASTA files.
--
-- [NCBI file specifications](https://blast.ncbi.nlm.nih.gov/Blast.cgi?CMD=Web&PAGE_TYPE=BlastDocs&DOC_TYPE=BlastHelp).
module ELynx.Sequence.Import.Fasta
  ( fastaSequence,
    fasta,
  )
where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as AS
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Set as S
import Data.Word8 (Word8)
import ELynx.Alphabet.Alphabet as A
import ELynx.Alphabet.Character
import ELynx.Sequence.Sequence

isSpecial :: Char -> Bool
isSpecial w = w `elem` ['_', '|', '.', '-', '/', '@']

isHeader :: Char -> Bool
isHeader w = AC.isAlpha_ascii w || AC.isDigit w || isSpecial w

sequenceHeader :: AS.Parser (BL.ByteString, BL.ByteString)
sequenceHeader = do
  _ <- AC.char '>'
  n <- AC.takeWhile1 isHeader
  _ <- AS.takeWhile AC.isHorizontalSpace
  d <- AC.takeWhile isHeader
  _ <- AC.endOfLine
  return (BL.fromStrict n, BL.fromStrict d)

-- It is a little faster to directly pass the set of allowed characters. Then,
-- this set only has to be calculcated once per sequence in 'fastaSequence'.
sequenceLine :: S.Set Word8 -> AS.Parser BL.ByteString
sequenceLine s = do
  -- XXX: Will fail for non-capital letters.
  !xs <- AS.takeWhile1 (`S.member` s)
  return (BL.fromStrict xs)

-- XXX: If sequences are parsed line by line, the lines have to be copied when
-- forming the complete sequence. This is not memory efficient.

-- | Parse a sequence of characters.
fastaSequence :: Alphabet -> AS.Parser Sequence
fastaSequence a = do
  (n, d) <- sequenceHeader
  let !alph = S.map toWord (A.all . alphabetSpec $ a)
  lns <- sequenceLine alph `AS.sepBy1` AC.endOfLine
  _ <- many AC.endOfLine
  return $ Sequence n d a (fromByteString $ BL.concat lns)

-- | Parse a Fasta file with given 'Alphabet'.
fasta :: Alphabet -> AS.Parser [Sequence]
fasta a = some (fastaSequence a) <* AS.endOfInput
