-- | Library with auxiliary functions needed in multiple other modules.
module Bio.ViennaRNAParserLibrary (
                      parseNucleotideSequence,
                      parseNucleotideAlignmentEntry,
                      parseProteinSequence,
                      parseProteinAlignmentEntry,
                      readInt,
                      readDouble
                      ) where

import Text.ParserCombinators.Parsec

readDouble :: String -> Double
readDouble = read              

readInt :: String -> Int
readInt = read

-- | Parse nucleotide sequence. Allowed letters according to IUPAC
parseNucleotideSequence :: GenParser Char st String
parseNucleotideSequence = do
  nucleotideSequence <- many1 (oneOf "RYSWKMBDHVNATUGCryswkmbdhvnatugc") 
  return $ nucleotideSequence

-- | Parse nucleotide alignment entry. Allowed letters according to IUPAC and commonly used gap characters
parseNucleotideAlignmentEntry :: GenParser Char st String
parseNucleotideAlignmentEntry = do
  entry <- many1 (oneOf "~_-.RYSWKMBDHVNATUGCryswkmbdhvnatugc") 
  return $ entry

-- | Parse protein amino acid code sequence. Allowed letters according to IUPAC
parseProteinSequence :: GenParser Char st String
parseProteinSequence = do
  proteinSequence <- many1 (oneOf "ABCDEFGHIKLMNPQRSTVWXYZabcdefghiklmnpqrstvwxyz") 
  return $ proteinSequence

-- | Parse protein amino acid code alignment entry. Allowed letters according to IUPAC and commonly used gap characters
parseProteinAlignmentEntry :: GenParser Char st String
parseProteinAlignmentEntry = do
  entry <- many1 (oneOf "~_-.ABCDEFGHIKLMNPQRSTVWXYZabcdefghiklmnpqrstvwxyz") 
  return $ entry
