-- | This module contains a data structure for RNAcofold output
--   For more information on RNAcofold consult: <http://www.tbi.univie.ac.at/RNA/RNAcofold.html>

module Bio.RNAcofoldData where
-- | Data structure for cofolding of 2 nucleic acid sequences
data RNAcofold = RNAcofold
  { 
    coFoldSequence1 :: String,
    coFoldSequence2 :: String,
    coFoldSecondaryStructure1 :: String,
    coFoldSecondaryStructure2 :: String,
    coFoldingEnergy :: Double
  }
  deriving (Show, Eq)
