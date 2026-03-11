-- | This module contains a data structure for RNAalifold output
--   For more information on RNAalifold consult: <>

module Bio.RNAalifoldData where

-- | Data structure corrsponds with alifoldoutput redirected to file. Output written directly to commandline also contains number of sequences and length of consensus sequence
data RNAalifold = RNAalifold
  {
    alignmentConsensusSequence :: String,
    alignmentConsensusDotBracket :: String,
    alignmentConsensusMinimumFreeEnergy :: Double,
    alignmentConsensusInitialMinimumFreeEnergy :: Double,
    alignmentConsensusEnergyCovarianceContributions :: Double
  }
  deriving (Show, Eq)
