-- | This module contains a data structure for RNAfold output
--   For more information on RNAplex consult: <http://www.tbi.univie.ac.at/RNA/RNAfold.html>

module Bio.RNAfoldData where
-- | Data structure for RNAfold
data RNAfold = RNAfold
  { 
    sequenceIdentifier :: String,
    foldSequence :: String,
    foldSecondaryStructure :: String,
    foldingEnergy :: Double
  }
  deriving (Show, Eq)

data RNAfoldMEA = RNAfoldMEA
  { 
    meaSequenceIdentifier :: String,
    meaFoldSequence :: String,
    mfeStructure :: String,
    mfefoldingEnergy :: Double,
    coarseStructure :: String,
    coarseFoldingEnergy :: Double,
    centroidStructure :: String,
    centroidFoldingEnergy :: Double,
    centroidDistance :: Double,
    meaStructure :: String,
    meaFoldingEnergy :: Double,
    meaDistance :: Double,
    mfeFreq :: Double,
    ensembleDiversity :: Double
  }
  deriving (Show, Eq)
