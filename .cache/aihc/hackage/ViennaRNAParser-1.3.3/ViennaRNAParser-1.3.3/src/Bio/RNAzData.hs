-- | This module contains a hierarchical data structure for
--   RNAz output
--   For more information on RNAz consult: <http://www.tbi.univie.ac.at/~wash/RNAz/>

module Bio.RNAzData where
--import Biobase.RNA
    
-- | All elements of RNAz output are contained in this datatype 
data RNAz = RNAz
  { version :: String,
    sequenceNumber :: Int,
    columnNumber :: Int,
    readingDirection :: String,
    meanPairwiseIdentity :: Double,
    shannonEntropy :: Double,
    gcContent :: Double,
    meanSingleSequenceMinimumFreeEnergy :: Double,
    consensusMinimumFreeEnergy :: Double,
    energyContribution :: Double,
    covarianceContribution :: Double,
    combinationsPair :: Double,
    meanZScore :: Double,
    structureConservationIndex :: Double,
    backgroundModel :: String,
    decisionModel :: String,
    svmDecisionValue :: Double,
    svmRNAClassProbability :: Double,
    prediction :: String,
    rnazResults :: [RNAzResult],
    rnazConsensus :: RNAzConsensus
  }
  deriving (Show, Eq)

-- | Datatype contains all result elements for each member of the input alignment            
data RNAzResult = RNAzResult
  { header :: String,
    resultSequence :: String,
    dotBracket :: String,         
    minimumFreeEnergy :: Double,
    zScore :: Double,
    --S = huffling, R = regression
    zscoreCalculationApproach :: Char
  }
  deriving (Show, Eq)

-- | Datatype contains all elements for the consenus result of input alignment
data RNAzConsensus = RNAzConsensus
  { consensusSequence :: String,
    consensusDotBracket :: String         
  }
  deriving (Show, Eq)           
