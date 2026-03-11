-- | This module contains a hierarchical data structure for
--   RNAplex output
--   For more information on RNAplex consult: <http://www.bioinf.uni-leipzig.de/Software/RNAplex/>

module Bio.RNAplexData where
    
-- | Data structure for individual interaction between a target and query nucleic acid seqence
data RNAplexInteraction = RNAplexInteraction
  { 
    targetIdentifier :: String,
    queryIdentifier :: String,
    plexSecondaryStructure :: String,
    targetDuplexBegin :: Int,
    targetDuplexEnd :: Int,
    queryDuplexBegin :: Int,
    queryDuplexEnd :: Int,
    duplexEnergy :: Double,
    duplexEnergyWithoutAccessiblity :: Maybe Double,
    queryAccessiblity :: Maybe Double,
    targetAccessibility :: Maybe Double,
    prefilterStart ::  Maybe Int,
    prefilterStop :: Maybe Int,
    prefilterEnergy :: Maybe Double
  }
  deriving (Show, Eq)
