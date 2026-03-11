-- | This module contains a data structure for
--   RNAup output
--   For more information on RNAup consult: <http://www.tbi.univie.ac.at/RNA/RNAup.html>

module Bio.RNAupData where
    
-- | Data structure for individual interaction between a target and query nucleic acid seqence, contains all (sub)-optimal interactions 
data RNAupInteraction = RNAupInteraction
  { 
    upQueryIdentifier :: String,
    upTargetIdentifier :: String,
    --first interaction region is minimum free energy interaction
    upInteractions :: [RNAupInteractionRegion]
  }
  deriving (Show, Eq)

-- | Data structure for (sub-) optimal interaction region 
data RNAupInteractionRegion = RNAupInteractionRegion
 {
  upSecondaryStructure :: String,
  upQueryDuplexBegin :: Int,
  upQueryDuplexEnd :: Int,
  upTargetDuplexBegin :: Int,
  upTargetDuplexEnd :: Int,
  upDuplexEnergy :: Double,
  upDuplexEnergyWithoutAccessiblity :: Maybe Double,
  upQueryAccessiblity :: Maybe Double,
  upTargetAccessibility :: Maybe Double,
  upQuerySequence :: String,
  upTargetSequence :: String,
  upOutputFileName :: Maybe String
 }
 deriving (Show, Eq)
