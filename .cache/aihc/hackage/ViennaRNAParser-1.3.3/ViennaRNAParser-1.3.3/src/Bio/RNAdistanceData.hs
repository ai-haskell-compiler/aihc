-- | This module contains a data structure for RNAdistance output
--   For more information on RNAdistance consult: <>

module Bio.RNAdistanceData where

-- | Data structure
data RNAdistance = RNAdistance
  {
    secondaryStructureDistance :: Int
  }
  deriving (Show, Eq)
