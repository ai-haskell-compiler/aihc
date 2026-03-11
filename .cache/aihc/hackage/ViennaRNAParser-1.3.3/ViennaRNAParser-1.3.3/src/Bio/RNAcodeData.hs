-- | This module contains a hierarchical data structure for
--   RNAcode output
--   For more information on RNAcode consult: <http://wash.github.io/rnacode/>

module Bio.RNAcodeData where
    
-- | All elements of RNAcode output are contained in this datatype 
data RNAcode = RNAcode
  { 
    rnacodeHits :: [RNAcodeHit],
    rcAlignmentNumber :: Maybe Int,
    rcTime :: Maybe Double,
    rcSampleNumber :: Maybe Int,
    rcDelta :: Maybe Double,
    rcBigOmega :: Maybe Double,
    rcSmallOmega :: Maybe Double,
    rcStopPenalty :: Maybe Double
  }
  deriving (Show, Eq)

-- | RNAcode Hit
data RNAcodeHit = RNAcodeHit
  { 
    hss :: Int,
    strand :: Char,
    frame :: Int,
    hitLength :: Int,
    from :: Int,
    to :: Int,
    name :: String,
    start :: Int,
    end :: Int,
    score :: Double,
    pvalue :: Double
  }
  deriving (Show, Eq)
