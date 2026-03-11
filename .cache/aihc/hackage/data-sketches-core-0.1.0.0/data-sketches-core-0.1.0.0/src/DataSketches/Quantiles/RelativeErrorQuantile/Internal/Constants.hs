module DataSketches.Quantiles.RelativeErrorQuantile.Internal.Constants where
-- Constants
import Data.Word

sqrt2 :: Double 
sqrt2 = sqrt 2

initNumberOfSections :: Num a => a
initNumberOfSections = 3

minK :: Num a => a
minK = 4

nomCapMulti :: Num a => a
nomCapMulti = 2

relRseFactor :: Double
relRseFactor = sqrt (0.0512 / fromIntegral initNumberOfSections)

fixRseFactor :: Double
fixRseFactor = 0.084
