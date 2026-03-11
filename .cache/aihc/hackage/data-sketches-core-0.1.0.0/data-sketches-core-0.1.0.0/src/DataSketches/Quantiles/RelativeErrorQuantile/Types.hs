module DataSketches.Quantiles.RelativeErrorQuantile.Types where
import Control.Monad.Primitive
import qualified DataSketches.Quantiles.RelativeErrorQuantile.Internal.InequalitySearch as IS

data Criterion = (:<) | (:<=)
  deriving (Show, Eq)

instance IS.InequalitySearch Criterion where
  inequalityCompare c = case c of
    (:<) -> IS.inequalityCompare (IS.:<)
    (:<=) -> IS.inequalityCompare(IS.:<=)
  resolve c = case c of
    (:<) -> IS.resolve (IS.:<)
    (:<=) -> IS.resolve (IS.:<=)
  getIndex c = case c of
    (:<) -> IS.getIndex (IS.:<)
    (:<=) -> IS.getIndex (IS.:<=)


data RankAccuracy 
  = HighRanksAreAccurate 
  -- ^ High ranks are prioritized for better accuracy.
  | LowRanksAreAccurate
  -- ^ Low ranks are prioritized for better accuracy
  deriving (Show, Eq)
