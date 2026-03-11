module Data.Range.Algebra.Range where

import Data.Range.Data
import Data.Range.RangeInternal (exportRangeMerge, loadRanges)
import Data.Range.Algebra.Internal

import Control.Monad.Free

rangeAlgebra :: (Ord a) => Algebra RangeExprF [Range a]
rangeAlgebra = exportRangeMerge . iter rangeMergeAlgebra . Free . fmap (Pure . loadRanges)
