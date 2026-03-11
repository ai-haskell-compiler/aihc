module Data.List.HT (
   -- * Improved standard functions
   L.inits,
   L.tails,
   L.groupBy,
   L.group,
   L.unzip,
   L.partition,
   L.span,
   L.break,
   -- * Split
   L.chop,
   L.breakAfter,
   L.takeUntil,
   L.segmentAfter,
   L.segmentBefore,
   L.segmentAfterJust, segmentAfterMaybe,
   L.segmentBeforeJust, segmentBeforeMaybe,
   L.segmentAfterRight,
   L.segmentBeforeRight,
   L.removeEach,
   L.splitEverywhere,
   --  * inspect ends of a list
   L.splitLast,
   L.viewL,
   L.viewR,
   L.switchL,
   L.switchR,
   -- * List processing starting at the end
   L.dropRev,
   L.takeRev,
   L.splitAtRev,
   dropWhileRev,
   takeWhileRev,
   -- * List processing with Maybe and Either
   L.maybePrefixOf,
   L.maybeSuffixOf,
   L.partitionMaybe,
   L.takeWhileJust,
   L.dropWhileNothing,
   L.breakJust,
   L.spanJust,
   L.unzipEithers,
   -- * Sieve and slice
   L.sieve,
   L.sliceHorizontal,
   L.sliceVertical,
   -- * Search&replace
   L.search,
   L.replace,
   L.multiReplace,
   -- * Lists of lists
   L.shear,
   L.shearTranspose,
   L.outerProduct,
   -- * Miscellaneous
   L.takeWhileMulti,
   L.rotate,
   L.mergeBy,
   L.allEqual,
   L.isAscending,
   L.isAscendingLazy,
   L.mapAdjacent,
   L.mapAdjacent1,
   L.equalWith,
   L.range,
   L.padLeft,
   L.padRight,
   L.iterateAssociative,
   L.iterateLeaky,
   L.lengthAtLeast,
   L.lengthAtMost,
   ) where

import qualified Data.List.HT.Private as L
import qualified Data.List.Reverse.StrictElement as Rev

{-# DEPRECATED dropWhileRev "Use dropWhile from Data.List.Reverse.StrictElement or Data.List.Reverse.StrictSpine instead" #-}
dropWhileRev :: (a -> Bool) -> [a] -> [a]
dropWhileRev = Rev.dropWhile

{-# DEPRECATED takeWhileRev "Use takeWhile from Data.List.Reverse.StrictElement or Data.List.Reverse.StrictSpine instead" #-}
takeWhileRev :: (a -> Bool) -> [a] -> [a]
takeWhileRev = Rev.takeWhile

{-# DEPRECATED segmentBeforeMaybe "use segmentBeforeJust instead" #-}
segmentBeforeMaybe :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
segmentBeforeMaybe = L.segmentBeforeJust

{-# DEPRECATED segmentAfterMaybe "use segmentAfterJust instead" #-}
segmentAfterMaybe :: (a -> Maybe b) -> [a] -> ([([a], b)], [a])
segmentAfterMaybe = L.segmentAfterJust
