module PureSAT.Stats (
    Stats,
    newStats,
    readStatsConflicts, incrStatsConflicts,
    readStatsRestarts, incrStatsRestarts,
    readStatsLearnt, incrStatsLearnt,
    readStatsClauses, incrStatsClauses,
    readStatsLearntLiterals, incrStatsLearntLiterals,
) where

import PureSAT.Base
import PureSAT.Prim

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

data Stats s = MkStats (MutablePrimArray s Int)

newStats :: ST s (Stats s)
newStats = MkStats <$> do
    arr <- newPrimArray size
    setPrimArray arr 0 size 0
    return arr
  where
    size = 5

readStatsConflicts :: Stats s -> ST s Int
incrStatsConflicts :: Stats s -> ST s ()
(readStatsConflicts, incrStatsConflicts) = makeStat 0

readStatsRestarts :: Stats s -> ST s Int
incrStatsRestarts :: Stats s -> ST s ()
(readStatsRestarts, incrStatsRestarts) = makeStat 1

readStatsLearnt :: Stats s -> ST s Int
incrStatsLearnt :: Stats s -> ST s ()
(readStatsLearnt, incrStatsLearnt) = makeStat 2

readStatsClauses :: Stats s -> ST s Int
incrStatsClauses :: Stats s -> ST s ()
(readStatsClauses, incrStatsClauses) = makeStat 3

readStatsLearntLiterals :: Stats s -> ST s Int
incrStatsLearntLiterals :: Stats s -> Int -> ST s ()
(readStatsLearntLiterals, incrStatsLearntLiterals) = makeStatBy 4

makeStat :: Int -> (Stats s1 -> ST s1 Int, Stats s2 -> ST s2 ())
makeStat i = (read_, incr_)
  where
    read_ (MkStats arr) = readPrimArray arr i
    incr_ (MkStats arr) = do
        x <- readPrimArray arr i
        writePrimArray arr i (x + 1)

makeStatBy :: Int -> (Stats s1 -> ST s1 Int, Stats s2 -> Int -> ST s2 ())
makeStatBy i = (read_, incr_)
  where
    read_ (MkStats arr) = readPrimArray arr i
    incr_ (MkStats arr) !n = do
        x <- readPrimArray arr i
        writePrimArray arr i (x + n)
