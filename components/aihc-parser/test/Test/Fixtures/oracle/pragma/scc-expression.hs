{- ORACLE_TEST pass -}

module SCCPragmaExpression where

mapL f lx = cachedLatch ({-# SCC mapL #-} f <$> getValueL lx)
