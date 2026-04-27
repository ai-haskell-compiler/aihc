{- ORACLE_TEST xfail SCC pragma in expression context not yet supported -}

module SCCPragmaExpression where

mapL f lx = cachedLatch ({-# SCC mapL #-} f <$> getValueL lx)
