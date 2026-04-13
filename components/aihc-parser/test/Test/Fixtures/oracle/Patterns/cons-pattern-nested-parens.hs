{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

-- Roundtrip now works correctly for nested cons patterns
f xs = go xs where
  go (x1:x2:xs) = (x1, x2) : go (x2:xs)
  go [x] = []
  go _ = []
