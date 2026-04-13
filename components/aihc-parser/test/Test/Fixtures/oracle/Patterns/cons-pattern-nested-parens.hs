{- ORACLE_TEST xfail reason="roundtrip adds extra parentheses around nested cons patterns" -}
{-# LANGUAGE Haskell2010 #-}

-- Roundtrip fails: parser adds extra parentheses around nested cons patterns
f xs = go xs where
  go (x1:x2:xs) = (x1, x2) : go (x2:xs)
  go [x] = []
  go _ = []
