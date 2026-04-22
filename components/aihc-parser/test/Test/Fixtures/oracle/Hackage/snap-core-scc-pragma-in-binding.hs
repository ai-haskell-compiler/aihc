{- ORACLE_TEST xfail SCC pragma dropped during roundtrip causing fingerprint mismatch -}
module A where
f = g
  where
    g = {-# SCC "name" #-} undefined
