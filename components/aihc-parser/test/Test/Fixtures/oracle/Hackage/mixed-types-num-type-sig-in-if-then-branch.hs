{- ORACLE_TEST xfail type signature in if-then branch rejected -}
module A where
f = if True then x :: Int else x
