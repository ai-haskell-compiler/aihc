{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module M where

f = proc x -> case [] of { _ -> a -< x } `op` (b -< x)
g = proc x -> (a -< x) `op` if True then b -< x else c -< x
h = proc x -> (a -< x) `op` \_ -> b -< x
i = proc x -> (a -< x) `op` let y = x in b -< y
j = proc x -> (a -< x) `op` do { b -< x }
