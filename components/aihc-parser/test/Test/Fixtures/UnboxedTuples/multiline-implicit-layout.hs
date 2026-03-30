{- ORACLE_TEST
id: multiline-implicit-layout
category: layout
expected: pass
reason: multiline unboxed tuples with implicit layout
-}
{-# LANGUAGE UnboxedTuples #-}
module MultilineImplicitLayout where

x :: Int -> (# Int, Int #)
x n =
  (#
    n
  , 2
  #)

f :: (# Int, Int #) -> Int
f
  (# a
   , _
   #) = a

g :: (# Int
      , Int #) -> (# Int
                  , Int #)
g t = t
