{- ORACLE_TEST
id: type-applications-no-space
category: expressions
expected: xfail
reason: parser intentionally disabled
-}
module TypeApplicationsNoSpace where

f :: a -> a
f x = x

x :: Int
x = f @Int 1
