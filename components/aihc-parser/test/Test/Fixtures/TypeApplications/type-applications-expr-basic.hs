{- ORACLE_TEST
id: type-applications-expr-basic
category: expressions
expected: xfail
reason: parser intentionally disabled
-}
module TypeApplicationsExprBasic where

f :: a -> b -> a
f x _ = x

h :: (Int, Int)
h = (f 2 @Int 3, f @Int @Bool 1 True)
