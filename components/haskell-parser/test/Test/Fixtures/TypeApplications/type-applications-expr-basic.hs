module TypeApplicationsExprBasic where

f :: a -> b -> a
f x _ = x

x :: Int
x = f 2 @Int 3
