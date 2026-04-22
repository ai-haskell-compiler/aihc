{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
module LayoutExpressions where

-- do expression in unboxed tuple
f1 = (# do x <- pure 1; pure x #)

-- if expression in unboxed tuple
f2 x = (# if x then 1 else 2 #)

-- let expression in unboxed tuple
f3 = (# let y = 1 in y #)

-- lambda in unboxed tuple
f4 = (# \x -> x + 1 #)

-- lambda-case in unboxed tuple
f5 = (# \case 0 -> "zero"; _ -> "other" #)

-- nested case in unboxed tuple
f6 x y = (# case x of { 0 -> case y of { 0 -> 0; _ -> 1 }; _ -> 2 } #)

-- multiple layout expressions in unboxed tuple
f7 x = (# case x of y -> y, do pure 1 #)
