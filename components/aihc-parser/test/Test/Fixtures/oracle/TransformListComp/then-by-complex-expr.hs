{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenByComplexExpr where
f xs = [ (x, y) | x <- xs, y <- ys, then sortWith by (x + y) ]
