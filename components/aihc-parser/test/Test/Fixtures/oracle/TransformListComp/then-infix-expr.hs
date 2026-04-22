{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenInfixExpr where
f xs = [ x | x <- xs, then reverse . sort ]
