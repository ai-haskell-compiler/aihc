{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenParenFn where
f xs = [ x | x <- xs, then (take 3) ]
