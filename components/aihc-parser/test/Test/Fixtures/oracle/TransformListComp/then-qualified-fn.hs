{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenQualifiedFn where
f xs = [ x | x <- xs, then Data.List.sort ]
