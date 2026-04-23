{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenF where
f xs = [ x | x <- xs, then take 5 ]
