{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenFSingle where
f xs = [ x | x <- xs, then reverse ]
