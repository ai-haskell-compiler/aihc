{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module ThenFByE where
f xs = [ x | x <- xs, then sortWith by x ]
