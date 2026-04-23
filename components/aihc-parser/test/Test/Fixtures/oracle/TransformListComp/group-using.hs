{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module GroupUsing where
f xs = [ x | x <- xs, then group using inits ]
