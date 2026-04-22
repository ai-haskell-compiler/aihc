{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module GroupAfterGuard where
import GHC.Exts (groupWith)
f xs = [ x | x <- xs, x > 0, then group by x using groupWith ]
