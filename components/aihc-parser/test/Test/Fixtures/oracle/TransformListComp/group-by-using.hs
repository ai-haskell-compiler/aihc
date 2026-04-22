{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module GroupByUsing where
import GHC.Exts (the, groupWith)
f xs = [ (the x, y) | x <- xs, y <- ys, then group by x using groupWith ]
