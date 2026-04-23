{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module A where
import GHC.Exts (the, groupWith)
f ts = [ (the x, y) | t <- ts, let x = t, let y = t, then group by x using groupWith ]
