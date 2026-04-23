{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}
module LetBeforeGroup where
import GHC.Exts (the, groupWith)
f xs = [ (the k, vs) | x <- xs, let k = fst x, let vs = snd x, then group by k using groupWith ]
