{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module M where

import Control.Arrow

f a b c = proc x -> (a -< x) <+> (b -< x) <+> (c -< x)
