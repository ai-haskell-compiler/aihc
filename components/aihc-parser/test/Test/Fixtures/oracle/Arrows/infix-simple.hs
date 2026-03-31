{- ORACLE_TEST xfail arrow infix operator simple -}
{-# LANGUAGE Arrows #-}
module ArrowInfixSimple where

import Control.Arrow

f g h = proc x -> (g -< x) <+> (h -< x + 1)
