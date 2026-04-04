{- ORACLE_TEST xfail Mantissa boxed Word# literal with 0## -}
{-# LANGUAGE MagicHash #-}

module WordLiteralDoubleHash where

import GHC.Exts

f = W# 0##
