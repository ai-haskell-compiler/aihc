{- ORACLE_TEST pass -}
{-# LANGUAGE MagicHash #-}
module A where

import GHC.Exts

-- Negative MagicHash literal in expression context.
-- GHC treats -42# as a single literal token, not negation applied to 42#.
x :: Int#
x = -42#
