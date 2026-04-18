{- ORACLE_TEST pass -}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: inner 'if-then-else' inside 'else do' layout block where the
-- outer if-then-else is on one line. The else-do layout must stay open across
-- the nested conditional and only close when the outer branch ends.
-- Minimized from http-download-0.2.1.0 Verified.hs:322.
module DoAndIfThenElseElseDoIfLayout where

foo = if a then b else do
    if c then d else e
