{- ORACLE_TEST xfail else-do layout block closed prematurely by inner then/else -}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: inner 'if-then-else' inside 'else do' layout block where the
-- outer if-then-else is on one line. The 'do' in the else branch starts a
-- layout block, but the lexer's closeBeforeThenElse incorrectly closes the
-- do-layout when it encounters the inner 'then' (whose column is less than
-- the outer 'else' column). GHC accepts this; aihc-parser rejects it.
-- Minimized from http-download-0.2.1.0 Verified.hs:322.
module DoAndIfThenElseElseDoIfLayout where

foo = if a then b else do
    if c then d else e
