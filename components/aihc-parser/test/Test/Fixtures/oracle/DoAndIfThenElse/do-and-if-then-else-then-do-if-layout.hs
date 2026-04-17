{- ORACLE_TEST xfail then-do layout block closed prematurely by inner then/else -}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: inner 'if-then-else' inside 'then do' layout block where the
-- outer if-then-else has 'then do' on one line. The 'do' in the then branch
-- starts a layout block, but the lexer's closeBeforeThenElse incorrectly
-- closes the do-layout when it encounters the inner 'then' (whose column is
-- less than the outer 'then' column). GHC accepts this; aihc-parser rejects
-- it. Minimized from http-download-0.2.1.0 Verified.hs:322.
module DoAndIfThenElseThenDoIfLayout where

foo = if a then do
    if c then d else e
    else b
