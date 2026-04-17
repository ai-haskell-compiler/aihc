{- ORACLE_TEST pass -}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: inner 'if-then-else' inside 'then do' layout block where the
-- outer if-then-else has 'then do' on one line. The then-do layout must stay
-- open across the nested conditional and only close for the outer else.
module DoAndIfThenElseThenDoIfLayout where

foo = if a then do
    if c then d else e
    else b
