{- ORACLE_TEST pass -}
-- From GHC testsuite/tests/layout/layout005.hs.
module M where

-- GHC's Lexer.x had a piece of code like this

f = if True then do
        case () of
            () -> ()
            else ()
