{- ORACLE_TEST xfail "we shouldn't wrap 'bit (w - 1)' in parens" -}
module M where

n = -bit (w - 1)
