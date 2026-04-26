{- ORACLE_TEST xfail "we shouldn't wrap 'Just a' in parens" -}
module M where

n = \(Just a :: Maybe ()) -> a
