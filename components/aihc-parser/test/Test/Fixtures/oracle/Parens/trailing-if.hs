{- ORACLE_TEST xfail "we add parens around if-statements when we shouldn't" -}
module M where

x = do fn $ val ++ if True then "\n" else ""

x = show a ++ ":" ++ show b ++ if b == d then "" else '-' : show d
