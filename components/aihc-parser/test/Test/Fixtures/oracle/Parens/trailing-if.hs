{- ORACLE_TEST pass -}
module M where

x = do fn $ val ++ if True then "\n" else ""

x = show a ++ ":" ++ show b ++ if b == d then "" else '-' : show d
