{- ORACLE_TEST xfail newtype with operator-named record field fails to parse -}
{-# LANGUAGE GHC2021 #-}

newtype Managed a = Managed { (>>-) :: a -> a }
