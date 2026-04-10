{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

newtype Managed a = Managed { (>>-) :: a -> a }
