{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

data Proxy (a :: *) = Proxy

x :: Proxy (*)
x = Proxy @(*)
