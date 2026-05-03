{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

data Proxy a = Proxy

mulProxy :: Proxy a -> Proxy b -> Proxy (a * b)
mulProxy _ _ = Proxy
