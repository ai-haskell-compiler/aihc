{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies, ExplicitForAll, PolyKinds #-}
module ExplicitForAll where

data family F a
data instance forall a (b :: Proxy a). F (Proxy b) = FProxy Bool