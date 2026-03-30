{- ORACLE_TEST xfail data instance with explicit forall -}
{-# LANGUAGE TypeFamilies, ExplicitForAll, PolyKinds #-}
module ExplicitForAll where

data family F a
data instance forall a (b :: Proxy a). F (Proxy b) = FProxy Bool