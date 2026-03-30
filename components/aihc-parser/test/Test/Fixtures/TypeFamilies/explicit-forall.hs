{- ORACLE_TEST
id: explicit-forall
category: declarations
expected: xfail
reason: data instance with explicit forall
-}
{-# LANGUAGE TypeFamilies, ExplicitForAll, PolyKinds #-}
module ExplicitForAll where

data family F a
data instance forall a (b :: Proxy a). F (Proxy b) = FProxy Bool
