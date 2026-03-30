{- ORACLE_TEST
id: standalone-deriving-with-context
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingWithContext where

data PairBox a b = PairBox a b

deriving instance (Eq a, Eq b) => Eq (PairBox a b)
