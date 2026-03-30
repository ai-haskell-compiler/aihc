{- ORACLE_TEST
id: standalone-deriving-multi-arg
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingMultiArg where

data Triple a b c = Triple a b c

deriving instance (Eq a, Eq b, Eq c) => Eq (Triple a b c)
