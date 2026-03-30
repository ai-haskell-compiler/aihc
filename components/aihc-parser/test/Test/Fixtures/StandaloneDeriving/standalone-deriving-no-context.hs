{- ORACLE_TEST
id: standalone-deriving-no-context
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingNoContext where

data Pair = Pair Int Int

deriving instance Eq Pair
deriving instance Show Pair
