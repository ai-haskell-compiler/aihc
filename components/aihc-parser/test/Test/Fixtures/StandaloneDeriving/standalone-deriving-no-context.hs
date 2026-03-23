{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingNoContext where

data Pair = Pair Int Int

deriving instance Eq Pair
deriving instance Show Pair
