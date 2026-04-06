{- ORACLE_TEST pass -}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingForall where

class C a

deriving instance forall a. C a
