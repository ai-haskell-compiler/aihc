{- ORACLE_TEST
id: standalone-deriving-type-application
category: declarations
expected: pass
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingTypeApplication where

data Proxy a = Proxy

deriving instance Eq (Proxy Int)
deriving instance Show (Proxy Int)
