{- ORACLE_TEST
id: standalone-deriving-strategy-stock
category: declarations
expected: pass
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingStrategyStock where

data Box a = Box a

deriving stock instance Eq a => Eq (Box a)
deriving stock instance Show a => Show (Box a)
