{- ORACLE_TEST
id: standalone-deriving-basic
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingBasic where

data Box a = Box a

deriving instance Eq a => Eq (Box a)
