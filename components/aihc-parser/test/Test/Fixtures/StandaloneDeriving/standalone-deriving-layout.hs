{- ORACLE_TEST
id: standalone-deriving-layout
category: declarations
expected: pass
-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingLayout where

data W a = W a

deriving instance
  Show a => Show (W a)
