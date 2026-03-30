{- ORACLE_TEST
id: standalone-deriving-nested-type
category: declarations
expected: pass
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module StandaloneDerivingNestedType where

data Wrapper f a = Wrapper (f a)

deriving instance Eq (f a) => Eq (Wrapper f a)
deriving instance Show (f a) => Show (Wrapper f a)
