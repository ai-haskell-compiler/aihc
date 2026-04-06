{- ORACLE_TEST xfail DEPRECATED and WARNING pragmas on instances not supported by parser -}
{-# LANGUAGE StandaloneDeriving #-}

module InstanceWarnings where

data T1 = MkT1
newtype T2 = MkT2 Int

instance {-# DEPRECATED "Do not use Show T1" #-} Show T1 where
  show MkT1 = "MkT1"

newtype G1 = MkG1 Int
instance {-# WARNING "Do not use Eq G1" #-} Eq G1 where
  MkG1 a == MkG1 b = a == b

deriving instance {-# DEPRECATED "Eq T2 will be removed" #-} Eq T2
