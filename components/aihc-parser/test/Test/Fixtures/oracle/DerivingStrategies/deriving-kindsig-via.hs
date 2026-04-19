{- ORACLE_TEST pass -}
{-# LANGUAGE DerivingVia #-}
module DerivingKindSigVia where

class CK a b

newtype T a = T a
  deriving (CK a [a]) via [a]
