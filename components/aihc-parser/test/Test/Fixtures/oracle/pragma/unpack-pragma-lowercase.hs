{- ORACLE_TEST pass -}
module UnpackPragmaLowercase where

data Inner = Inner !Int

data Nested = Nested {-# unpack #-} !Inner
