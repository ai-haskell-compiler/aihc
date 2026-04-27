{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CApiFFI #-}

module CtypeNewtype where

import Foreign.C.Types (CInt (..))

newtype {-# CTYPE "signed int" #-} Fixed = Fixed CInt
