{- ORACLE_TEST xfail CTYPE pragma on newtype declaration is not preserved in roundtrip -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CApiFFI #-}

module CtypeNewtype where

import Foreign.C.Types (CInt (..))

newtype {-# CTYPE "signed int" #-} Fixed = Fixed CInt
