{-# LANGUAGE MagicHash #-}

module GHC.Ptr
  ( Ptr (..),
    FunPtr (..),
    nullPtr,
    nullFunPtr,
  )
where

import GHC.Prim (Addr#, nullAddr#)

data Ptr a = Ptr Addr#

data FunPtr a = FunPtr Addr#

nullPtr :: Ptr a
nullPtr = Ptr nullAddr#

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr#
