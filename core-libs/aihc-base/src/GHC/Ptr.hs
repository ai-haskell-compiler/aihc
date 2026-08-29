{-# LANGUAGE MagicHash #-}

module GHC.Ptr
  ( Ptr (..),
    FunPtr (..),
  )
where

import GHC.Prim (Addr#)

data Ptr a = Ptr Addr#

data FunPtr a = FunPtr Addr#
