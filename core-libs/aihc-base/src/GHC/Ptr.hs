{-# LANGUAGE MagicHash #-}

module GHC.Ptr (Ptr (..)) where

import GHC.Prim (Addr#)

data Ptr a = Ptr Addr#
