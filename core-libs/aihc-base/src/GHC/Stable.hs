{-# LANGUAGE MagicHash #-}

module GHC.Stable (StablePtr (..)) where

import GHC.Prim (StablePtr#)

data StablePtr a = StablePtr (StablePtr# a)
