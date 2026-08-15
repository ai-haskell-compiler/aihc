{-# LANGUAGE MagicHash #-}

module GHC.Prim.PtrEq (eqStableName#) where

import GHC.Prim (Int#, StableName#)

foreign import prim eqStableName# :: StableName# a -> StableName# b -> Int#
