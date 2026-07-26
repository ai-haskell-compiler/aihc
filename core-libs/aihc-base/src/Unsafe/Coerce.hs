{-# LANGUAGE MagicHash #-}

module Unsafe.Coerce (unsafeCoerce) where

import GHC.Prim (unsafeCoerce#)

unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#
