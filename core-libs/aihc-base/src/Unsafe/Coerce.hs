{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}

module Unsafe.Coerce (unsafeCoerce) where

foreign import prim unsafeCoerce# :: a -> b

unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#
