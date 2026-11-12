{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}

module Unsafe.Coerce (unsafeCoerce, unsafeCoerceUnlifted, unsafeCoerce#) where

import GHC.Prim (unsafeCoerce#)
import GHC.Types (UnliftedType)

unsafeCoerce :: a -> b
unsafeCoerce = unsafeCoerce#

unsafeCoerceUnlifted :: forall (a :: UnliftedType) (b :: UnliftedType). a -> b
unsafeCoerceUnlifted = unsafeCoerce#
