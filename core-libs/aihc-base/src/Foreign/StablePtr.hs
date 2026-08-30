{-# LANGUAGE MagicHash #-}

module Foreign.StablePtr
  ( StablePtr,
    castPtrToStablePtr,
  )
where

import GHC.Prim (unsafeCoerce#)
import GHC.Ptr (Ptr)
import GHC.Stable (StablePtr)

castPtrToStablePtr :: Ptr () -> StablePtr a
castPtrToStablePtr = unsafeCoerce#
