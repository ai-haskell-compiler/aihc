module Foreign.StablePtr
  ( StablePtr,
    newStablePtr,
    deRefStablePtr,
    freeStablePtr,
    castStablePtrToPtr,
    castPtrToStablePtr,
  )
where

import GHC.Stable (StablePtr, castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
