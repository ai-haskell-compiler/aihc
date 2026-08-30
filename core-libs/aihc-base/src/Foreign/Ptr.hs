module Foreign.Ptr
  ( Ptr (..),
    FunPtr (..),
    nullPtr,
    nullFunPtr,
  )
where

import GHC.Ptr (FunPtr (..), Ptr (..), nullFunPtr, nullPtr)
