module Foreign.Marshal.Alloc
  ( alloca,
  )
where

import Foreign.Ptr (Ptr)

alloca :: (Ptr a -> IO b) -> IO b
alloca = alloca
