{-# LANGUAGE MagicHash #-}

module Foreign.Marshal.Alloc (allocaBytes) where

import GHC.IO (IO)
import GHC.Int (Int)
import GHC.Prim (raise#)
import GHC.Ptr (Ptr)

allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
allocaBytes _ _ = raise# ()
