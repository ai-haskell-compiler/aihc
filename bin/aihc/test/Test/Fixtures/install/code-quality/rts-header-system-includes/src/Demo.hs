module Demo where

import GHC.Prim

data Ptr a = Ptr Addr#

data CChar = CChar Int8#

data CDouble = CDouble Double#

data CLong = CLong Int64#

newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

foreign import ccall unsafe "demo_paste_double" c_pasteDouble :: Ptr CChar -> CDouble -> IO CLong
