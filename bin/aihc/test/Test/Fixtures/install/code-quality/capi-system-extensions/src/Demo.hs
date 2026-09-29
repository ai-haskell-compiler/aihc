module Demo where

import GHC.Prim

data CInt = CInt Int32#
data Ptr a = Ptr Addr#
data CChar = CChar Int8#
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))

-- ptsname is an X/Open function: glibc declares it only when a feature
-- macro is set before the first system header, as unix's
-- System.Posix.Terminal expects.
foreign import capi unsafe "stdlib.h ptsname" c_ptsname :: CInt -> IO (Ptr CChar)
