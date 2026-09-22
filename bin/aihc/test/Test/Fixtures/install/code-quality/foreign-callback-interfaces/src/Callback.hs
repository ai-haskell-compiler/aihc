module Callback where

import GHC.Prim

data CInt = CInt Int32#
data FunPtr a = FunPtr Addr#
newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
type Output = CInt -> IO CInt

foreign import ccall "wrapper" wrap :: Output -> IO (FunPtr Output)
foreign import ccall "dynamic" invoke :: FunPtr Output -> Output
