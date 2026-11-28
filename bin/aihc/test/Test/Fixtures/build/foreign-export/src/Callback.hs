module Callback (callExports) where

import Data.Int (Int64)

-- | The C function calls both exports.
foreign import ccall safe "c_call_exports" c_callExports :: Int64 -> IO Int64

-- | An export with an entity string is the C function of that name.
foreign export ccall "hs_twice" twice :: Int64 -> Int64

-- | An export without an entity string has the name of the value.
foreign export ccall hs_bump :: Int64 -> IO Int64

twice :: Int64 -> Int64
twice value = value * 2

hs_bump :: Int64 -> IO Int64
hs_bump value = pure (value + 100)

callExports :: Int64 -> IO Int64
callExports = c_callExports
