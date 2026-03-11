module Crypto.OpenSSL.Random
    ( randBytes
    ) where

import           Foreign.C.Types
import           Crypto.OpenSSL.Misc
import           Foreign.Ptr
import qualified Data.ByteArray as B

randBytes :: B.ByteArray byteArray => Int -> IO byteArray
randBytes n = B.alloc n $ \ptr ->
    check $ openssl_rand_bytes ptr (fromIntegral n)

foreign import ccall unsafe "RAND_bytes"
    openssl_rand_bytes :: Ptr CUChar -> CInt -> IO CInt
