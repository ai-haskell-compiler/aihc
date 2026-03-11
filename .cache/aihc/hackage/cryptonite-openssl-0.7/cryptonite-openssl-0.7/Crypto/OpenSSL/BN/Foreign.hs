{-# LANGUAGE ForeignFunctionInterface #-}
module Crypto.OpenSSL.BN.Foreign where

import           Foreign.C.Types
import           Foreign.Ptr

data BN_CTX
data BIGNUM

foreign import ccall unsafe "&BN_CTX_free"
    ssl_bn_ctx_free :: FunPtr (Ptr BN_CTX -> IO ())

foreign import ccall unsafe "BN_CTX_new"
    ssl_bn_ctx_new :: IO (Ptr BN_CTX)

foreign import ccall unsafe "BN_new"
    ssl_bn_new :: IO (Ptr BIGNUM)

foreign import ccall unsafe "&BN_free"
    ssl_bn_free :: FunPtr (Ptr BIGNUM -> IO ())

foreign import ccall unsafe "BN_num_bits"
    ssl_bn_num_bits :: Ptr BIGNUM -> IO CInt

foreign import ccall unsafe "BN_bn2bin"
    ssl_bn_2bin :: Ptr BIGNUM -> Ptr CUChar -> IO CInt

foreign import ccall unsafe "BN_bin2bn"
    ssl_bn_bin2 :: Ptr CUChar -> CInt -> Ptr BIGNUM -> IO (Ptr BIGNUM)

-- setter

foreign import ccall unsafe "BN_set_word"
    ssl_bn_set_word :: Ptr BIGNUM -> CULong -> IO CInt

ssl_bn_zero :: Ptr BIGNUM -> IO CInt
ssl_bn_zero p = ssl_bn_set_word p 0

ssl_bn_one :: Ptr BIGNUM -> IO CInt
ssl_bn_one p = ssl_bn_set_word p 1

-- arithmetic operations

foreign import ccall unsafe "BN_add" -- r = a + b
    ssl_bn_add :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> IO CInt

foreign import ccall unsafe "BN_sub" -- r = a - b
    ssl_bn_sub :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> IO CInt

foreign import ccall unsafe "BN_mul" -- r = a * b
    ssl_bn_mul :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_sqr" -- r = sqrt(a)
    ssl_bn_sqr :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_div" -- div,rem = a / b
    ssl_bn_div :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

--foreign import ccall unsafe "BN_mod" -- r = a % b
--    ssl_bn_mod :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_nnmod" -- r = a % b
    ssl_bn_nnmod :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

-- arithmetic modular operations

foreign import ccall unsafe "BN_mod_add" -- r = a + b [m]
    ssl_bn_mod_add :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_mod_sub" -- r = a - b [m]
    ssl_bn_mod_sub :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_mod_mul" -- r = a * b [m]
    ssl_bn_mod_mul :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_mod_sqr" -- r = sqrt a [m]
    ssl_bn_mod_sqr :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

-- exponantiations

foreign import ccall unsafe "BN_exp" -- r = a^p
    ssl_bn_exp :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_mod_exp" -- r = a^p [m]
    ssl_bn_mod_exp :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "BN_gcd" -- r = gcd(a,b)
    ssl_bn_gcd :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt


-- bn_num_bytes is a macro,
ssl_bn_num_bytes :: Ptr BIGNUM -> IO CInt
ssl_bn_num_bytes ptr = do
    bits <- ssl_bn_num_bits ptr
    return $ ((bits + 7) `div` 8)
