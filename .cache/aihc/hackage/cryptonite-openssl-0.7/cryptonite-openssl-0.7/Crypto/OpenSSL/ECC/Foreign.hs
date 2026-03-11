{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Crypto.OpenSSL.ECC.Foreign where

import           Foreign.C.Types
--import           Foreign.C.String (withCString)
import           Foreign.Ptr

import           Crypto.OpenSSL.BN.Foreign

#include <openssl/opensslconf.h>

data EC_GROUP
data EC_POINT
data EC_KEY

type PointConversionFormT = CInt

#ifdef OPENSSL_NO_EC2M
gf2m_not_supported :: a
gf2m_not_supported = error "GF2m not supported"
#endif

foreign import ccall unsafe "OBJ_txt2nid"
    ssl_obj_txt2nid :: Ptr CChar -> IO CInt

foreign import ccall unsafe "&EC_GROUP_free"
    ssl_group_free :: FunPtr (Ptr EC_GROUP -> IO ())

foreign import ccall unsafe "EC_GROUP_new_by_curve_name"
    ssl_group_new_by_curve_name :: CInt -> IO (Ptr EC_GROUP)

#ifdef OPENSSL_NO_EC2M
ssl_group_new_curve_GF2m :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO (Ptr EC_GROUP)
ssl_group_new_curve_GF2m = gf2m_not_supported
#else
foreign import ccall unsafe "EC_GROUP_new_curve_GF2m"
    ssl_group_new_curve_GF2m :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO (Ptr EC_GROUP)
#endif

foreign import ccall unsafe "EC_GROUP_new_curve_GFp"
    ssl_group_new_curve_GFp :: Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO (Ptr EC_GROUP)

foreign import ccall unsafe "EC_GROUP_get_order"
    ssl_group_get_order :: Ptr EC_GROUP -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_GROUP_get_cofactor"
    ssl_group_get_cofactor :: Ptr EC_GROUP -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_GROUP_get_degree"
    ssl_group_get_degree :: Ptr EC_GROUP -> IO CInt

foreign import ccall unsafe "EC_GROUP_get_curve_GFp"
    ssl_group_get_curve_gfp :: Ptr EC_GROUP -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

#ifdef OPENSSL_NO_EC2M
ssl_group_get_curve_gf2m :: Ptr EC_GROUP -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt
ssl_group_get_curve_gf2m = gf2m_not_supported
#else
foreign import ccall unsafe "EC_GROUP_get_curve_GF2m"
    ssl_group_get_curve_gf2m :: Ptr EC_GROUP -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt
#endif

foreign import ccall unsafe "EC_GROUP_get0_generator"
    ssl_group_get0_generator :: Ptr EC_GROUP -> IO (Ptr EC_POINT)

foreign import ccall unsafe "EC_GROUP_get_curve_name"
    ssl_group_get_curve_name :: Ptr EC_GROUP -> IO CInt

foreign import ccall unsafe "EC_GROUP_set_generator"
    ssl_group_set_generator :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> IO CInt

-------------------------------
-- EC_POINT related functions

foreign import ccall unsafe "&EC_POINT_free"
    ssl_point_free_funptr :: FunPtr (Ptr EC_POINT -> IO ())

foreign import ccall unsafe "EC_POINT_free"
    ssl_point_free :: Ptr EC_POINT -> IO ()

foreign import ccall unsafe "&EC_POINT_clear_free"
    ssl_point_clear_free :: FunPtr (Ptr EC_POINT -> IO ())

foreign import ccall unsafe "EC_POINT_new"
    ssl_point_new :: Ptr EC_GROUP -> IO (Ptr EC_POINT)

foreign import ccall unsafe "EC_POINT_dup"
    ssl_point_dup :: Ptr EC_POINT -> Ptr EC_GROUP -> IO (Ptr EC_POINT)

foreign import ccall unsafe "EC_POINT_copy"
    ssl_point_copy :: Ptr EC_POINT -> Ptr EC_POINT -> IO CInt

foreign import ccall unsafe "EC_POINT_add"
    ssl_point_add :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr EC_POINT -> Ptr EC_POINT -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_mul"
    ssl_point_mul :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_dbl"
    ssl_point_dbl :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr EC_POINT -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_invert"
    ssl_point_invert :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BN_CTX -> IO CInt

-- 1 is true, 0 false
foreign import ccall unsafe "EC_POINT_is_at_infinity"
    ssl_point_is_at_infinity :: Ptr EC_GROUP -> Ptr EC_POINT -> IO CInt
-- 1 is true, 0 false
foreign import ccall unsafe "EC_POINT_is_on_curve"
    ssl_point_is_on_curve :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BN_CTX -> IO CInt

-- 1 not equal, 0 equal, -1 error
foreign import ccall unsafe "EC_POINT_cmp"
    ssl_point_cmp :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr EC_POINT -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_point2oct"
    ssl_point_2oct :: Ptr EC_GROUP -> Ptr EC_POINT -> PointConversionFormT -> Ptr CUChar -> CSize -> Ptr BN_CTX -> IO CSize

foreign import ccall unsafe "EC_POINT_oct2point"
    ssl_point_oct2 :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr CUChar -> CSize -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_point2bn"
    ssl_point_2bn :: Ptr EC_GROUP -> Ptr EC_POINT -> PointConversionFormT -> Ptr BIGNUM -> Ptr BN_CTX -> IO (Ptr BIGNUM)

foreign import ccall unsafe "EC_POINT_bn2point"
    ssl_point_bn2 :: Ptr EC_GROUP -> Ptr BIGNUM -> Ptr EC_POINT -> Ptr BN_CTX -> IO (Ptr EC_POINT)

foreign import ccall unsafe "EC_POINT_point2hex"
    ssl_point_2hex :: Ptr EC_GROUP -> Ptr EC_POINT -> PointConversionFormT -> Ptr BN_CTX -> IO (Ptr CChar)

foreign import ccall unsafe "EC_POINT_hex2point"
    ssl_point_hex2 :: Ptr EC_GROUP -> Ptr CChar -> Ptr EC_POINT -> Ptr BN_CTX -> IO (Ptr EC_POINT)

foreign import ccall unsafe "EC_POINT_set_to_infinity"
    ssl_point_set_to_infinity :: Ptr EC_GROUP -> Ptr EC_POINT -> IO CInt

foreign import ccall unsafe "EC_POINT_set_Jprojective_coordinates_GFp"
    ssl_point_set_Jprojective_coordinates_GFp :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_get_Jprojective_coordinates_GFp"
    ssl_point_get_Jprojective_coordinates_GFp :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_set_affine_coordinates_GFp"
    ssl_point_set_affine_coordinates_GFp :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_get_affine_coordinates_GFp"
    ssl_point_get_affine_coordinates_GFp :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_set_compressed_coordinates_GFp"
    ssl_point_set_compressed_coordinates_GFp :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> CInt -> Ptr BN_CTX -> IO CInt

#ifdef OPENSSL_NO_EC2M
ssl_point_set_affine_coordinates_GF2m :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt
ssl_point_set_affine_coordinates_GF2m = gf2m_not_supported

ssl_point_get_affine_coordinates_GF2m :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt
ssl_point_get_affine_coordinates_GF2m = gf2m_not_supported

ssl_point_set_compressed_coordinates_GF2m :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> CInt -> Ptr BN_CTX -> IO CInt
ssl_point_set_compressed_coordinates_GF2m = gf2m_not_supported

#else
foreign import ccall unsafe "EC_POINT_set_affine_coordinates_GF2m"
    ssl_point_set_affine_coordinates_GF2m :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_get_affine_coordinates_GF2m"
    ssl_point_get_affine_coordinates_GF2m :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> Ptr BIGNUM -> Ptr BN_CTX -> IO CInt

foreign import ccall unsafe "EC_POINT_set_compressed_coordinates_GF2m"
    ssl_point_set_compressed_coordinates_GF2m :: Ptr EC_GROUP -> Ptr EC_POINT -> Ptr BIGNUM -> CInt -> Ptr BN_CTX -> IO CInt
#endif

-------------------------------
-- EC_KEY related functions

foreign import ccall unsafe "EC_KEY_new"
    ssl_key_new :: IO (Ptr EC_KEY)

foreign import ccall unsafe "&EC_KEY_free"
    ssl_key_free :: FunPtr (Ptr EC_KEY -> IO ())

foreign import ccall unsafe "EC_KEY_get0_group"
    ssl_key_get0_group :: Ptr EC_KEY -> IO (Ptr EC_GROUP)

foreign import ccall unsafe "EC_KEY_set_group"
    ssl_key_set_group :: Ptr EC_KEY -> Ptr EC_GROUP -> IO CInt

foreign import ccall unsafe "EC_KEY_generate_key"
    ssl_key_generate_key :: Ptr EC_KEY -> IO CInt

foreign import ccall unsafe "EC_KEY_get0_private_key"
    ssl_key_get0_private_key :: Ptr EC_KEY -> IO (Ptr BIGNUM)

foreign import ccall unsafe "EC_KEY_get0_public_key"
    ssl_key_get0_public_key :: Ptr EC_KEY -> IO (Ptr EC_POINT)

foreign import ccall unsafe "EC_KEY_set_private_key"
    ssl_key_set_private_key :: Ptr EC_KEY -> Ptr BIGNUM -> IO CInt

foreign import ccall unsafe "EC_KEY_set_public_key"
    ssl_key_set_public_key :: Ptr EC_KEY -> Ptr EC_POINT -> IO CInt
