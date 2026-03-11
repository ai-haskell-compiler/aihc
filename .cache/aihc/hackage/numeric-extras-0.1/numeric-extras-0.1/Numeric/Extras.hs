{-# LANGUAGE ForeignFunctionInterface, FlexibleContexts, TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Numeric.Extras
    ( RealExtras(..)
    ) where

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Arrow ((***))
import Foreign
import Foreign.C.Types
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
import System.IO.Unsafe (unsafeDupablePerformIO)
#else
import System.IO.Unsafe (unsafePerformIO)
#endif

{-# ANN module "HLint: ignore Use camelCase" #-}

class (Storable (C a), RealFloat (C a), RealFloat a) => RealExtras a where
    type C a :: *
    fmod :: a -> a -> a
    expm1 :: a -> a
    log1p :: a -> a
    hypot :: a -> a -> a
    cbrt  :: a -> a
    erf   :: a -> a
    floor :: a -> a
    ceil  :: a -> a
    trunc :: a -> a
    modf  :: a -> (a, a)
    remainder :: a -> a -> a

instance RealExtras Double where
    type C Double = CDouble
    fmod = lift2D c_fmod
    expm1 = lift1D c_expm1
    log1p = lift1D c_log1p
    hypot = lift2D c_hypot
    cbrt = lift1D c_cbrt
    erf = lift1D c_erf
    floor = lift1D c_floor
    ceil  = lift1D c_ceil
    trunc = lift1D c_trunc
    modf = lift1D2 c_modf
    remainder = lift2D c_remainder

lift1D :: (CDouble -> CDouble) -> Double -> Double
lift1D f a = realToFrac (f (realToFrac a))
{-# INLINE lift1D #-}

lift1D2 :: (CDouble -> (CDouble, CDouble)) -> Double -> (Double, Double)
lift1D2 f a = (realToFrac *** realToFrac) (f (realToFrac a))
{-# INLINE lift1D2 #-}

lift2D :: (CDouble -> CDouble -> CDouble) -> Double -> Double -> Double
lift2D f a b = realToFrac (f (realToFrac a) (realToFrac b))
{-# INLINE lift2D #-}

c_modf :: CDouble -> (CDouble, CDouble)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
c_modf a = unsafeDupablePerformIO $ alloca $ \i -> (,) <$> c_modf_imp a i <*> peek i
#else
c_modf a = unsafePerformIO $ alloca $ \i -> (,) <$> c_modf_imp a i <*> peek i
#endif

instance RealExtras Float where
    type C Float = CFloat
    fmod = lift2F c_fmodf
    expm1 = lift1F c_expm1f
    log1p = lift1F c_log1pf
    hypot = lift2F c_hypotf
    cbrt  = lift1F c_cbrtf
    erf   = lift1F c_erff
    floor = lift1F c_floorf
    ceil  = lift1F c_ceilf
    trunc = lift1F c_truncf
    modf = lift1F2 c_modff
    remainder = lift2F c_remainderf

lift1F :: (CFloat -> CFloat) -> Float -> Float
lift1F f a = realToFrac (f (realToFrac a))
{-# INLINE lift1F #-}

lift1F2 :: (CFloat -> (CFloat, CFloat)) -> Float -> (Float, Float)
lift1F2 f a = (realToFrac *** realToFrac) (f (realToFrac a))
{-# INLINE lift1F2 #-}

lift2F :: (CFloat -> CFloat -> CFloat) -> Float -> Float -> Float
lift2F f a b = realToFrac (f (realToFrac a) (realToFrac b))
{-# INLINE lift2F #-}

c_modff :: CFloat -> (CFloat, CFloat)
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
c_modff a = unsafeDupablePerformIO $ alloca $ \i -> (,) <$> c_modff_imp a i <*> peek i
#else
c_modff a = unsafePerformIO $ alloca $ \i -> (,) <$> c_modff_imp a i <*> peek i
#endif

foreign import ccall unsafe "math.h fmod"
    c_fmod :: CDouble -> CDouble -> CDouble
foreign import ccall unsafe "math.h expm1"
    c_expm1 :: CDouble -> CDouble
foreign import ccall unsafe "math.h log1p"
    c_log1p :: CDouble -> CDouble
foreign import ccall unsafe "math.h hypot"
    c_hypot :: CDouble -> CDouble -> CDouble
foreign import ccall unsafe "math.h cbrt"
    c_cbrt :: CDouble -> CDouble
foreign import ccall unsafe "math.h erf"
    c_erf :: CDouble -> CDouble
foreign import ccall unsafe "math.h floor"
    c_floor :: CDouble -> CDouble
foreign import ccall unsafe "math.h ceil"
    c_ceil :: CDouble -> CDouble
foreign import ccall unsafe "math.h trunc"
    c_trunc :: CDouble -> CDouble
foreign import ccall unsafe "math.h modf"
    c_modf_imp :: CDouble -> Ptr CDouble -> IO CDouble
foreign import ccall unsafe "math.h remainder"
    c_remainder :: CDouble -> CDouble -> CDouble

foreign import ccall unsafe "math.h fmodf"
    c_fmodf :: CFloat -> CFloat -> CFloat
foreign import ccall unsafe "math.h expm1f"
    c_expm1f :: CFloat -> CFloat
foreign import ccall unsafe "math.h log1pf"
    c_log1pf :: CFloat -> CFloat
foreign import ccall unsafe "math.h hypotf"
    c_hypotf :: CFloat -> CFloat -> CFloat
foreign import ccall unsafe "math.h cbrtf"
    c_cbrtf :: CFloat -> CFloat
foreign import ccall unsafe "math.h erff"
    c_erff :: CFloat -> CFloat
foreign import ccall unsafe "math.h floorf"
    c_floorf :: CFloat -> CFloat
foreign import ccall unsafe "math.h ceilf"
    c_ceilf :: CFloat -> CFloat
foreign import ccall unsafe "math.h truncf"
    c_truncf :: CFloat -> CFloat
foreign import ccall unsafe "math.h modff"
    c_modff_imp :: CFloat -> Ptr CFloat -> IO CFloat
foreign import ccall unsafe "math.h remainderf"
    c_remainderf :: CFloat -> CFloat -> CFloat

default (Double)
