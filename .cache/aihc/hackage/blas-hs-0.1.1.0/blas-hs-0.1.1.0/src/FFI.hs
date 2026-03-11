{-# OPTIONS_HADDOCK hide #-}
module FFI where
import Foreign (Ptr, Storable(peek), castPtr, with, alloca)
import Foreign.C (CFloat(CFloat), CDouble(CDouble))

-- | Convert to a C enum value.
toCEnum :: (Enum a, Num b) => a -> b
toCEnum = fromIntegral . fromEnum

-- | Convert from a C enum value.
fromCEnum :: (Integral a, Enum b) => a -> b
fromCEnum = toEnum . fromIntegral

-- | Convert by unwrapping a @newtype@.
--   Safe, though possibly unportable.
fromCFloat :: CFloat -> Float
fromCFloat (CFloat x) = x

-- | Convert by unwrapping a @newtype@.
--   Safe, though possibly unportable.
fromCDouble :: CDouble -> Double
fromCDouble (CDouble x) = x

-- | Same as 'with' but accepts a @void@-pointer instead.
--   Warning: make sure the type is compatible.
withVoidPtr :: Storable a => a -> (Ptr () -> IO c) -> IO c
withVoidPtr x f = with x (f . castPtr)

-- | Extract the return value via a pointer argument.
--   Warning: make sure the only a one element is written.
getReturnValue :: Storable a => (Ptr a -> IO ()) -> IO a
getReturnValue f = alloca $ \ x -> f x >> peek x
