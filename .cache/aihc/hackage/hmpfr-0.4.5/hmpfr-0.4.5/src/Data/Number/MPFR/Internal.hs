{-# LANGUAGE BangPatterns #-}

module Data.Number.MPFR.Internal (
       module Data.Number.MPFR.FFIhelper,
       withMPFRsBA, withMPFRBAui, withMPFRBAiu, withMPFRBAd,
       withMPFRBAsi, withMPFRBAis,  withMPFRBAd',
       withMPFRB, withMPFRP, withMPFR, withMPFRBB, withMPFRC,
       withMPFRF, withMPFRUI, withMPFRR, checkPrec, getMantissa',
       unsafePerformIO, peek, Ptr, nullPtr, mallocForeignPtrBytes, with,
       withForeignPtr, CInt, CLong, CULong, withCString, peekCString, alloca,
       peekArray, shiftL, Word, minPrec
)
where

import Data.Number.MPFR.FFIhelper

import Foreign.C(CInt, CLong, CULong, CDouble, withCString, peekCString)
import Foreign.Marshal(alloca, peekArray)
import Foreign(peek, Ptr, nullPtr, mallocForeignPtrBytes, with, withForeignPtr)
import Foreign.Storable(sizeOf)
import System.IO.Unsafe(unsafePerformIO)

import Data.Bits(shiftL)

-- import Data.Word(Word)

-- these are helper functions, only for internal use
{-# INLINE withMPFRsBA #-}
withMPFRsBA               :: RoundMode -> Precision -> MPFR -> MPFR
                               -> (Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt)
                               -> (MPFR, Int)
withMPFRsBA r p !mp1 !mp2 f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               with mp1 $ \p2 ->
               with mp2 $ \p3 ->
               f p1 p2 p3 ((fromIntegral . fromEnum) r)


{-# INLINE withMPFRBAui #-}
withMPFRBAui :: RoundMode -> Precision -> MPFR -> CULong
                  ->  (Ptr MPFR -> Ptr MPFR -> CULong -> CRoundMode -> IO CInt)
                  -> (MPFR, Int)
withMPFRBAui r p !mp1 d f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               with mp1 $ \p2 ->
               f p1 p2 d ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRBAsi #-}
withMPFRBAsi             :: RoundMode -> Precision -> MPFR -> CLong
                              -> (Ptr MPFR -> Ptr MPFR -> CLong -> CRoundMode -> IO CInt)
                              -> (MPFR, Int)
withMPFRBAsi r p !mp1 d f = unsafePerformIO go
    where go = withDummy p $ \ p1 ->
               with mp1 $ \ p2 ->
               f p1 p2 d ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRBAiu #-}
withMPFRBAiu             :: RoundMode -> Precision -> CULong -> MPFR
                              -> (Ptr MPFR -> CULong -> Ptr MPFR -> CRoundMode -> IO CInt)
                              -> (MPFR, Int)
withMPFRBAiu r p d !mp1 f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               with mp1 $ \p2 ->
               f p1 d p2 ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRBAis #-}
withMPFRBAis             :: RoundMode -> Precision -> CLong -> MPFR
                              -> (Ptr MPFR -> CLong -> Ptr MPFR -> CRoundMode -> IO CInt)
                              -> (MPFR, Int)
withMPFRBAis r p d !mp1 f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               with mp1 $ \p2 ->
               f p1 d p2 ((fromIntegral . fromEnum) r)
{-# INLINE withMPFRBAd #-}
withMPFRBAd              :: RoundMode -> Precision -> MPFR -> CDouble
                               -> (Ptr MPFR -> Ptr MPFR -> CDouble -> CRoundMode -> IO CInt)
                               -> (MPFR, Int)
withMPFRBAd r p !mp1 d f = unsafePerformIO go
    where go = withDummy p $ \ p1 ->
               with mp1 $ \ p2 ->
               f p1 p2 d ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRBAd' #-}
withMPFRBAd'              :: RoundMode -> Precision -> CDouble -> MPFR
                               -> (Ptr MPFR -> CDouble -> Ptr MPFR -> CRoundMode -> IO CInt)
                               -> (MPFR, Int)
withMPFRBAd' r p d !mp1 f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               with mp1 $ \p2 ->
               f p1 d p2 ((fromIntegral . fromEnum) r)


{-# INLINE withMPFRB #-}
withMPFRB       :: MPFR -> (Ptr MPFR -> IO CInt) -> CInt
withMPFRB !mp1 f = unsafePerformIO go
    where go = with mp1 f

withMPFRP       :: MPFR -> (Ptr MPFR -> IO CPrecision) -> CPrecision
withMPFRP !mp1 f = unsafePerformIO go
    where go = with mp1 f

{-# INLINE withMPFR #-}
withMPFR           :: RoundMode -> Precision -> MPFR
                        -> (Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt)
                        -> (MPFR, Int)
withMPFR r p !mp1 f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               with mp1 $ \p2 ->
               f p1 p2 ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRBB #-}
withMPFRBB           :: MPFR -> MPFR
                          -> (Ptr MPFR -> Ptr MPFR -> IO CInt)
                          -> CInt
withMPFRBB !mp1 !mp2 f = unsafePerformIO go
    where go = with mp1 $ with mp2 . f

{-# INLINE withMPFRC #-}
withMPFRC       :: RoundMode -> Precision ->
                     (Ptr MPFR -> CRoundMode -> IO CInt) -> (MPFR, Int)
withMPFRC r p f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               f p1 ((fromIntegral . fromEnum) r)

withMPFRF         :: MPFR -> RoundMode
                       -> (Ptr MPFR -> CRoundMode -> IO CInt)
                       -> Int
withMPFRF !mp1 r f = (fromIntegral . unsafePerformIO) go
    where go = with mp1 $ \p1 -> f p1 ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRUI #-}
withMPFRUI         :: RoundMode -> Precision -> Word
                        -> (Ptr MPFR -> CULong -> CRoundMode -> IO CInt)
                        -> (MPFR, Int)
withMPFRUI r p d f = unsafePerformIO go
    where go = withDummy p $ \p1 ->
               f p1 (fromIntegral d) ((fromIntegral . fromEnum) r)

{-# INLINE withMPFRR #-}
withMPFRR       :: Precision -> MPFR
                     -> (Ptr MPFR -> Ptr MPFR -> IO CInt)
                     -> (MPFR, Int)
withMPFRR p !d f = unsafePerformIO go
    where go = withDummy p $ with d . f


{-# INLINE checkPrec #-}
checkPrec :: Precision -> Precision
checkPrec = max minPrec

getMantissa'     :: MPFR -> [Limb]
getMantissa' (MP p _ _ p1) = unsafePerformIO go
    where go = withForeignPtr p1 $
               peekArray (Prelude.ceiling ((fromIntegral p ::Double) / fromIntegral bitsPerMPLimb))

minPrec :: Precision
minPrec = fromIntegral $ 8 * sizeOf (undefined :: Int)
