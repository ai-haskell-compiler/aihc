{-# LANGUAGE CPP #-}
module Data.Number.MPFR.Mutable.Internal (
       module Data.Number.MPFR.FFIhelper,
       withMutableMPFRBA,
       withMutableMPFR1,withMutableMPFR2,
       withMutableMPFRS, withMutableMPFRUI,
       withMutableMPFRd, withMutableMPFRd',
       withMutableMPFRBA3, withMutableMPFRSC,
       withMutableMPFRSNR, withMutableMPFRSNRNR,
       withMutableMPFRP, withMutableMPFRB,
       unsafeWriteMMPFR, unsafeFreeze, unsafeThaw,
       writeMMPFR, freeze, thaw,
       MMPFR
)
where

import Data.Number.MPFR.FFIhelper

import Foreign.C(CInt, CDouble)
import Foreign(Ptr, with)

import Data.STRef(STRef, readSTRef, writeSTRef, newSTRef)

#if (__GLASGOW_HASKELL__ >= 702)
import Control.Monad.ST(ST)
import Control.Monad.ST.Unsafe(unsafeIOToST)
#else
import Control.Monad.ST(ST,unsafeIOToST)
#endif
import Foreign.Marshal.Utils(copyBytes)

import Foreign.ForeignPtr(withForeignPtr, mallocForeignPtrBytes)

-- | A mutable MPFR. Currently this is just a newtype wrapped STRef to
-- a MPFR but this may change in the future for a more efficient implementation.
-- Type argument @s@ is the state variable argument for the @ST@ type.
newtype MMPFR s = MMPFR { run :: STRef s MPFR } deriving Eq

{-# INLINE unsafeWriteMMPFR #-}
-- | Replace the state of the mutable MPFR with a new one. The actual limbs are
-- not copied, so any further modifications on the mutable MPFR will reflect on
-- the MPFR given in as the second argument.
unsafeWriteMMPFR :: MMPFR s -> MPFR -> ST s ()
unsafeWriteMMPFR (MMPFR m1) m2 = writeSTRef m1 m2

{-# INLINE unsafeFreeze #-}
-- | Convert a mutable MPFR to an immutable one. The unsafe prefix comes from
-- the fact that limbs of the MPFR are not copied so any further modifications
-- on the mutable MPFR will reflect on the \"frozen\" one. If mutable MPFR will
-- not be modified afterwards, it is perfectly safe to use.
unsafeFreeze :: MMPFR s -> ST s MPFR
unsafeFreeze (MMPFR m) = readSTRef m

{-# INLINE unsafeThaw #-}
-- | Convert an immutable MPFR to a mutable one. The unsafe prefix comes from
-- the fact that limbs of the MPFR are not copied so any modifications done on
-- on the mutable MPFR will reflect on the original. If the original will not be
-- used or limbs of the mutable not modified, then it is safe to use.
unsafeThaw :: MPFR -> ST s (MMPFR s)
unsafeThaw m = newSTRef m >>= return . MMPFR 

{-# INLINE writeMMPFR #-}
-- | Replace the state of the mutable MPFR with a new one,
-- making a complete copy.
writeMMPFR :: MMPFR s -> MPFR -> ST s ()
writeMMPFR (MMPFR m1) (MP p s e fp)  =
    do fp' <- unsafeIOToST $ do p' <- mpfr_custom_get_size p
                                fp' <- mallocForeignPtrBytes (fromIntegral p')
                                withForeignPtr fp' $ \p1 ->
                                    withForeignPtr fp $ \p2 ->
                                        copyBytes p1 p2 (fromIntegral p')
                                return fp'
       writeSTRef m1 (MP p s e fp')

{-# INLINE freeze #-}
-- | Convert a mutable MPFR to an immutable one, making a complete copy.
freeze :: MMPFR s -> ST s MPFR
freeze m = 
    do (MP p s e fp) <- unsafeFreeze m
       fp' <- unsafeIOToST $ do p' <- mpfr_custom_get_size p
                                fp' <- mallocForeignPtrBytes (fromIntegral p')
                                withForeignPtr fp' $ \p1 ->
                                    withForeignPtr fp $ \p2 ->
                                        copyBytes p1 p2 (fromIntegral p')
                                return fp'
       return (MP p s e fp')

{-# INLINE thaw #-}
-- | Convert an immutable MPFR to a mutable one, making a complete copy.
thaw :: MPFR -> ST s (MMPFR s)
thaw (MP p s e fp) = 
    do fp' <- unsafeIOToST $ do p' <- mpfr_custom_get_size p
                                fp' <- mallocForeignPtrBytes (fromIntegral p')
                                withForeignPtr fp' $ \p1 ->
                                    withForeignPtr fp $ \p2 ->
                                        copyBytes p1 p2 (fromIntegral p')
                                return fp'
       unsafeThaw (MP p s e fp')


{-# INLINE withMutableMPFRBA #-}
withMutableMPFRBA :: (Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
withMutableMPFRBA f m1 m2 m3 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2' <- unsafeFreeze m2
  m3' <- unsafeFreeze m3
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
      with m3' $ \p3 -> 
          with m2' $ \p2 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 p2 p3 (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1
  
{-# INLINE withMutableMPFR1 #-}
withMutableMPFR1 :: (Integral a, Integral b) => 
                    (Ptr MPFR -> Ptr MPFR -> a -> CRoundMode -> IO CInt)
                 ->  MMPFR s -> MMPFR s -> b -> RoundMode -> ST s Int
withMutableMPFR1 f m1 m2 m3 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2' <- unsafeFreeze m2
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
          with m2' $ \p2 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 p2 (fromIntegral m3) (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1
  
{-# INLINE withMutableMPFR2 #-}
withMutableMPFR2 :: (Integral a, Integral b) => 
                     (Ptr MPFR -> a -> Ptr MPFR -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> b -> MMPFR s -> RoundMode -> ST s Int
withMutableMPFR2 f m1 m2 m3 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m3' <- readSTRef .run $ m3
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
          with m3' $ \p3 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 (fromIntegral m2) p3 (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1

{-# INLINE withMutableMPFRd #-}
withMutableMPFRd :: (Ptr MPFR -> Ptr MPFR -> CDouble -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> MMPFR s -> Double -> RoundMode -> ST s Int
withMutableMPFRd f m1 m2 m3 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2' <- unsafeFreeze m2
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
          with m2' $ \p2 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 p2 (realToFrac m3) (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1

{-# INLINE withMutableMPFRd' #-}
withMutableMPFRd' :: (Ptr MPFR -> CDouble -> Ptr MPFR -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> Double -> MMPFR s -> RoundMode -> ST s Int
withMutableMPFRd' f m1 m2 m3 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m3' <- readSTRef .run $ m3
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
          with m3' $ \p3 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 (realToFrac m2) p3 (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1

{-# INLINE withMutableMPFRS #-}
withMutableMPFRS :: (Ptr MPFR ->  Ptr MPFR -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
withMutableMPFRS f m1 m2 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2' <- readSTRef .run $ m2
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
          with m2' $ \p2 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 p2 (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1

{-# INLINE withMutableMPFRSNR #-}
-- mainly for use with rounding functions
withMutableMPFRSNR :: (Ptr MPFR ->  Ptr MPFR -> IO CInt)
                  ->  MMPFR s -> MMPFR s -> ST s Int
withMutableMPFRSNR f m1 m2 = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2' <- readSTRef .run $ m2
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
          with m2' $ \p2 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 p2
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1


{-# INLINE withMutableMPFRP #-}
withMutableMPFRP      :: (Ptr MPFR -> IO CInt) 
                      -> MMPFR s -> ST s Int
withMutableMPFRP f m1 = do 
  m1' <- unsafeFreeze m1
  unsafeIOToST $ with m1' f >>= return . fromIntegral

{-# INLINE withMutableMPFRB #-}
withMutableMPFRB      :: (Ptr MPFR -> Ptr MPFR -> IO CInt) 
                      -> MMPFR s -> MMPFR s -> ST s Int
withMutableMPFRB f m1 m2 = do 
  m1' <- unsafeFreeze m1
  m2' <- unsafeFreeze m2
  unsafeIOToST $ 
               with m1' $ \p1 ->
                   with m2' $ \p2 -> 
                       f p1 p2 >>= return . fromIntegral


{-# INLINE withMutableMPFRUI #-}
withMutableMPFRUI :: (Integral a, Integral b) => 
                     (Ptr MPFR -> a -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> b -> RoundMode -> ST s Int
withMutableMPFRUI f m1 m2 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
      with m1' $ \p1 -> do 
        r1 <- f p1 (fromIntegral m2) (fromIntegral . fromEnum $ r)
        r2 <- peekNoLimbPrec p1
        return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1

{-# INLINE withMutableMPFRBA3 #-}
withMutableMPFRBA3 :: (Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> 
                      CRoundMode -> IO CInt)
                  ->  MMPFR s -> MMPFR s -> MMPFR s -> MMPFR s
                  -> RoundMode -> ST s Int
withMutableMPFRBA3 f m1 m2 m3 m4 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2' <- unsafeFreeze m2
  m3' <- unsafeFreeze m3
  m4' <- unsafeFreeze m4
  (r1,(r21, r22)) <- 
    unsafeIOToST $ do
      with m4' $ \p4 -> 
          with m3' $ \p3 -> 
              with m2' $ \p2 -> 
                  with m1' $ \p1 -> do 
                     r1 <- f p1 p2 p3 p4 (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     return (fromIntegral r1, r2)
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  return r1

{-# INLINE withMutableMPFRSC #-}  
withMutableMPFRSC :: (Ptr MPFR -> Ptr MPFR -> Ptr MPFR -> CRoundMode -> IO CInt)
                  ->  MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
withMutableMPFRSC f m1 m2 m3 r = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  m2'@(MP p' _ _ fp') <- unsafeFreeze m2
  m3' <- unsafeFreeze m3
  (r1,(r21, r22), (r21', r22')) <- 
    unsafeIOToST $ do
      with m3' $ \p3 -> 
          with m2' $ \p2 -> 
              with m1' $ \p1 -> do 
                     r1 <- f p1 p2 p3 (fromIntegral . fromEnum $ r)
                     r2 <- peekNoLimbPrec p1
                     r2' <- peekNoLimbPrec p2
                     return (fromIntegral r1, r2, r2')
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
  unsafeWriteMMPFR m2 (MP p' r21' r22' fp')
  return r1

{-# INLINE withMutableMPFRSNRNR #-}
-- mainly for next* functions
withMutableMPFRSNRNR :: (Ptr MPFR ->  IO ())
                  ->  MMPFR s -> ST s ()
withMutableMPFRSNRNR f m1 = do
  m1'@(MP p _ _ fp) <- unsafeFreeze m1
  (r21, r22) <- unsafeIOToST $ do
    with m1' $ \p1 -> do 
                   f p1
                   r2 <- peekNoLimbPrec p1
                   return r2
  unsafeWriteMMPFR m1 (MP p r21 r22 fp)
