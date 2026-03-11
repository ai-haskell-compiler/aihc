{-# LANGUAGE MagicHash, CPP #-}
{-|
    Module      :  Data.Number.MPFR.Assignment
    Description :  wrappers for assignment functions
    Copyright   :  (c) Aleš Bizjak, Michal Konečný
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 Conversion from basic Haskell types to MPFR. 
 See <http://www.mpfr.org/mpfr-current/mpfr.html#Assignment-Functions> for
 documentation on particular functions.
-}

module Data.Number.MPFR.Assignment where

import Data.Number.MPFR.Internal

import Data.Number.MPFR.Arithmetic

import GHC.Num.Integer ( integerLogBase )

set     :: RoundMode -> Precision -> MPFR -> MPFR
set r p = fst . set_ r p

set_         :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
set_ r p mp1 = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                 with mp1 $ \p2 -> 
                   mpfr_set p1 p2 ((fromIntegral . fromEnum) r) 

fromWord     :: RoundMode -> Precision -> Word -> MPFR
fromWord r p = fst . fromWord_ r p

fromInt     :: RoundMode -> Precision -> Int -> MPFR
fromInt r p = fst . fromInt_ r p

fromDouble     :: RoundMode -> Precision -> Double -> MPFR
fromDouble r p = fst . fromDouble_ r p

fromWord_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
fromWord_ r p d = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                   mpfr_set_ui p1 (fromIntegral d) ((fromIntegral . fromEnum) r)

fromInt_       :: RoundMode -> Precision -> Int -> (MPFR, Int)
fromInt_ r p d = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                   mpfr_set_si p1 (fromIntegral d) ((fromIntegral . fromEnum) r)
                    
fromDouble_       :: RoundMode -> Precision -> Double -> (MPFR, Int)
fromDouble_ r p d = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                   mpfr_set_d p1 (realToFrac d) ((fromIntegral . fromEnum) r)
                      
-- | x * 2 ^ y
int2w         :: RoundMode -> Precision -> Word -> Int -> MPFR
int2w r p i = fst . int2w_ r p i

-- | x * 2 ^ y
int2i         :: RoundMode -> Precision -> Int -> Int -> MPFR
int2i r p i = fst . int2i_ r p i

int2w_         :: RoundMode -> Precision -> Word -> Int -> (MPFR, Int)
int2w_ r p i e = unsafePerformIO go
    where go = withDummy p $ \p1 -> 
                   mpfr_set_ui_2exp p1 (fromIntegral i) (fromIntegral e) ((fromIntegral . fromEnum) r)
                    
int2i_         :: RoundMode -> Precision -> Int -> Int -> (MPFR, Int)
int2i_ r p i e = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                   mpfr_set_si_2exp p1 (fromIntegral i) (fromIntegral e) ((fromIntegral . fromEnum) r)
                    
stringToMPFR       :: RoundMode -> Precision 
                     -> Word -- ^ Base 
                     -> String -> MPFR
stringToMPFR r p b = fst . stringToMPFR_ r p b

stringToMPFR_         :: RoundMode -> Precision 
                       -> Word -- ^ Base 
                       -> String -> (MPFR, Int)
stringToMPFR_ r p b d = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                   withCString d $ \p2 ->
                       mpfr_set_str p1 p2 (fromIntegral b) ((fromIntegral . fromEnum) r) 

strtofr         :: RoundMode -> Precision
                -> Word -- ^ base
                -> String -> (MPFR, String)
strtofr r p b d = case strtofr_ r p b d of
                    (a, b', _) -> (a,b')

strtofr_         :: RoundMode -> Precision
                   -> Word -- ^ base
                   -> String -> (MPFR, String, Int)
strtofr_ r p b d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do 
                    pokeDummy p1 fp p
                    withCString d $ \p2 ->
                      alloca $ \p3 -> do
                        r3 <- mpfr_strtofr p1 p2 p3 (fromIntegral b) ((fromIntegral . fromEnum) r)
                        p3' <- peek p3
                        r2 <- peekCString p3'
                        r1 <- peekP p1 fp
                        return (r1, r2, fromIntegral r3)
                        
                                                                
setInf     :: Precision -> Int -> MPFR
setInf p i = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do 
                    pokeDummy p1 fp p
                    mpfr_set_inf p1 (fromIntegral  i)
                    peekP p1 fp

setNaN   :: Precision -> MPFR
setNaN p = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do 
                    pokeDummy p1 fp p
                    mpfr_set_nan p1
                    peekP p1 fp

fromIntegerA     :: RoundMode -> Precision -> Integer -> MPFR
fromIntegerA r p = stringToMPFR r p 10 . show 

bitsInInteger :: (Num a) => Integer -> a

bitsInInteger = fromIntegral . integerLogBase 2

compose             :: RoundMode -> Precision -> (Integer, Int) -> MPFR 
compose r p (i, ii) = div2i r p (fromIntegerA r p i) ii

-- | 'stringToMPFR' with default rounding to Near.
fromString       :: String -> Precision -> Word -> MPFR
fromString s p b = stringToMPFR Near p b s
