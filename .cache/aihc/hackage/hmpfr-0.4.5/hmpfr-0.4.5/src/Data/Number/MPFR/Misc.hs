{-|
    Module      :  Data.Number.MPFR.Misc
    Description :  Miscellaneous functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-current/mpfr.html#Miscellaneous-Functions>.
-}

module Data.Number.MPFR.Misc where

import Data.Number.MPFR.Internal

import Data.Number.MPFR.Assignment

import Data.Number.MPFR.Comparison

import Data.List(foldl')

nextToward         :: MPFR -> MPFR -> MPFR
nextToward mp1 mp2 = unsafePerformIO go
    where go = do let p = getPrec mp1
                  ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do
                      pokeDummy p1 fp p
                      with mp1 $ \p2 ->
                        with mp2 $ \p3 -> do
                          _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near) 
                          mpfr_nexttoward p1 p3 
                          peekP p1 fp


nextAbove     :: MPFR -> MPFR
nextAbove mp1 = unsafePerformIO go
    where go = do let p = getPrec mp1
                  ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do
                      pokeDummy p1 fp p
                      with mp1 $ \p2 -> do 
                        _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near) 
                        mpfr_nextabove p1 
                        peekP p1 fp

nextBelow     :: MPFR -> MPFR
nextBelow mp1 = unsafePerformIO go
    where go = do let p = getPrec mp1
                  ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do
                      pokeDummy p1 fp p
                      with mp1 $ \p2 -> do 
                        _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near) 
                        mpfr_nextbelow p1 
                        peekP p1 fp

maxD        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
maxD r p d1 = fst . maxD_ r p d1

minD        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
minD r p d1 = fst . minD_ r p d1

newRandomStatePointer :: Ptr GmpRandState
newRandomStatePointer =
    unsafePerformIO new_gmp_randstate

urandomb :: Ptr GmpRandState -> Precision -> MPFR
urandomb randStateP p =
    fst $
    unsafePerformIO $
    do
    withDummy p $ \dP ->
        do
        res <- mpfr_urandomb_deref_randstate dP randStateP
        return $ fromIntegral res


getExp              :: MPFR -> Exp
getExp (MP _ _ e _) = e 

setExp     :: MPFR -> Exp -> MPFR
setExp d e = unsafePerformIO go
    where go = do let p = getPrec d
                  ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do
                    pokeDummy p1 fp p
                    with d $ \p2 -> do 
                      _ <- mpfr_set p1 p2 ((fromIntegral . fromEnum) Near)
                      _ <- mpfr_set_exp p1 e
                      peekP p1 fp

signbit   :: MPFR -> Bool
signbit d = withMPFRB d mpfr_signbit /= 0

maxD_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
maxD_ r pw d1@(MP p _ e _) d2@(MP p' _ e' _) | fromIntegral pw == p && fromIntegral pw == p' && e > expInf && e' > expInf = 
                                                 case cmp d1 d2 of 
                                                   Just LT -> (d2, 0)
                                                   _ -> (d1, 0)
                                             | otherwise           = withMPFRsBA r pw d1 d2 mpfr_max


minD_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
minD_ r pw d1@(MP p _ e _) d2@(MP p' _ e' _) | fromIntegral pw == p && fromIntegral pw == p' && e > expInf && e' > expInf = 
                                                 case cmp d1 d2 of 
                                                   Just GT -> (d2, 0)
                                                   _ -> (d1, 0)
                                             | otherwise = withMPFRsBA r pw d1 d2 mpfr_min

getPrec   :: MPFR -> Precision
getPrec (MP p _ _ _) = fromIntegral p

-- | getMantissa and getExp return values such that
--
-- > d = getMantissa d * 2^(getExp d - ceiling ((getPrec d) / bitsPerMPLimb)* bitsPerMPLimb )
--
-- In case of @0@, @NaN@ or @+-Inf@ getMantissa will return @0@
getMantissa   :: MPFR -> Integer
getMantissa d@(MP _ s e _) | e /= expInf && e /= expNaN && e /= expZero = toInteger s * h
                           | otherwise                                  = 0
    where (h, _) = foldl' (\(a,b) c ->
                               (a + toInteger c `shiftL` b, b + bitsPerMPLimb))
                   (0,0) (getMantissa' d) 

one :: MPFR
one = fromWord Near minPrec 1

zero :: MPFR
zero = fromWord Near minPrec 0

maxPrec      :: MPFR -> MPFR -> Precision
maxPrec d d' = max (getPrec d) (getPrec d')
