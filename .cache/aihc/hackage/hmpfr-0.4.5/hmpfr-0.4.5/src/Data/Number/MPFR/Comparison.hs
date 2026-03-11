{-|
    Module      :  Data.Number.MPFR.Comparison
    Description :  Comparison functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

  All the functions that return Maybe Ordering return Nothing
  when one of the operands is NaN and  Just _ otherwise.
  
  For documentation on particular functions see
  <http://www.mpfr.org/mpfr-current/mpfr.html#Comparison-Functions>
-}

module Data.Number.MPFR.Comparison where

import Data.Number.MPFR.Internal

import Prelude hiding (isNaN, exponent, isInfinite)

import Data.Maybe

{-# INLINE cmp #-}
cmp :: MPFR -> MPFR -> Maybe Ordering
cmp mp1@(MP _ s e _) mp2@(MP _ s' e' _) 
    | e > expInf && e' > expInf = 
        case (s /= s', e /= e') of
          (True, _) -> Just $ compare (signum s) (signum s')
          (_, True) -> Just $ compare (fromIntegral s * e) (fromIntegral s * e')
          (False, False) -> Just (compare (withMPFRBB mp1 mp2 mpfr_cmp) 0)
    | isNaN mp1 || isNaN mp2 = Nothing 
    | isZero mp1             = Just $ if isZero mp2 
                                      then EQ 
                                      else toEnum . (+ 1) . negate . fromIntegral . signum $ s'
    | isZero mp2             = Just . toEnum . (+ 1) . fromIntegral $ signum s
    | isInfinite mp1         = Just .compare s $ if isInfinite mp2 then s' else 0 
    | isInfinite mp2         = Just $ compare 0 s'
    | otherwise              = Nothing

cmpw       :: MPFR -> Word -> Maybe Ordering
cmpw mp1 w = if isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_ui p (fromIntegral w) 

cmpi       :: MPFR -> Int -> Maybe Ordering
cmpi mp1 i = if isNaN mp1 then Nothing else Just (compare (unsafePerformIO go) 0)
    where go = with mp1 $ \p -> mpfr_cmp_si p (fromIntegral i)

cmpd       :: MPFR -> Double -> Maybe Ordering
cmpd mp1 d = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with mp1 $ \p -> do
                    r1 <- mpfr_cmp_d p (realToFrac d)
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing
                    
cmp2w       :: MPFR -> Word -> Exp -> Maybe Ordering
cmp2w d w e = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with d $ \p -> do
                    r1 <- mpfr_cmp_ui_2exp p (fromIntegral w) e
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing

cmp2i       :: MPFR -> Int -> Exp -> Maybe Ordering
cmp2i d w e = unsafePerformIO go
    where go = do mpfr_clear_erangeflag
                  with d $ \p -> do
                    r1 <- mpfr_cmp_si_2exp p (fromIntegral w) e
                    r2 <- mpfr_erangeflag_p
                    if r2 == 0 then return (Just (compare r1 0))
                      else do mpfr_clear_erangeflag
                              return Nothing

cmpabs         :: MPFR -> MPFR -> Maybe Ordering
cmpabs mp1 mp2 = if isNaN mp1 || isNaN mp2 then Nothing 
                   else Just (compare (withMPFRBB mp1 mp2 mpfr_cmpabs) 0)

{-# INLINE isNaN #-}
isNaN   :: MPFR -> Bool
isNaN (MP _ _ e _) = e == expNaN -- withMPFRB d mpfr_nan_p /= 0

{-# INLINE isInfinite #-}
isInfinite   :: MPFR -> Bool
isInfinite (MP _ _ e _) = e == expInf -- withMPFRB d mpfr_inf_p /= 0 

isNumber   :: MPFR -> Bool
isNumber d = withMPFRB d mpfr_number_p /= 0 

{-# INLINE isZero #-}
isZero   :: MPFR -> Bool
isZero (MP _ _ e _) = e == expZero --withMPFRB d mpfr_zero_p /= 0

{-# INLINE sgn #-}
sgn                               :: MPFR -> Maybe Int 
sgn mp1@(MP _ s _ _) | isZero mp1 = Just 0
                     | isNaN mp1  = Nothing
                     | otherwise  = Just $ fromIntegral $ signum s
{-
sgn     :: MPFR -> Maybe Int 
sgn mp1 = case (cmpw mp1 0) of
            Nothing -> Nothing
            Just x -> Just (pred . fromEnum $ x)
-}
-- TODO Maybe Bool????
greater    :: MPFR -> MPFR -> Bool
greater d1 = maybe False (== GT) . cmp d1 --withMPFRBB d1 d2 mpfr_greater_p /= 0

greatereq    :: MPFR -> MPFR -> Bool
greatereq d1 = maybe False (/= LT) . cmp d1 --withMPFRBB d1 d2 mpfr_greaterequal_p /= 0

less    :: MPFR -> MPFR -> Bool
less d1 = maybe False (== LT) . cmp d1 --withMPFRBB d1 d2 mpfr_less_p /= 0

lesseq    :: MPFR -> MPFR -> Bool
lesseq d1 = maybe False (/= GT) . cmp d1  --withMPFRBB d1 d2 mpfr_lessequal_p /= 0

lessgreater       :: MPFR -> MPFR -> Maybe Bool
lessgreater d1 d2 = if isNaN d1 || isNaN d2 then Nothing 
                      else Just (withMPFRBB d1 d2 mpfr_lessgreater_p /= 0)

equal    :: MPFR -> MPFR -> Bool
equal d1 = maybe False (== EQ) . cmp d1 --withMPFRBB d1 d2 mpfr_equal_p /= 0

unordered       :: MPFR -> MPFR -> Maybe Bool
unordered d1 d2 = if isNaN d1 || isNaN d2 then Nothing 
                    else Just (withMPFRBB d1 d2 mpfr_unordered_p /= 0)


instance Eq MPFR where
    (==) = equal

instance Ord MPFR where
    compare d = fromMaybe GT . cmp d
    (<)       = less
    (<=)      = lesseq
    (>)       = greater
    (>=)      = greatereq
--    max d d'     = maxD Zero (maxPrec d d') d d'
--    min d d'     = minD Zero (maxPrec d d') d d'
                    