{-|
    Module      :  Data.Number.MPFR.Arithmetic
    Description :  Basic arithmetic functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-current/mpfr.html#Basic-Arithmetic-Functions>.
-}

module Data.Number.MPFR.Arithmetic 
    where

import Data.Number.MPFR.Internal

import Prelude hiding(isNaN)

add        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
add r p d1 = fst . add_ r p d1
      
addw        :: RoundMode -> Precision -> MPFR -> Word -> MPFR
addw r p d1 = fst . addw_ r p d1

addi        :: RoundMode -> Precision -> MPFR -> Int -> MPFR
addi r p d1 = fst . addi_ r p d1

addd        :: RoundMode -> Precision -> MPFR -> Double -> MPFR
addd r p d1 = fst . addd_ r p d1

sub        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
sub r p d1 = fst . sub_ r p d1
      
subw        :: RoundMode -> Precision -> MPFR -> Word -> MPFR
subw r p d1 = fst . subw_ r p d1
      
subi        :: RoundMode -> Precision -> MPFR -> Int -> MPFR
subi r p d1 = fst . subi_ r p d1

subd        :: RoundMode -> Precision -> MPFR -> Double -> MPFR
subd r p d1 = fst . subd_ r p d1
      
wsub       :: RoundMode -> Precision -> Word -> MPFR -> MPFR
wsub r p d = fst . wsub_ r p d
      
isub       :: RoundMode -> Precision -> Int -> MPFR -> MPFR
isub r p d = fst . isub_ r p d

dsub       :: RoundMode -> Precision -> Double -> MPFR -> MPFR
dsub r p d = fst . dsub_ r p d
      
mul        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
mul r p d1 = fst . mul_ r p d1

mulw        :: RoundMode -> Precision -> MPFR -> Word -> MPFR
mulw r p d1 = fst . mulw_ r p d1
      
muli        :: RoundMode -> Precision -> MPFR -> Int -> MPFR
muli r p d1 = fst . muli_ r p d1

muld        :: RoundMode -> Precision -> MPFR -> Double -> MPFR
muld r p d1 = fst . muld_ r p d1
      
sqr     :: RoundMode -> Precision -> MPFR -> MPFR 
sqr r p = fst . sqr_ r p
      
div        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
div r p d1 = fst . div_ r p d1
      
divw        :: RoundMode -> Precision -> MPFR -> Word -> MPFR
divw r p d1 = fst . divw_ r p d1
      
divi        :: RoundMode -> Precision -> MPFR -> Int -> MPFR
divi r p d1 = fst . divi_ r p d1

divd        :: RoundMode -> Precision -> MPFR -> Double -> MPFR
divd r p d1 = fst . divd_ r p d1
      
wdiv       :: RoundMode -> Precision -> Word -> MPFR -> MPFR
wdiv r p d = fst . wdiv_ r p d
      
idiv       :: RoundMode -> Precision -> Int -> MPFR -> MPFR
idiv r p d = fst . idiv_ r p d

ddiv       :: RoundMode -> Precision -> Double -> MPFR -> MPFR
ddiv r p d = fst . ddiv_ r p d
      
sqrt     :: RoundMode -> Precision -> MPFR -> MPFR
sqrt r p = fst . sqrt_ r p
      
sqrtw     :: RoundMode -> Precision -> Word -> MPFR
sqrtw r p = fst . sqrtw_ r p

recSqrt     :: RoundMode -> Precision -> MPFR -> MPFR
recSqrt r p = fst . recSqrt_ r p
      
cbrt     :: RoundMode -> Precision -> MPFR -> MPFR
cbrt r p = fst . cbrt_ r p
      
root       :: RoundMode -> Precision -> MPFR -> Word -> MPFR
root r p d = fst . root_ r p d
      
pow        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
pow r p d1 = fst . pow_ r p d1
      
poww        :: RoundMode -> Precision -> MPFR -> Word -> MPFR 
poww r p d1 = fst . poww_ r p d1
      
powi        :: RoundMode -> Precision -> MPFR -> Int -> MPFR 
powi r p d1 = fst . powi_ r p d1
      
wpoww       :: RoundMode -> Precision -> Word -> Word -> MPFR 
wpoww r p d = fst . wpoww_ r p d
      
wpow        :: RoundMode -> Precision -> Word -> MPFR -> MPFR 
wpow r p d1 = fst . wpow_ r p d1
      
neg     :: RoundMode -> Precision -> MPFR -> MPFR
neg r p = fst . neg_ r p
      
absD     :: RoundMode -> Precision -> MPFR -> MPFR 
absD r p = fst . absD_ r p
      
dim        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
dim r p d1 = fst . dim_ r p d1 
      
mul2w        :: RoundMode -> Precision -> MPFR -> Word -> MPFR
mul2w r p d1 = fst . mul2w_ r p d1
      
mul2i        :: RoundMode -> Precision -> MPFR -> Int -> MPFR
mul2i r p d1 = fst . mul2i_ r p d1
      
div2w        :: RoundMode -> Precision -> MPFR -> Word -> MPFR
div2w r p d1 = fst . div2w_ r p d1
      
div2i        :: RoundMode -> Precision -> MPFR -> Int -> MPFR
div2i r p d1 = fst . div2i_ r p d1
      
add_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
add_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_add
      
addw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
addw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_add_ui

addi_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
addi_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_add_si

addd_          :: RoundMode -> Precision -> MPFR -> Double -> (MPFR, Int)
addd_ r p d1 d = withMPFRBAd r p d1 (realToFrac d) mpfr_add_d
      
sub_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
sub_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_sub
      
subw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
subw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_sub_ui
      
subi_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
subi_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_sub_si

subd_          :: RoundMode -> Precision -> MPFR -> Double -> (MPFR, Int)
subd_ r p d1 d = withMPFRBAd r p d1 (realToFrac d) mpfr_sub_d
      
wsub_          :: RoundMode -> Precision -> Word -> MPFR -> (MPFR, Int)
wsub_ r p d d1 = withMPFRBAiu r p (fromIntegral d) d1 mpfr_ui_sub
      
isub_          :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
isub_ r p d d1 = withMPFRBAis r p (fromIntegral d) d1 mpfr_si_sub

dsub_          :: RoundMode -> Precision -> Double -> MPFR -> (MPFR, Int)
dsub_ r p d d1 = withMPFRBAd' r p (realToFrac d) d1 mpfr_d_sub


mul_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
mul_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_mul
      
mulw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
mulw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_mul_ui
      
muli_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
muli_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_mul_si

muld_          :: RoundMode -> Precision -> MPFR -> Double -> (MPFR, Int)
muld_ r p d1 d = withMPFRBAd r p d1 (realToFrac d) mpfr_mul_d
      
sqr_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sqr_ r p d = withMPFR r p d mpfr_sqr
      
div_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
div_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_div
      
divw_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
divw_ r p d1 d = withMPFRBAui r p d1 (fromIntegral d) mpfr_div_ui
      
divi_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
divi_ r p d1 d = withMPFRBAsi r p d1 (fromIntegral d) mpfr_div_si

divd_          :: RoundMode -> Precision -> MPFR -> Double -> (MPFR, Int)
divd_ r p d1 d = withMPFRBAd r p d1 (realToFrac d) mpfr_div_d
      
wdiv_          :: RoundMode -> Precision -> Word -> MPFR -> (MPFR, Int)
wdiv_ r p d d1 = withMPFRBAiu r p (fromIntegral d) d1 mpfr_ui_div
      
idiv_          :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
idiv_ r p d d1 = withMPFRBAis r p (fromIntegral d) d1 mpfr_si_div

ddiv_          :: RoundMode -> Precision -> Double -> MPFR -> (MPFR, Int)
ddiv_ r p d d1 = withMPFRBAd' r p (realToFrac d) d1 mpfr_d_div
      
sqrt_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sqrt_ r p d = withMPFR r p d mpfr_sqrt
      
sqrtw_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
sqrtw_ r p d = withMPFRUI r p d mpfr_sqrt_ui

recSqrt_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
recSqrt_ r p d = withMPFR r p d mpfr_rec_sqrt
      
cbrt_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cbrt_ r p d = withMPFR r p d mpfr_cbrt
      
root_        :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
root_ r p d n = withMPFRBAui r p d (fromIntegral n) mpfr_root
      
pow_          :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
pow_ r p d1 d2 = withMPFRsBA r p d1 d2 mpfr_pow 
      
poww_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR , Int)
poww_ r p d1 d2 = withMPFRBAui r p d1 (fromIntegral d2) mpfr_pow_ui
      
powi_           :: RoundMode -> Precision -> MPFR -> Int -> (MPFR , Int)
powi_ r p d1 d2 = withMPFRBAsi r p d1 (fromIntegral d2) mpfr_pow_si
      
wpoww_          :: RoundMode -> Precision -> Word -> Word -> (MPFR , Int)
wpoww_ r p d1 d2 = unsafePerformIO go
    where go = withDummy p $ \p1 -> 
                    mpfr_ui_pow_ui p1 (fromIntegral d1) (fromIntegral d2) ((fromIntegral . fromEnum) r)
        
wpow_           :: RoundMode -> Precision -> Word -> MPFR -> (MPFR , Int)
wpow_ r p d1 d2 = withMPFRBAiu r p (fromIntegral d1) d2 mpfr_ui_pow
      
neg_                       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
neg_ r p mp1@(MP p' s e fp) | p' == fromIntegral p && e /= expNaN = (MP p' (negate s) e fp, 0)
                            | otherwise = withMPFR r p mp1 mpfr_neg
      
absD_      :: RoundMode -> Precision -> MPFR -> (MPFR , Int)
absD_ r p d@(MP p' s e fp) | p' == fromIntegral p && e /= expNaN = (MP p' (abs s) e fp, 0)
                           | otherwise                           = withMPFR r p d mpfr_abs
      
dim_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
dim_ r p d1 d2 = withMPFRsBA r p d1 d2 mpfr_dim
      
mul2w_           :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
mul2w_ r p d1 d2 = withMPFRBAui r p d1 (fromIntegral d2) mpfr_mul_2ui
      
mul2i_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
mul2i_ r p d1 d2 = withMPFRBAsi r p d1 (fromIntegral d2) mpfr_mul_2si
      
div2w_          :: RoundMode -> Precision -> MPFR -> Word -> (MPFR, Int)
div2w_ r p d1 d2 = withMPFRBAui r p d1 (fromIntegral d2) mpfr_div_2ui
      
div2i_          :: RoundMode -> Precision -> MPFR -> Int -> (MPFR, Int)
div2i_ r p d1 d2 = withMPFRBAsi r p d1 (fromIntegral d2) mpfr_div_2si
