{-|
    Module      :  Data.Number.MPFR.Mutable.Arithmetic
    Description :  Basic arithmetic functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-current/mpfr.html#Basic-Arithmetic-Functions>.
-}

module Data.Number.MPFR.Mutable.Arithmetic where

import Data.Number.MPFR.Mutable.Internal

import Prelude hiding(isNaN)

import Control.Monad.ST(ST)

-- import Data.Word(Word)

add :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
add = withMutableMPFRBA mpfr_add

addw :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
addw = withMutableMPFR1 mpfr_add_ui

addi :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
addi = withMutableMPFR1 mpfr_add_si

addd :: MMPFR s -> MMPFR s -> Double -> RoundMode -> ST s Int
addd = withMutableMPFRd mpfr_add_d

sub :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sub = withMutableMPFRBA mpfr_sub

subw :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
subw = withMutableMPFR1 mpfr_sub_ui

subi :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
subi = withMutableMPFR1 mpfr_sub_si

subd :: MMPFR s -> MMPFR s -> Double -> RoundMode -> ST s Int
subd = withMutableMPFRd mpfr_sub_d

wsub :: MMPFR s -> Word -> MMPFR s -> RoundMode -> ST s Int
wsub = withMutableMPFR2 mpfr_ui_sub

isub :: MMPFR s -> Int -> MMPFR s -> RoundMode -> ST s Int
isub = withMutableMPFR2 mpfr_si_sub

dsub :: MMPFR s -> Double ->  MMPFR s -> RoundMode -> ST s Int
dsub = withMutableMPFRd' mpfr_d_sub

mul :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
mul = withMutableMPFRBA mpfr_mul

mulw :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
mulw = withMutableMPFR1 mpfr_mul_ui

muli :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
muli = withMutableMPFR1 mpfr_mul_si

muld :: MMPFR s -> MMPFR s -> Double -> RoundMode -> ST s Int
muld = withMutableMPFRd mpfr_mul_d

sqr ::  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sqr = withMutableMPFRS mpfr_sqr

div :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
div = withMutableMPFRBA mpfr_div

divw :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
divw = withMutableMPFR1 mpfr_div_ui

divi :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
divi = withMutableMPFR1 mpfr_div_si

divd :: MMPFR s -> MMPFR s -> Double -> RoundMode -> ST s Int
divd = withMutableMPFRd mpfr_div_d

wdiv :: MMPFR s -> Word -> MMPFR s -> RoundMode -> ST s Int
wdiv = withMutableMPFR2 mpfr_ui_div

idiv :: MMPFR s -> Int -> MMPFR s -> RoundMode -> ST s Int
idiv = withMutableMPFR2 mpfr_si_div

ddiv :: MMPFR s -> Double ->  MMPFR s -> RoundMode -> ST s Int
ddiv = withMutableMPFRd' mpfr_d_div

sqrt ::  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sqrt = withMutableMPFRS mpfr_sqrt

sqrtw :: MMPFR s -> Word -> RoundMode -> ST s Int
sqrtw = withMutableMPFRUI mpfr_sqrt_ui

recSqrt ::  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
recSqrt = withMutableMPFRS mpfr_rec_sqrt

cbrt ::  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
cbrt = withMutableMPFRS mpfr_cbrt

root :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
root = withMutableMPFR1 mpfr_root

pow :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
pow = withMutableMPFRBA mpfr_pow

poww :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
poww = withMutableMPFR1 mpfr_pow_ui

powi :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
powi = withMutableMPFR1 mpfr_pow_si

wpow :: MMPFR s -> Word -> MMPFR s -> RoundMode -> ST s Int
wpow = withMutableMPFR2 mpfr_ui_pow

--wpoww       :: RoundMode -> Precision -> Word -> Word -> MPFR
--wpoww r p d = fst . wpoww_ r p d

neg ::  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
neg = withMutableMPFRS mpfr_neg

absD ::  MMPFR s -> MMPFR s -> RoundMode -> ST s Int
absD = withMutableMPFRS mpfr_abs

dim :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
dim = withMutableMPFRBA mpfr_dim

mul2w :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
mul2w = withMutableMPFR1 mpfr_mul_2ui

mul2i :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
mul2i = withMutableMPFR1 mpfr_mul_2si

div2w :: MMPFR s -> MMPFR s -> Word -> RoundMode -> ST s Int
div2w = withMutableMPFR1 mpfr_div_2ui

div2i :: MMPFR s -> MMPFR s -> Int -> RoundMode -> ST s Int
div2i = withMutableMPFR1 mpfr_div_2si
