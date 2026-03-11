{-|
    Module      :  Data.Number.MPFR.Mutable.Special
    Description :  Special functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-current/mpfr.html#Special-Functions>.
-}

module Data.Number.MPFR.Mutable.Special where

import Data.Number.MPFR.Mutable.Internal

import Control.Monad.ST(ST)

-- import Data.Word(Word)

log :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
log = withMutableMPFRS mpfr_log

log2 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
log2 = withMutableMPFRS mpfr_log2

log10 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
log10 = withMutableMPFRS mpfr_log10

exp :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
exp = withMutableMPFRS mpfr_exp

exp2 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
exp2 = withMutableMPFRS mpfr_exp2

exp10 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
exp10 = withMutableMPFRS mpfr_exp10

sin :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sin = withMutableMPFRS mpfr_sin

cos :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
cos = withMutableMPFRS mpfr_cos

tan :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
tan = withMutableMPFRS mpfr_tan

sec :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sec = withMutableMPFRS mpfr_sec

csc :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
csc = withMutableMPFRS mpfr_csc

cot :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
cot = withMutableMPFRS mpfr_cot

sincos :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sincos = withMutableMPFRSC mpfr_sin_cos

asin :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
asin = withMutableMPFRS mpfr_asin

acos :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
acos = withMutableMPFRS mpfr_acos

atan :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
atan = withMutableMPFRS mpfr_atan

atan2 :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
atan2 = withMutableMPFRBA mpfr_atan2

sinh :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sinh = withMutableMPFRS mpfr_sinh

cosh :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
cosh = withMutableMPFRS mpfr_cosh

tanh :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
tanh = withMutableMPFRS mpfr_tanh

sinhcosh :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sinhcosh = withMutableMPFRSC mpfr_sinh_cosh

sech :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
sech = withMutableMPFRS mpfr_sech

csch :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
csch = withMutableMPFRS mpfr_csch

coth :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
coth = withMutableMPFRS mpfr_coth

asinh :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
asinh = withMutableMPFRS mpfr_asinh

acosh :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
acosh = withMutableMPFRS mpfr_acosh

atanh :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
atanh = withMutableMPFRS mpfr_atanh

facw :: MMPFR s -> Word -> RoundMode -> ST s Int
facw = withMutableMPFRUI mpfr_fac_ui

log1p :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
log1p = withMutableMPFRS mpfr_log1p

expm1 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
expm1 = withMutableMPFRS mpfr_expm1

eint :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
eint = withMutableMPFRS mpfr_eint

li2 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
li2 = withMutableMPFRS mpfr_li2

gamma :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
gamma = withMutableMPFRS mpfr_gamma

lngamma :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
lngamma = withMutableMPFRS mpfr_lngamma

{- TODO
lgamma       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
lgamma r p d = case lgamma_ r p d of
                 (a, b, _) -> (a,b)
-}

digamma :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
digamma = withMutableMPFRS mpfr_digamma

zeta :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
zeta = withMutableMPFRS mpfr_zeta

zetaw :: MMPFR s -> Word -> RoundMode -> ST s Int
zetaw = withMutableMPFRUI mpfr_zeta_ui

erf :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
erf = withMutableMPFRS mpfr_erf

erfc :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
erfc = withMutableMPFRS mpfr_erfc

j0 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
j0 = withMutableMPFRS mpfr_j0

j1 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
j1 = withMutableMPFRS mpfr_j1

jn :: MMPFR s -> Word -> MMPFR s -> RoundMode -> ST s Int
jn = withMutableMPFR2 mpfr_jn

y0 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
y0 = withMutableMPFRS mpfr_y0

y1 :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
y1 = withMutableMPFRS mpfr_y1

yn :: MMPFR s -> Word -> MMPFR s -> RoundMode -> ST s Int
yn = withMutableMPFR2 mpfr_yn

fma :: MMPFR s -> MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
fma = withMutableMPFRBA3 mpfr_fma

fms :: MMPFR s -> MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
fms = withMutableMPFRBA3 mpfr_fms

agm :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
agm = withMutableMPFRBA mpfr_agm

hypot :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
hypot = withMutableMPFRBA mpfr_hypot

{- INTERNAL CACHES, dangerous
pi   :: RoundMode -> Precision -> MPFR
pi r = fst . pi_ r

log2c   :: RoundMode -> Precision -> MPFR
log2c r = fst . pi_ r

euler   :: RoundMode -> Precision -> MPFR
euler r = fst . pi_ r

catalan   :: RoundMode -> Precision -> MPFR
catalan r = fst . pi_ r
-}
