{-|
    Module      :  Data.Number.MPFR.Special
    Description :  Special functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-current/mpfr.html#Special-Functions>.
-}

module Data.Number.MPFR.Special where

import Data.Number.MPFR.Internal

log     :: RoundMode -> Precision -> MPFR -> MPFR
log r p = fst . log_ r p

log2     :: RoundMode -> Precision -> MPFR -> MPFR
log2 r p = fst . log2_ r p

log10     :: RoundMode -> Precision -> MPFR -> MPFR
log10 r p = fst . log10_ r p

exp     :: RoundMode -> Precision -> MPFR -> MPFR
exp r p = fst . exp_ r p

exp2     :: RoundMode -> Precision -> MPFR -> MPFR
exp2 r p = fst . exp2_ r p

exp10     :: RoundMode -> Precision -> MPFR -> MPFR
exp10 r p = fst . exp10_ r p

sin     :: RoundMode -> Precision -> MPFR -> MPFR
sin r p = fst . sin_ r p

cos     :: RoundMode -> Precision -> MPFR -> MPFR
cos r p = fst . cos_ r p

tan     :: RoundMode -> Precision -> MPFR -> MPFR
tan r p = fst . tan_ r p

sec     :: RoundMode -> Precision -> MPFR -> MPFR
sec r p = fst . sec_ r p

csc     :: RoundMode -> Precision -> MPFR -> MPFR
csc r p = fst . csc_ r p

cot     :: RoundMode -> Precision -> MPFR -> MPFR
cot r p = fst . cot_ r p

sincos          :: RoundMode
                -> Precision -- ^ precision to compute sin
                -> Precision -- ^ precision to compute cos
                -> MPFR
                -> (MPFR, MPFR) -- ^ return (sin x, cos x)
sincos r p p' d = case sincos_ r p p' d of
                    (a, b, _) -> (a, b)

asin     :: RoundMode -> Precision -> MPFR -> MPFR
asin r p = fst . asin_ r p

acos     :: RoundMode -> Precision -> MPFR -> MPFR
acos r p = fst . acos_ r p

atan     :: RoundMode -> Precision -> MPFR -> MPFR
atan r p = fst . atan_ r p

atan2       :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
atan2 r p d = fst . atan2_ r p d

sinh     :: RoundMode -> Precision -> MPFR -> MPFR
sinh r p = fst . sinh_ r p

cosh     :: RoundMode -> Precision -> MPFR -> MPFR
cosh r p = fst . cosh_ r p

tanh     :: RoundMode -> Precision -> MPFR -> MPFR
tanh r p = fst . tanh_ r p

sinhcosh          :: RoundMode
                  -> Precision -- ^ precision to compute sin
                  -> Precision -- ^ precision to compute cos
                  -> MPFR
                  -> (MPFR, MPFR) -- ^ return (sin x, cos x)
sinhcosh r p p' d = case sinhcosh_ r p p' d of
                    (a, b, _) -> (a, b)

sech     :: RoundMode -> Precision -> MPFR -> MPFR
sech r p = fst . sech_ r p

csch     :: RoundMode -> Precision -> MPFR -> MPFR
csch r p = fst . csch_ r p

coth     :: RoundMode -> Precision -> MPFR -> MPFR
coth r p = fst . coth_ r p

acosh     :: RoundMode -> Precision -> MPFR -> MPFR
acosh r p = fst . acosh_ r p

asinh     :: RoundMode -> Precision -> MPFR -> MPFR
asinh r p = fst . asinh_ r p

atanh     :: RoundMode -> Precision -> MPFR -> MPFR
atanh r p = fst . atanh_ r p

facw     :: RoundMode -> Precision -> Word -> MPFR
facw r p = fst . facw_ r p

log1p     :: RoundMode -> Precision -> MPFR -> MPFR
log1p r p = fst . log1p_ r p

expm1     :: RoundMode -> Precision -> MPFR -> MPFR
expm1 r p = fst . expm1_ r p

eint     :: RoundMode -> Precision -> MPFR -> MPFR
eint r p = fst . eint_ r p

li2     :: RoundMode -> Precision -> MPFR -> MPFR
li2 r p = fst . li2_ r p

gamma     :: RoundMode -> Precision -> MPFR -> MPFR
gamma r p = fst . gamma_ r p

lngamma     :: RoundMode -> Precision -> MPFR -> MPFR
lngamma r p = fst . lngamma_ r p

lgamma       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
lgamma r p d = case lgamma_ r p d of
                 (a, b, _) -> (a,b)

digamma     :: RoundMode -> Precision -> MPFR -> MPFR
digamma r p = fst . digamma_ r p

zeta     :: RoundMode -> Precision -> MPFR -> MPFR
zeta r p = fst . zeta_ r p

zetaw     :: RoundMode -> Precision -> Word -> MPFR
zetaw r p = fst . zetaw_ r p

erf     :: RoundMode -> Precision -> MPFR -> MPFR
erf r p = fst . erf_ r p

erfc     :: RoundMode -> Precision -> MPFR -> MPFR
erfc r p = fst . erfc_ r p

j0     :: RoundMode -> Precision -> MPFR -> MPFR
j0 r p = fst . j0_ r p

j1     :: RoundMode -> Precision -> MPFR -> MPFR
j1 r p = fst . j1_ r p

jn       :: RoundMode -> Precision -> Int -> MPFR -> MPFR
jn r p w = fst . jn_ r p w

y0     :: RoundMode -> Precision -> MPFR -> MPFR
y0 r p = fst . y0_ r p

y1     :: RoundMode -> Precision -> MPFR -> MPFR
y1 r p = fst . y1_ r p

yn       :: RoundMode -> Precision -> Int -> MPFR -> MPFR
yn r p w = fst . yn_ r p w

fma           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> MPFR
fma r p d1 d2 = fst . fma_ r p d1 d2

fms           :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> MPFR
fms r p d1 d2 = fst . fms_ r p d1 d2

agm        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
agm r p d1 = fst . agm_ r p d1

hypot        :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR
hypot r p d1 = fst . hypot_ r p d1

pi   :: RoundMode -> Precision -> MPFR
pi r = fst . pi_ r

log2c   :: RoundMode -> Precision -> MPFR
log2c r = fst . log2c_ r

euler   :: RoundMode -> Precision -> MPFR
euler r = fst . euler_ r

catalan   :: RoundMode -> Precision -> MPFR
catalan r = fst . catalan_ r


log_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log_ r p d = withMPFR r p d mpfr_log

log2_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log2_ r p d = withMPFR r p d mpfr_log2

log10_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log10_ r p d = withMPFR r p d mpfr_log10

exp_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
exp_ r p d = withMPFR r p d mpfr_exp

exp2_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
exp2_ r p d = withMPFR r p d mpfr_exp2

exp10_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
exp10_ r p d = withMPFR r p d mpfr_exp10

sin_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sin_ r p d = withMPFR r p d mpfr_sin

cos_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cos_ r p d = withMPFR r p d mpfr_cos

tan_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
tan_ r p d = withMPFR r p d mpfr_tan

sec_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sec_ r p d = withMPFR r p d mpfr_sec

csc_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
csc_ r p d = withMPFR r p d mpfr_csc

cot_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cot_ r p d = withMPFR r p d mpfr_cot


sincos_ :: RoundMode
         -> Precision -- ^ precision to compute sin
         -> Precision -- ^ precision to compute cos
         -> MPFR
         -> (MPFR, MPFR, Int)
sincos_ r p p' d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  ls' <- mpfr_custom_get_size (fromIntegral p')
                  fp' <- mallocForeignPtrBytes (fromIntegral ls')
                  alloca $ \p1 -> do
                    pokeDummy p1 fp (fromIntegral ls)
                    alloca $ \p2 -> do
                      pokeDummy p2 fp' (fromIntegral ls')
                      with d $ \p3 -> do
                        r3 <- mpfr_sin_cos p1 p2 p3 ((fromIntegral . fromEnum) r)
                        r1 <- peekP p1 fp
                        r2 <- peekP p2 fp'
                        return (r1, r2, fromIntegral r3)

asin_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
asin_ r p d = withMPFR r p d mpfr_asin

acos_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
acos_ r p d = withMPFR r p d mpfr_acos

atan_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
atan_ r p d = withMPFR r p d mpfr_atan

atan2_ :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
atan2_ r p d d' = withMPFRsBA r p d d' mpfr_atan2

sinh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sinh_ r p d = withMPFR r p d mpfr_sinh

cosh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
cosh_ r p d = withMPFR r p d mpfr_cosh

tanh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
tanh_ r p d = withMPFR r p d mpfr_tanh

sinhcosh_          :: RoundMode
                   -> Precision -- ^ precision to compute sinh
                   -> Precision -- ^ precision to compute cosh
                   -> MPFR
                   -> (MPFR, MPFR, Int)
sinhcosh_ r p p' d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  ls' <- mpfr_custom_get_size (fromIntegral p')
                  fp' <- mallocForeignPtrBytes (fromIntegral ls')
                  alloca $ \p1 -> do
                    pokeDummy p1 fp (fromIntegral ls)
                    alloca $ \p2 -> do
                      pokeDummy p2 fp' (fromIntegral ls')
                      with d $ \p3 -> do
                        r3 <- mpfr_sinh_cosh p1 p2 p3 ((fromIntegral . fromEnum) r)
                        r1 <- peekP p1 fp
                        r2 <- peekP p2 fp'
                        return (r1, r2, fromIntegral r3)

sech_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
sech_ r p d = withMPFR r p d mpfr_sech

csch_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
csch_ r p d = withMPFR r p d mpfr_csch

coth_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
coth_ r p d = withMPFR r p d mpfr_coth

acosh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
acosh_ r p d = withMPFR r p d mpfr_acosh

asinh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
asinh_ r p d = withMPFR r p d mpfr_asinh

atanh_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
atanh_ r p d = withMPFR r p d mpfr_atanh

facw_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
facw_ r p w = withMPFRUI r p w mpfr_fac_ui

log1p_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
log1p_ r p d = withMPFR r p d mpfr_log1p

expm1_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
expm1_ r p d = withMPFR r p d mpfr_expm1

eint_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
eint_ r p d = withMPFR r p d mpfr_eint

li2_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
li2_ r p d = withMPFR r p d mpfr_li2

gamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
gamma_ r p d = withMPFR r p d mpfr_gamma

lngamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
lngamma_ r p d = withMPFR r p d mpfr_lngamma

lgamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int, Int)
lgamma_ r p d = unsafePerformIO go
    where go = do ls <- mpfr_custom_get_size (fromIntegral p)
                  fp <- mallocForeignPtrBytes (fromIntegral ls)
                  alloca $ \p1 -> do
                    pokeDummy p1 fp (fromIntegral ls)
                    with d $ \p2 ->
                      alloca $ \p3 -> do
                        r3 <- mpfr_lgamma p1 p3 p2 ((fromIntegral . fromEnum) r)
                        r2 <- peek p3
                        r1 <- peekP p1 fp
                        return (r1, fromIntegral r2, fromIntegral r3)

digamma_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
digamma_ r p d = withMPFR r p d mpfr_digamma

zeta_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
zeta_ r p d = withMPFR r p d mpfr_zeta

zetaw_       :: RoundMode -> Precision -> Word -> (MPFR, Int)
zetaw_ r p d = withMPFRUI r p d mpfr_zeta_ui

erf_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
erf_ r p d = withMPFR r p d mpfr_erf

erfc_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
erfc_ r p d = withMPFR r p d mpfr_erfc

j0_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
j0_ r p d = withMPFR r p d mpfr_j0

j1_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
j1_ r p d = withMPFR r p d mpfr_j1

jn_         :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
jn_ r p i d = withMPFRBAis r p (fromIntegral i) d mpfr_jn

y0_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
y0_ r p d = withMPFR r p d mpfr_y0

y1_       :: RoundMode -> Precision -> MPFR -> (MPFR, Int)
y1_ r p d = withMPFR r p d mpfr_y1

yn_         :: RoundMode -> Precision -> Int -> MPFR -> (MPFR, Int)
yn_ r p i d = withMPFRBAis r p (fromIntegral i) d mpfr_yn

fma_                 :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> (MPFR, Int)
fma_ r p mp1 mp2 mp3 = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                 with mp1 $ \p2 ->
                   with mp2 $ \p3 ->
                     with mp3 $ \p4 ->
                       mpfr_fma p1 p2 p3 p4 ((fromIntegral . fromEnum) r)

fms_                 :: RoundMode -> Precision -> MPFR -> MPFR -> MPFR -> (MPFR, Int)
fms_ r p mp1 mp2 mp3 = unsafePerformIO go
    where go = withDummy p $ \p1 ->
                 with mp1 $ \p2 ->
                   with mp2 $ \p3 ->
                     with mp3 $ \p4 ->
                       mpfr_fms p1 p2 p3 p4 ((fromIntegral . fromEnum) r)

agm_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
agm_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_agm

hypot_           :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR,Int)
hypot_ r p d1 d2 =  withMPFRsBA r p d1 d2 mpfr_hypot

pi_     :: RoundMode -> Precision -> (MPFR, Int)
pi_ r p = withMPFRC r p mpfr_const_pi

log2c_     :: RoundMode -> Precision -> (MPFR, Int)
log2c_ r p = withMPFRC r p mpfr_const_log2

euler_     :: RoundMode -> Precision -> (MPFR, Int)
euler_ r p = withMPFRC r p mpfr_const_euler

catalan_     :: RoundMode -> Precision -> (MPFR, Int)
catalan_ r p = withMPFRC r p mpfr_const_catalan

freeCache :: IO ()
freeCache = mpfr_free_cache
