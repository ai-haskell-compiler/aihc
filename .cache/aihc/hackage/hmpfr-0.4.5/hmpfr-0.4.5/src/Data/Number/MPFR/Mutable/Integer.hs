{-|
    Module      :  Data.Number.MPFR.Mutable.Integer
    Description :  Integer related functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-chttp://www.mpfr.org/mpfr-current/mpfr.html#Integer-Related-Functions>
-}

module Data.Number.MPFR.Mutable.Integer where

import Data.Number.MPFR.Mutable.Internal

import Control.Monad.ST(ST)

rint :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
rint = withMutableMPFRS mpfr_rint

ceil :: MMPFR s -> MMPFR s -> ST s Int
ceil = withMutableMPFRSNR mpfr_ceil

floor :: MMPFR s -> MMPFR s -> ST s Int
floor = withMutableMPFRSNR mpfr_floor

round :: MMPFR s -> MMPFR s -> ST s Int
round = withMutableMPFRSNR mpfr_round

trunc :: MMPFR s -> MMPFR s -> ST s Int
trunc = withMutableMPFRSNR mpfr_trunc

rintCeil :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
rintCeil = withMutableMPFRS mpfr_rint_ceil

rintFloor :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
rintFloor = withMutableMPFRS mpfr_rint_floor

rintRound :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
rintRound = withMutableMPFRS mpfr_rint_round

rintTrunc :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
rintTrunc = withMutableMPFRS mpfr_rint_trunc

modf :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
modf = withMutableMPFRSC mpfr_modf

frac :: MMPFR s -> MMPFR s -> RoundMode -> ST s Int
frac = withMutableMPFRS mpfr_frac

fmod :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
fmod = withMutableMPFRBA mpfr_fmod

remainder :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
remainder = withMutableMPFRBA mpfr_remainder

{-TODO
remquo          :: RoundMode -> Precision -> MPFR -> MPFR -> (MPFR, Int)
remquo r p d d' = case remquo_ r p d d' of
                     (a, b, _) -> (a, b)
-}
