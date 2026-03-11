{-|
    Module      :  Data.Number.MPFR.Mutable.Misc
    Description :  Miscellaneous functions
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

 For documentation on particular functions see
 <http://www.mpfr.org/mpfr-current/mpfr.html#Miscellaneous-Functions>.
-}
module Data.Number.MPFR.Mutable.Misc where

import Data.Number.MPFR.Mutable.Internal

import Control.Monad.ST(ST)

{-TODO
nextToward :: MMPFR s -> MMPFR s -> ST s ()
nextToward = withMutableMPFRSNR mpfr_nexttoward
-}

nextAbove :: MMPFR s -> ST s ()
nextAbove = withMutableMPFRSNRNR mpfr_nextabove

nextbelow :: MMPFR s -> ST s ()
nextbelow = withMutableMPFRSNRNR mpfr_nextbelow

max :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
max = withMutableMPFRBA mpfr_max

min :: MMPFR s -> MMPFR s -> MMPFR s -> RoundMode -> ST s Int
min = withMutableMPFRBA mpfr_min

getExp :: MMPFR s -> ST s Exp
getExp m = unsafeFreeze m >>= return . \(MP _ _ e _) -> e 

{- TODO
setExp     :: MPFR s -> Exp -> ST s Int
setExp m e = do m' <- unsafeReadMMPFR m
                uns
-}
getPrec   :: MMPFR s -> ST s Precision
getPrec m = unsafeFreeze m >>= return . \(MP p _ _ _) -> fromIntegral p

