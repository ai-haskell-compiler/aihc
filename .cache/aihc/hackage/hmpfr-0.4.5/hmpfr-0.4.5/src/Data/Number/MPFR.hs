{-# LANGUAGE MagicHash, CPP #-}

{-|
    Module      :  Data.Number.MPFR
    Description :  Pure interface to the MPFR library.
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

This module exports a pure interface to the MPFR library functions. Functions
return new 'MPFR' structures instead of modifying existing ones and so all
functions which produce a new MPFR structure take one more parameter than
their original @C@ counterparts. This parameter, 'Precision', is the precision
of the resulting 'MPFR'.

This is naturally slower than modifying in-place, especially when dealing
with lower precisions, so a \"mutable\" interface is provided in 
"Data.Number.MPFR.Mutable" module.


/Naming conventions/

    - functions ending with _ (underscore) usually return a pair @('MPFR', 'Int')@, where
      'Int' is a return value of a corresponding @mpfr_@ function. See the MPFR manual for 
      a description of return values.

    - the same functions without the _ return just the 'MPFR'. 

    - @mpfr_@ prefix in functions is removed

    - @_ui@ and @ui_@ in function becomes @w@ (stands for 'Word').
      For example @mpfr_sub_ui@ becomes @'subw'@ and @mpfr_ui_sub@ becomes 'wsub'.

    - @si_@ and @_si@ in functions becomes @i@ (stands for 'Int').
      For example @mpfr_sub_si@ becomes @'subi'@ and @mpfr_si_sub@ becomes 'isub'.

    - comparison functions which have @_p@ appended loose it.
      For example @mpfr_less_p@ becomes @'less'@.

/Instances/

    [@'Eq'@]

        - NaN \/= NaN,

        - Infinity = Infinity, 

        - \-Infinity = -Infinity

        - otherwise normal comparison 


    [@'Ord'@]
 
        - compare NaN _ = 'GT'

        - compare _ NaN = 'GT'
  
        - infinity < _ = 'False'

        - \-infinity > _ = 'False'

        - NaN [\<,\>,\>=,<=] _ = 'False'

This mimics the behaviour of built in Haskell 'Float' and 'Double'.

If you need instances of numeric typeclasses import one of the 
Data.Number.MPFR.Instances.* modules.
-}

module Data.Number.MPFR (
         RoundMode (Near, Up, Down, Zero),
         MPFR, Precision(), Exp, MpSize,
         -- * Assignment functions
         -- | See <http://www.mpfr.org/mpfr-current/mpfr.html#Assignment-Functions>
         --  documentation on particular functions.
         module Data.Number.MPFR.Assignment,
         -- * Conversion functions
         -- |  See <http://www.mpfr.org/mpfr-current/mpfr.html#Conversion-Functions>
         --  documentation on particular functions.
         module Data.Number.MPFR.Conversion,
         -- * Basic arithmetic functions
         -- |  For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Basic-Arithmetic-Functions>.
         module Data.Number.MPFR.Arithmetic,
         -- * Comparison functions
         -- | For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Comparison-Functions>
         module Data.Number.MPFR.Comparison,
         -- * Special functions
         -- | For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Special-Functions>.

         module Data.Number.MPFR.Special,
         -- * Integer related functions
         -- | For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-chttp://www.mpfr.org/mpfr-current/mpfr.html#Integer-Related-Functions>
         module Data.Number.MPFR.Integer,
         -- * Miscellaneous functions
         -- |For documentation on particular functions see
         -- <http://www.mpfr.org/mpfr-current/mpfr.html#Miscellaneous-Functions>.
         module Data.Number.MPFR.Misc
) where

import Data.Number.MPFR.Assignment 
import Data.Number.MPFR.Conversion
import Data.Number.MPFR.Arithmetic
import Data.Number.MPFR.Comparison
import Data.Number.MPFR.Special 
import Data.Number.MPFR.Integer
import Data.Number.MPFR.Misc

import Data.Number.MPFR.Internal

{-
#if __GLASGOW_HASKELL__ >= 610
import GHC.Integer.Internals
#endif
import GHC.Exts

instance Num MPFR where
    d + d'        = add Zero (addPrec d d') d d'
    d - d'        = sub Zero (addPrec d d') d d'
    d * d'        = mul Zero (getPrec d + getPrec d') d d'
    negate d      = neg Zero (getPrec d) d
    abs d         = absD Zero (getPrec d) d
    signum        = fromInt Zero minPrec . fromMaybe (-1) . sgn
    fromInteger (S# i) = fromInt Zero minPrec (I# i)
    fromInteger i@(J# n _) = fromIntegerA Zero (fromIntegral . abs $ I# n * bitsPerIntegerLimb) i 

addPrec       :: MPFR -> MPFR -> Precision
addPrec d1 d2 = fromIntegral (max (p1 + e1 - e3) (p2 + e2 - e3)) + 1
                where e1 = if d1 == 0 then 0 else getExp d1
                      e2 = if d2 == 0 then 0 else getExp d2
                      p1 = fromIntegral $ getPrec d1
                      p2 = fromIntegral $ getPrec d2
                      e3 = min e1 e2

instance Real MPFR where
    toRational d = n % 2 ^ e
        where (n', e') = decompose d
              (n, e) = if e' >= 0 then ((n' * 2 ^ e'), 0)
                         else (n', - e')

-}