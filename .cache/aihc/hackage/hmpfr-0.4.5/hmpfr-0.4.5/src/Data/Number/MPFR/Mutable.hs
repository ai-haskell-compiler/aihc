{-|
    Module      :  Data.Number.MPFR.Mutable
    Description :  Mutable MPFR's
    Copyright   :  (c) Aleš Bizjak
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  non-portable

This module provides a \"mutable\" interface to the MPFR library. Functions i
this module should have very little overhead over the original @C@ functions.

Type signatures of functions should be self-explanatory. Order of arguments
is identical to the one in @C@ functions. See MPFR manual for documentation
on particular functions.

All operations are performed in the 'ST' monad so safe transition between mutable
and immutable interface is possible with 'runST'. For example mutable interface 
could be used in inner loops or in local calculations with temporary variables,
helping reduce allocation overhead of the pure interface.
-}

module Data.Number.MPFR.Mutable (
     MMPFR,
     -- * Utility functions
     thaw, writeMMPFR, freeze, 
     unsafeThaw, unsafeWriteMMPFR, unsafeFreeze, 

     -- * Basic arithmetic functions
     -- | For documentation on particular functions see
     -- <http://www.mpfr.org/mpfr-current/mpfr.html#Basic-Arithmetic-Functions>

     module Data.Number.MPFR.Mutable.Arithmetic,
     -- * Special functions
     -- | For documentation on particular functions see
     -- <http://www.mpfr.org/mpfr-current/mpfr.html#Special-Functions>

     module Data.Number.MPFR.Mutable.Special,
     -- * Miscellaneous functions
     -- | For documentation on particular functions see
     -- <http://www.mpfr.org/mpfr-current/mpfr.html#Miscellaneous functions>

     module Data.Number.MPFR.Mutable.Misc,
     -- * Integer related functions
     -- | For documentation on particular functions see
     -- <http://www.mpfr.org/mpfr-current/mpfr.html#Integer-Related-Functions>

     module Data.Number.MPFR.Mutable.Integer
) where

import Data.Number.MPFR.Mutable.Arithmetic
import Data.Number.MPFR.Mutable.Special
import Data.Number.MPFR.Mutable.Misc
import Data.Number.MPFR.Mutable.Integer

import Data.Number.MPFR.Mutable.Internal
