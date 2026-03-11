
{-# LANGUAGE CPP #-}

{-# OPTIONS_GHC -Wall -fwarn-tabs #-}

#if defined(__HUGS__) && (__HUGS__ <= 200609)
#define REALFLOAT_VERSION corrected Hugs version.
#elif defined(__GLASGOW_HASKELL__) || defined(__NHC__)
#define REALFLOAT_VERSION normal Prelude version. This should be correct.
#else
#define REALFLOAT_VERSION normal Prelude version. This could be buggy.
#endif
----------------------------------------------------------------
--                                                  ~ 2021.10.17
-- |
-- Module      :  Hugs.RealFloat
-- Copyright   :  Copyright (c) 2007--2021 wren gayle romano
-- License     :  BSD3
-- Maintainer  :  wren@cpan.org
-- Stability   :  stable
-- Portability :  portable (with CPP)
--
-- Hugs (September 2006) has buggy definitions for 'Prelude.isNaN'
-- and 'Prelude.isInfinite' on Float and Double. If this module is
-- run through CPP with the macro @__HUGS__@ set to a value no
-- larger than 200609, then correct definitions are used. Otherwise
-- the Prelude definitions are used (which should be correct for
-- other compilers). For example, run Hugs with
--
-- @hugs -F'cpp -P -D__HUGS__=200609' Hugs/RealFloat.hs@
--
-- N.B. The corrected definitions have only been tested to work for
-- 'Float' and 'Double'. These definitions should probably not be
-- used for other 'RealFloat' types.
--
-- /This installation was compiled with the REALFLOAT_VERSION/
----------------------------------------------------------------
module Hugs.RealFloat
    ( isInfinite
    , isNaN
    ) where

import Prelude hiding (isInfinite, isNaN)
import qualified Prelude
----------------------------------------------------------------

isInfinite :: (RealFloat a) => a -> Bool
{-# INLINE isInfinite #-}
#if defined(__HUGS__) && (__HUGS__ <= 200609)
isInfinite x = (1/0) == abs x
#else
isInfinite = Prelude.isInfinite
#endif


isNaN :: (RealFloat a) => a -> Bool
{-# INLINE isNaN #-}
#if defined(__HUGS__) && (__HUGS__ <= 200609)
isNaN x = compareEQ x 0 && compareEQ x 1

-- | In Hugs (September 2006), 'compare' always returns @EQ@ if one
-- of the arguments is not a number. Thus, if a number is @compareEQ@
-- against multiple different numbers, then it must be @isNaN@.
compareEQ :: (Ord a) => a -> a -> Bool
compareEQ x y =
    case compare x y of
    EQ -> True
    _  -> False
#else
isNaN = Prelude.isNaN
#endif
----------------------------------------------------------------
----------------------------------------------------------- fin.
