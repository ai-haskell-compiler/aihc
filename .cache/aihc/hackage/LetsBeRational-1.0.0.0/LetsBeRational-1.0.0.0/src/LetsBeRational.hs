{- |
Copyright: (c) 2021 Ghais Issa
SPDX-License-Identifier: MIT
Maintainer: Ghais Issa <0x47@0x49.dev>

Haskell binding for Jaekel's "Let's be Rational" implied volatility calculation
-}

module LetsBeRational
       ( lbr
       ) where
import           Foreign.C.Types
import Data.Coerce (coerce)


foreign import ccall
   "lets_be_rational.h implied_volatility_from_a_transformed_rational_guess" c_lbr ::
     CDouble -> CDouble  -> CDouble -> CDouble  -> CDouble  -> CDouble

-- | Calculate implied volatility for a European option using Let's Be Rational.
lbr :: Int   -- ^ 1 for CALL -1 for PUT.
  -> Double -- ^ Forward
  -> Double -- ^ Strike
  -> Double -- ^ Time to maturity
  -> Double -- ^ Premium
  -> Double -- ^ Implied vol.
lbr cp f k t p = coerce $ c_lbr (coerce p) (coerce f) (coerce k) (coerce t) (coerce (fromIntegral cp::Double))
