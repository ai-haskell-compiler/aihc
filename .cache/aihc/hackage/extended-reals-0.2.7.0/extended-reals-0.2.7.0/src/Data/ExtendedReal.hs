{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.ExtendedReal
-- Copyright   :  (c) Masahiro Sakai 2014
-- License     :  BSD-style
-- Maintainer  :  masahiro.sakai@gmail.com
--
-- Extension of real numbers with positive/negative infinities (±∞).
-- It is useful for describing various limiting behaviors in mathematics.
--
-- Remarks:
--
-- * @∞ - ∞@ is left undefined as usual,
--   but we define @0 × ∞ = 0 × -∞ = 0@ by following the convention of
--   probability or measure theory.
--
-- References:
--
-- * Wikipedia contributors, "Extended real number line," Wikipedia,
--   The Free Encyclopedia, https://en.wikipedia.org/wiki/Extended_real_number_line
--   (accessed September 1, 2014).
--
-----------------------------------------------------------------------------
module Data.ExtendedReal
  ( Extended (..)
  , inf
  , isFinite
  , isInfinite
  , fromRealFloat
  , toRealFloat
  ) where

import Prelude hiding (isInfinite)
import qualified Prelude as P
import Control.DeepSeq
import Data.Data (Data)
import Data.Hashable
import GHC.Generics (Generic)
import qualified GHC.Real
import Language.Haskell.TH.Syntax (Lift)

-- | @Extended r@ is an extension of /r/ with positive/negative infinity (±∞).
data Extended r
  = NegInf    -- ^ negative infinity (-∞)
  | Finite !r -- ^ finite value
  | PosInf    -- ^ positive infinity (+∞)
  deriving
  ( Ord
  , Eq
  , Show
  , Read
  , Data
  , Functor
  , Foldable    -- ^ @since 0.2.6.0
  , Traversable -- ^ @since 0.2.6.0
  , Generic     -- ^ @since 0.2.6.0
  , Lift        -- ^ @since 0.2.6.0
  )

instance Bounded (Extended r) where
  minBound = NegInf
  maxBound = PosInf

instance NFData r => NFData (Extended r)

instance Hashable r => Hashable (Extended r)

-- | Infinity (∞)
inf :: Extended r
inf = PosInf

-- | @isFinite x = not (isInfinite x)@.
isFinite :: Extended r -> Bool
isFinite (Finite _) = True
isFinite _ = False

-- | @isInfinite x@ returns @True@ iff @x@ is @PosInf@ or @NegInf@.
isInfinite :: Extended r -> Bool
isInfinite = not . isFinite

-- | Note that @Extended r@ is /not/ a field, nor a ring.
-- 
-- @PosInf + NegInf@ is left undefined as usual,
-- but we define @0 * PosInf = 0 * NegInf = 0@ by following the convention of probability or measure theory.
instance (Num r, Ord r) => Num (Extended r) where
  Finite a + Finite b = Finite (a+b)
  PosInf + NegInf = error "PosInf + NegInf is undefined"
  NegInf + PosInf = error "NegInf + PosInf is undefined"
  PosInf + _ = PosInf
  _ + PosInf = PosInf
  NegInf + _ = NegInf
  _ + NegInf = NegInf

  Finite x1 * e = scale x1 e
  e * Finite x2 = scale x2 e
  PosInf * PosInf = numericPosInf
  PosInf * NegInf = numericNegInf
  NegInf * PosInf = numericNegInf
  NegInf * NegInf = numericPosInf

  negate NegInf = PosInf
  negate (Finite x) = Finite (negate x)
  negate PosInf = NegInf

  abs NegInf = PosInf
  abs (Finite x) = Finite (abs x)
  abs PosInf = PosInf

  signum NegInf = Finite (-1)
  signum (Finite x) = Finite (signum x)
  signum PosInf = Finite 1

  fromInteger = Finite . fromInteger  

numericPosInf :: forall r. (Ord r, Num r) => Extended r
numericPosInf = if (1 :: r) >= 0 then PosInf else NegInf

numericNegInf :: forall r. (Ord r, Num r) => Extended r
numericNegInf = if (1 :: r) >= 0 then NegInf else PosInf

-- | Note that @Extended r@ is /not/ a field, nor a ring.
instance (Fractional r, Ord r) => Fractional (Extended r) where
  recip (Finite x) = Finite (1/x)
  recip _ = Finite 0

  fromRational r
    | r == GHC.Real.notANumber = error "fromRational: argument should not be NaN"
    | r == GHC.Real.infinity = PosInf
    | r == -GHC.Real.infinity = NegInf
    | otherwise = Finite $ fromRational r

-- Note that we define @0 * PosInf = 0 * NegInf = 0@ by the convention of probability or measure theory.
scale :: (Num r, Ord r) => r -> Extended r -> Extended r
scale a e = seq e $
  case a `compare` 0 of
    EQ -> Finite 0
    GT ->
      case e of
        NegInf   -> NegInf
        Finite b -> Finite (a*b)
        PosInf   -> PosInf
    LT ->
      case e of
        NegInf   -> PosInf
        Finite b -> Finite (a*b)
        PosInf   -> NegInf

-- | Helper to convert 'Double' and 'Float' to 'Extended',
-- taking care of infinite values automatically.
--
-- >>> fromRealFloat (1 / 0)
-- PosInf
-- >>> fromRealFloat (-1 / 0)
-- NegInf
-- >>> fromRealFloat (0 / 0)
-- *** Exception: fromRealFloat: argument should not be NaN
--
-- Beware that an ordinal infinity might not be equal to an arithmetic infinity.
-- 'PosInf' / 'NegInf' stand for infinite elements with regards to ordering, so:
--
-- >>> fromRealFloat (Down (1 / 0))
-- NegInf
-- >>> fromRealFloat (Down (-1 / 0))
-- PosInf
--
-- @since 0.2.5.0
fromRealFloat :: RealFloat r => r -> Extended r
fromRealFloat x
  | isNaN x = error "fromRealFloat: argument should not be NaN"
  | P.isInfinite x = if x > 0 then PosInf else NegInf
  | otherwise = Finite x

-- | Helper to convert 'Extended' to 'Double' or 'Float',
-- taking care of infinite values automatically.
--
-- >>> toRealFloat PosInf :: Double
-- Infinity
-- >>> toRealFloat NegInf :: Double
-- -Infinity
-- >>> toRealFloat PosInf :: Down Double
-- Down (-Infinity)
-- >>> toRealFloat NegInf :: Down Double
-- Down Infinity
--
-- @since 0.2.7.0
toRealFloat :: RealFloat r => Extended r -> r
toRealFloat = \case
  NegInf -> negInf
  Finite r -> r
  PosInf -> posInf
  where
    -- Less hacky than 1/0, but hacky nevertheless.
    infinity = encodeFloat 1 maxBound
    -- For Data.Ord.Down Double an arithmetic positive infinity
    -- is a negative infinity with regards to Ord instance.
    posInf = if infinity > 0 then infinity else -infinity
    negInf = negate posInf
