{-# LANGUAGE ApplicativeDo                            #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveGeneric                            #-}
{-# LANGUAGE GADTs                                    #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                            #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}
{-# OPTIONS_HADDOCK not-home                          #-}

-- |
-- Module      : Numeric.EMD.Internal.Spline
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Internal splining functionality exported for testing purposes only.
-- This will likely go away in future versions, so please do not depend on
-- this!
--

module Numeric.EMD.Internal.Spline (
    Spline, SplineEnd(..)
  , makeSpline
  , sampleSpline
  ) where

import           Data.Finite
import           Data.Maybe
import           Data.Proxy
import           Data.Type.Equality
import           GHC.Generics                     (Generic)
import           GHC.TypeLits.Compare
import           GHC.TypeNats
import           Numeric.EMD.Internal.Tridiagonal
import qualified Data.Binary                      as Bi
import qualified Data.Map                         as M
import qualified Data.Vector.Sized                as SV

-- | End condition for spline
data SplineEnd a
    -- | "Not-a-knot" condition: third derivatives are continuous at
    -- endpoints.  Default for matlab spline.
    = SENotAKnot
    -- | \"Natural\" condition: curve becomes a straight line at endpoints.
    | SENatural
    -- | \"Clamped\" condition: Slope of curves at endpoints are explicitly
    -- given.
    --
    -- @since 0.1.2.0
    | SEClamped a a
  deriving (Show, Eq, Ord, Generic)

-- | @since 0.1.3.0
instance Bi.Binary a => Bi.Binary (SplineEnd a)

data SplineCoef a = SC { _scAlpha  :: !a      -- ^ a
                       , _scBeta   :: !a      -- ^ b
                       , _scGamma0 :: !a      -- ^ y_{i-1}
                       , _scGamma1 :: !a      -- ^ y_i
                       , _scDelta  :: !a      -- ^ x_i - x_{i-1}
                       }
  deriving Show

-- | 1D Cubic spline
data Spline a = Spline { splineHead :: !(a, SplineCoef a)
                       , splineTail :: !(M.Map a (SplineCoef a))
                       }

runSplineCoef
    :: Fractional a
    => a
    -> SplineCoef a
    -> a
    -> a
runSplineCoef x0 (SC α β γ0 γ1 δ) x = q * γ0
                                    + t * γ1
                                    + t * q * (q * α + t * β)
  where
    t = (x - x0) / δ
    q = 1 - t

-- | Sample a spline at a given point.
sampleSpline
    :: (Fractional a, Ord a)
    => Spline a
    -> a
    -> a
sampleSpline Spline{..} x = case x `M.lookupLE` splineTail of
    Nothing ->
      let (x0, sc) = splineHead
      in  runSplineCoef x0 sc x
    Just (x0, sc) -> runSplineCoef x0 sc x

-- | Build a cubic spline based on control points using given end
-- conditions (not-a-knot, or natural)
--
-- <https://en.wikipedia.org/wiki/Spline_interpolation>
makeSpline
    :: forall a. (Ord a, Fractional a)
    => SplineEnd a
    -> M.Map a a            -- ^ (x, y)
    -> Maybe (Spline a)
makeSpline se ps = SV.withSizedList (M.toList ps) $ \(xsys :: SV.Vector n (a, a)) -> do
      Refl <- Proxy @1 `isLE` Proxy @n
      Refl <- Proxy @2 `isLE` Proxy @n
      let xs, ys :: SV.Vector n a
          (xs, ys) = SV.unzip xsys
          dxs, dys :: SV.Vector (n - 1) a
          dxs = SV.tail xs - SV.init xs
          rdxs :: SV.Vector (n - 1) a
          rdxs = recip dxs
          rdxssq :: SV.Vector (n - 1) a
          rdxssq = rdxs * rdxs
          dys  = SV.tail ys - SV.init ys
          dydxssq = dys * rdxssq
          mainDiag :: SV.Vector (n - 2) a
          mainDiag = SV.zipWith (\rdx0 rdx1 -> 2 * ( rdx0 + rdx1 ))
                        (SV.init rdxs)
                        (SV.tail rdxs)
          lowerDiag :: SV.Vector (n - 2) a
          lowerDiag = SV.take rdxs
          upperDiag :: SV.Vector (n - 2) a
          upperDiag = SV.tail rdxs
          rhs :: SV.Vector (n - 2) a
          rhs = SV.zipWith (\dydxsq0 dydxsq1 -> 3 * (dydxsq0 + dydxsq1))
                        (SV.init dydxssq)
                        (SV.tail dydxssq)
          EE{..} = case se of
            SENotAKnot      -> notAKnot rdxs rdxssq dydxssq
            SENatural       -> natural rdxs dydxssq
            SEClamped c0 c1 -> clamped c0 c1
            -- TODO: perterb if singular
          solution = fromMaybe (errorWithoutStackTrace "Numeric.EMD.Internal.Spline.makeSpline: Splining coefficient matrix is singular") $
            solveTridiagonal (                    lowerDiag `SV.snoc` eeLower1)
                             (eeMain0   `SV.cons` mainDiag  `SV.snoc` eeMain1 )
                             (eeUpper0  `SV.cons` upperDiag                   )
                             (eeRhs0    `SV.cons` rhs       `SV.snoc` eeRhs1  )
          as :: SV.Vector (n - 1) a
          as = SV.zipWith3 (\k dx dy -> k * dx - dy) (SV.init solution) dxs dys
          bs :: SV.Vector (n - 1) a
          bs = SV.zipWith3 (\k dx dy -> - k * dx + dy) (SV.tail solution) dxs dys
          coefs :: SV.Vector (n - 1) (a, SplineCoef a)
          coefs = SV.zipWith6 (\x α β γ0 γ1 δ -> (x, SC α β γ0 γ1 δ))
                    (SV.init xs) as bs (SV.init ys) (SV.tail ys) dxs

      pure Spline
        { splineHead = SV.head coefs
        , splineTail = M.fromAscList . SV.toList . SV.tail $ coefs
        }

data EndEqn a = EE { eeMain0  :: !a
                   , eeUpper0 :: !a
                   , eeLower1 :: !a
                   , eeMain1  :: !a
                   , eeRhs0   :: !a
                   , eeRhs1   :: !a
                   }

natural
    :: (KnownNat n, Num a)
    => SV.Vector (n + 1) a
    -> SV.Vector (n + 1) a
    -> EndEqn a
natural rdxs dydxssq = EE
    { eeMain0  = 2 * (rdxs `SV.index` minBound)
    , eeUpper0 = rdxs `SV.index` minBound
    , eeLower1 = rdxs `SV.index` maxBound
    , eeMain1  = 2 * (rdxs `SV.index` maxBound)
    , eeRhs0   = 3 * (dydxssq `SV.index` minBound)
    , eeRhs1   = 3 * (dydxssq `SV.index` maxBound)
    }

notAKnot
    :: (KnownNat n, Num a)
    => SV.Vector (n + 1) a
    -> SV.Vector (n + 1) a
    -> SV.Vector (n + 1) a
    -> EndEqn a
notAKnot rdxs rdxssq dydxssq = EE
    { eeMain0  = rdxssq `SV.index` minBound + rdx12Upper
    , eeUpper0 = rdxssq `SV.index` minBound
               + rdxssq `SV.index` shift minBound
               + 2 * rdx12Upper
    , eeLower1 = - (rdxssq `SV.index` weaken maxBound)
               - (rdxssq `SV.index` maxBound)
               - 2 * rdx12Lower
    , eeMain1  = - rdxssq `SV.index` maxBound - rdx12Lower
    , eeRhs0   = 2 * (dydxssq `SV.index` minBound) * (rdxs `SV.index` minBound)
               + 3 * (dydxssq `SV.index` minBound) * (rdxs `SV.index` shift minBound)
               + (dydxssq `SV.index` shift minBound) * (rdxs `SV.index` shift minBound)
    , eeRhs1   = - (dydxssq `SV.index` weaken maxBound) * (rdxs `SV.index` weaken maxBound)
               - 3 * (dydxssq `SV.index` maxBound) * (rdxs `SV.index` weaken maxBound)
               - 2 * (dydxssq `SV.index` maxBound) * (rdxs `SV.index` maxBound)
    }
  where
    rdx12Upper = rdxs `SV.index` minBound * rdxs `SV.index` shift minBound
    rdx12Lower = rdxs `SV.index` maxBound * rdxs `SV.index` weaken maxBound

clamped
    :: Num a
    => a            -- ^ derivative at left end
    -> a            -- ^ derivative at right end
    -> EndEqn a
clamped c0 c1 = EE
    { eeMain0  = 1
    , eeUpper0 = 0
    , eeLower1 = 0
    , eeMain1  = 1
    , eeRhs0   = c0
    , eeRhs1   = c1
    }
