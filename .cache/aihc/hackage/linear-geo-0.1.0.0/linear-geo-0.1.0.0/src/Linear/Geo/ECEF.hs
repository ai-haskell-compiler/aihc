{-|
Module      : Linear.Geo.ECEF
Copyright   : Travis Whitaker 2023
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

Earth-centered Earth-fixed (ECEF) coordinates.

-}

{-# LANGUAGE DataKinds
           , DeriveDataTypeable
           , DeriveGeneric
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           , TypeFamilies
           #-}

module Linear.Geo.ECEF (
    ECEF(..)
  , cross
  , triple
  ) where

import Control.DeepSeq (NFData)

import Control.Monad.Fix (MonadFix)
import Control.Monad.Zip (MonadZip)

import Data.Coerce

import Data.Data (Data)

import Data.Distributive

import qualified Data.Vector as V

import GHC.Generics (Generic)

import qualified Linear.Affine  as L
import qualified Linear.Epsilon as L
import qualified Linear.Matrix  as L
import qualified Linear.Metric  as L
import qualified Linear.V       as L
import qualified Linear.V2      as L
import qualified Linear.V3      as L
import qualified Linear.Vector  as L

-- | R3 vector with the origin at the Earth's center of mass, first basis vector
--   through the intersection of the prime meridian and the equator, and the
--   third basis vector through True North. The origin and basis vectors move
--   and rotate with the Earth through space.
newtype ECEF a = ECEF (L.V3 a)
             deriving stock ( Eq
                            , Ord
                            , Show
                            , Generic
                            , Data
                            , Bounded
                            )
             deriving newtype ( Num
                              , Fractional
                              , Floating
                              , Functor
                              , Applicative
                              , Monad
                              , MonadFix
                              , MonadZip
                              , Foldable
                              , L.Additive
                              , L.Metric
                              , L.Trace
                              , L.Epsilon
                              , NFData
                              )

instance Traversable ECEF where
    traverse f ecef = traverse f (coerce ecef)

instance Distributive ECEF where
    distribute f = ECEF $ L.V3 (fmap (\(ECEF (L.V3 x _ _)) -> x) f)
                               (fmap (\(ECEF (L.V3 _ y _)) -> y) f)
                               (fmap (\(ECEF (L.V3 _ _ z)) -> z) f)

instance L.Finite ECEF where
    type Size ECEF = 3
    toV (ECEF (L.V3 x y z)) = L.V (V.fromListN 3 [x, y, z])
    fromV (L.V v)           = ECEF $ L.V3 (v V.! 0) (v V.! 1) (v V.! 2)

instance L.R1 ECEF where
    _x f (ECEF (L.V3 x y z)) = (\x' -> ECEF (L.V3 x' y z)) <$> f x

instance L.R2 ECEF where
    _y  f (ECEF (L.V3 x y z)) = (\y' -> ECEF (L.V3 x y' z)) <$> f y
    _xy f (ECEF (L.V3 x y z)) = (\(L.V2 x' y') -> ECEF (L.V3 x' y' z))
                            <$> f (L.V2 x y)

instance L.R3 ECEF where
    _z   f (ECEF (L.V3 x y z)) = (\z' -> ECEF (L.V3 x y z')) <$> f z
    _xyz f (ECEF v)            = ECEF <$> f v

instance L.Affine ECEF where
    type Diff ECEF = L.V3
    (ECEF x) .-. (ECEF y) = x L..-. y
    (ECEF x) .+^ y        = ECEF (x L..+^ y)
    (ECEF x) .-^ y        = ECEF (x L..-^ y)

-- | Right-handed orthogonal vector with magnitude equal to the area of the
--   subtended parallelogram.
cross :: Num a => ECEF a -> ECEF a -> ECEF a
cross x y = ECEF $ L.cross (coerce x) (coerce y)

-- | Scalar triple product.
triple :: Num a => ECEF a -> ECEF a -> ECEF a -> a
triple x y z = L.triple (coerce x) (coerce y) (coerce z)
