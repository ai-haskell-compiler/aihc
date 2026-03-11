{-|
Module      : Linear.Geo.PlaneAngle
Copyright   : Travis Whitaker 2023
License     : MIT
Maintainer  : pi.boy.travis@gmail.com
Stability   : Provisional
Portability : Portable (Windows, POSIX)

Types for dealing with different representations of angles in the plane.

-}

{-# LANGUAGE BangPatterns
           , DeriveAnyClass
           , DeriveDataTypeable
           , DeriveFunctor
           , DeriveGeneric
           , DerivingStrategies
           , GeneralizedNewtypeDeriving
           #-}

module Linear.Geo.PlaneAngle (
    PlaneAngle(..)
  , Radians(..)
  , Degrees(..)
  , DMS(..)
  , dmsToDegrees
  , degreesToDMS
  , DM(..)
  , dmToDegrees
  , degreesToDM
  ) where

import Control.Applicative

import Control.DeepSeq (NFData)

import Control.Monad.Fix
import Control.Monad.Zip

import Data.Coerce

import Data.Data (Data)

import Data.Distributive

import Data.Fixed (divMod', mod')

import GHC.Generics (Generic)

-- | Plane angles.
class PlaneAngle ang where
    --  | Put the angle into the canonical range 0 to 2*pi.
    normalizeAngle :: (Floating a, Real a) => ang a -> ang a
    -- | Convert the angle to radians.
    toRadians      :: (Floating a, Real a) => ang a -> Radians a
    -- | Convert the angle from radians.
    fromRadians    :: (Floating a, Real a) => Radians a -> ang a

-- | A quantity representing a plane angle that satisfies the equation
--   @S = r * a@ where @r@ is the radius of a circle, @a@ is the measure of some
--   angle subtending the circle, and @S@ is the length of the subtended arc.
newtype Radians a = Radians a
                  deriving stock ( Eq
                                 , Ord
                                 , Show
                                 , Generic
                                 , Data
                                 , Bounded
                                 , Functor
                                 )
                  deriving newtype ( Num
                                   , Fractional
                                   , Floating
                                   , Real
                                   , RealFrac
                                   , RealFloat
                                   , NFData
                                   )

instance Applicative Radians where
    pure = coerce
    Radians f <*> Radians x = Radians (f x)

instance Monad Radians where
    return = pure
    Radians x >>= f = f x

instance MonadZip Radians where
    mzipWith = liftA2

instance MonadFix Radians where
    mfix f = Radians (let Radians x = f x in x)

instance Foldable Radians where
    foldMap f (Radians x) = f x

instance Traversable Radians where
    traverse f (Radians x) = Radians <$> f x

instance Distributive Radians where
    distribute f = Radians (fmap coerce f)

instance PlaneAngle Radians where
    normalizeAngle = coerce . (`mod'` (2 * pi))
    toRadians = id
    fromRadians = id
    {-# INLINEABLE normalizeAngle #-}
    {-# INLINEABLE toRadians #-}
    {-# INLINEABLE fromRadians #-}

-- | One degree is @pi / 180@ radians.
newtype Degrees a = Degrees a
                  deriving stock ( Eq
                                 , Ord
                                 , Show
                                 , Generic
                                 , Data
                                 , Bounded
                                 , Functor
                                 )
                  deriving newtype ( Num
                                   , Fractional
                                   , Floating
                                   , Real
                                   , RealFrac
                                   , RealFloat
                                   , NFData
                                   )

instance Applicative Degrees where
    pure = coerce
    Degrees f <*> Degrees x = Degrees (f x)

instance Monad Degrees where
    return = pure
    Degrees x >>= f = f x

instance MonadZip Degrees where
    mzipWith = liftA2

instance MonadFix Degrees where
    mfix f = Degrees (let Degrees x = f x in x)

instance Foldable Degrees where
    foldMap f (Degrees x) = f x

instance Traversable Degrees where
    traverse f (Degrees x) = Degrees <$> f x

instance Distributive Degrees where
    distribute f = Degrees (fmap coerce f)

instance PlaneAngle Degrees where
    normalizeAngle = coerce . (`mod'` 360)
    toRadians (Degrees d) = Radians (pi * (d / 180))
    fromRadians (Radians r) = Degrees ((r / pi) * 180)
    {-# INLINEABLE normalizeAngle #-}
    {-# INLINEABLE toRadians #-}
    {-# INLINEABLE fromRadians #-}

-- | An angle represented as degrees, minutes, and seconds of arc.
data DMS a = DMS {
   dmsDeg :: !a
 , dmsMin :: !a
 , dmsSec :: !a
 } deriving stock ( Eq
                  , Ord
                  , Show
                  , Generic
                  , Data
                  , Bounded
                  , Functor
                  )
   deriving anyclass (NFData)

instance Applicative DMS where
    pure x = DMS x x x
    (DMS df mf sf) <*> (DMS d m s) = DMS (df d) (mf m) (sf s)

instance Monad DMS where
    return = pure
    (DMS d m s) >>= f = let DMS d' _ _ = f d
                            DMS _ m' _ = f m
                            DMS _ _ s' = f s
                        in DMS d' m' s'

instance MonadZip DMS where
    mzipWith = liftA2

instance MonadFix DMS where
    mfix f = DMS (let DMS d _ _ = f d in d)
                 (let DMS _ m _ = f m in m)
                 (let DMS _ _ s = f s in s)

instance Foldable DMS where
    foldMap f (DMS d m s) = f d <> f m <> f s

instance Traversable DMS where
    traverse f (DMS d m s) = DMS <$> f d <*> f m <*> f s

instance Distributive DMS where
    distribute f = DMS (fmap (\(DMS d _ _) -> d) f)
                       (fmap (\(DMS _ m _) -> m) f)
                       (fmap (\(DMS _ _ s) -> s) f)

-- | Convert DMS to Degrees. This does not normalize the angle.
dmsToDegrees :: Fractional a => DMS a -> Degrees a
dmsToDegrees (DMS d m s) = Degrees (d + (m * (1 / 60)) + (s * (1 / 3600)))
{-# INLINEABLE dmsToDegrees #-}

-- | Convert degrees to DMS. This does not normalize the angle.
degreesToDMS :: (Real a, Fractional a) => Degrees a -> DMS a
degreesToDMS (Degrees d) =
    let (dint, dleft) = divMod' d 1
        (mint, mleft) = divMod' dleft (1 / 60)
        sleft         = mleft / (1 / 3600)
    in DMS (fromIntegral dint) (fromIntegral mint) sleft
{-# INLINEABLE degreesToDMS #-}

instance PlaneAngle DMS where
    normalizeAngle = degreesToDMS . normalizeAngle . dmsToDegrees
    toRadians      = toRadians . dmsToDegrees
    fromRadians    = degreesToDMS . fromRadians
    {-# INLINEABLE normalizeAngle #-}
    {-# INLINEABLE toRadians #-}
    {-# INLINEABLE fromRadians #-}

-- | An angle represented as degrees and minutes of arc.
data DM a = DM {
    dmDeg :: !a
  , dmMin :: !a
  } deriving stock ( Eq
                   , Ord
                   , Show
                   , Generic
                   , Data
                   , Bounded
                   , Functor
                   )
    deriving anyclass (NFData)

instance Applicative DM where
    pure x = DM x x
    (DM df mf) <*> (DM d m) = DM (df d) (mf m)

instance Monad DM where
    return = pure
    (DM d m) >>= f = let DM d' _ = f d
                         DM _ m' = f m
                     in DM d' m'

instance MonadZip DM where
    mzipWith = liftA2

instance MonadFix DM where
    mfix f = DM (let DM d _ = f d in d)
                (let DM _ m = f m in m)

instance Foldable DM where
    foldMap f (DM d m) = f d <> f m

instance Traversable DM where
    traverse f (DM d m) = DM <$> f d <*> f m

instance Distributive DM where
    distribute f = DM (fmap (\(DM d _) -> d) f)
                      (fmap (\(DM _ m) -> m) f)

-- | Convert DM to degrees. This does not normalize the angle.
dmToDegrees :: Fractional a => DM a -> Degrees a
dmToDegrees (DM d m) = Degrees (d + (m * (1 / 60)))
{-# INLINEABLE dmToDegrees #-}

-- | Convert degrees to DM. This does not normalize the angle.
degreesToDM :: (Fractional a, Real a) => Degrees a -> DM a
degreesToDM (Degrees d) =
    let (dint, m) = divMod' d 1
    in DM (fromIntegral dint) (m / (1 / 60))
{-# INLINEABLE degreesToDM #-}

instance PlaneAngle DM where
    normalizeAngle = degreesToDM . normalizeAngle . dmToDegrees
    toRadians      = toRadians . dmToDegrees
    fromRadians    = degreesToDM . fromRadians
    {-# INLINEABLE normalizeAngle #-}
    {-# INLINEABLE toRadians #-}
    {-# INLINEABLE fromRadians #-}
