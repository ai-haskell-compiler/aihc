-------------------------------------------------------------------------------
-- time-units-types
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTSyntax #-}

-- | Exports types which can be used to describe time periods at
-- the type-level. Use the `durationVal` function to reify them as the
-- corresponding value-level descriptions from "Data.Time.Units" or the
-- `durationMicroseconds` function to reify them as microseconds straight away.
module Data.Time.TypeLevel (
    KnownDuration(..),
    durationMicroseconds,

    TimePeriod(..)
) where

-------------------------------------------------------------------------------

import GHC.TypeLits

import qualified Data.Time.Units as Units
import Data.Proxy
import Data.Kind

-------------------------------------------------------------------------------

data TimePeriod where
    -- | Represents @n@-many attoseconds.
    Attosecond :: Nat -> TimePeriod

    -- | Represents @n@-many femtoseconds.
    Femtosecond :: Nat -> TimePeriod

    -- | Represents @n@-many picoseconds.
    Picosecond :: Nat -> TimePeriod

    -- | Represents @n@-many nanoseconds.
    Nanosecond :: Nat -> TimePeriod

    -- | Represents @n@-many microseconds.
    Microsecond :: Nat -> TimePeriod

    -- | Represents @n@-many milliseconds.
    Millisecond :: Nat -> TimePeriod

    -- | Represents @n@-many seconds.
    Second :: Nat -> TimePeriod

    -- | Represents @n@-many minutes.
    Minute :: Nat -> TimePeriod

    -- | Represents @n@-many hours.
    Hour :: Nat -> TimePeriod

    -- | Represents @n@-many days.
    Day :: Nat -> TimePeriod

    -- | Represents @n@-many weeks.
    Week :: Nat -> TimePeriod

    -- | Represents @n@-many fortnights.
    Fortnight :: Nat -> TimePeriod

-- | A class of types which can be reified as value-level descriptions of
-- time periods.
class KnownDuration k where
    -- | The type representing value-level descriptions of the time period
    -- corresponding to @k@.
    type DurationUnit k :: Type

    -- | `durationVal` reifies the duration as the corresponding value-level
    -- type. Intended to be used with @TypeApplications@.
    --
    -- >>> durationVal @('Second 10)
    -- 10s :: Data.Time.Units.Second
    durationVal :: DurationUnit k

instance KnownNat n => KnownDuration ('Attosecond n) where
    type DurationUnit ('Attosecond n) = Units.Attosecond

    durationVal =
        fromInteger @Units.Attosecond $
        natVal (Proxy :: Proxy n)

-- | `durationMicroseconds` is a convenience function which reifies a
-- type-level duration as microseconds on the value level. Intended to be
-- used with @TypeApplications@.
--
-- >>> durationMicroseconds @('Second 10)
-- 10000000 :: Integer
durationMicroseconds
    :: forall d . (KnownDuration d, Units.TimeUnit (DurationUnit d))
    => Integer
durationMicroseconds = Units.toMicroseconds $ durationVal @d

instance KnownNat n => KnownDuration ('Femtosecond n) where
    type DurationUnit ('Femtosecond n) = Units.Femtosecond

    durationVal =
        fromInteger @Units.Femtosecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Picosecond n) where
    type DurationUnit ('Picosecond n) = Units.Picosecond

    durationVal =
        fromInteger @Units.Picosecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Nanosecond n) where
    type DurationUnit ('Nanosecond n) = Units.Nanosecond

    durationVal =
        fromInteger @Units.Nanosecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Microsecond n) where
    type DurationUnit ('Microsecond n) = Units.Microsecond

    durationVal =
        fromInteger @Units.Microsecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Millisecond n) where
    type DurationUnit ('Millisecond n) = Units.Millisecond

    durationVal =
        fromInteger @Units.Millisecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Second n) where
    type DurationUnit ('Second n) = Units.Second

    durationVal =
        fromInteger @Units.Second $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Minute n) where
    type DurationUnit ('Minute n) = Units.Minute

    durationVal =
        fromInteger @Units.Minute $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Hour n) where
    type DurationUnit ('Hour n) = Units.Hour

    durationVal =
        fromInteger @Units.Hour $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Day n) where
    type DurationUnit ('Day n) = Units.Day

    durationVal =
        fromInteger @Units.Day $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Week n) where
    type DurationUnit ('Week n) = Units.Week

    durationVal =
        fromInteger @Units.Week $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration ('Fortnight n) where
    type DurationUnit ('Fortnight n) = Units.Fortnight

    durationVal =
        fromInteger @Units.Fortnight $
        natVal (Proxy :: Proxy n)

-------------------------------------------------------------------------------
