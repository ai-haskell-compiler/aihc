{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-|
Copyright   : (c) Ryota Kameoka, 2018
License     : BSD-3
Maintainer  : kameoka.ryota@gmail.com
Stability   : experimental

This module exports internally used type classes and data types.
You can extend this package's functionality by utilizing them.
-}

module Data.Time.Clock.Duration.Types
    (
    -- * Type classes
      AbsoluteDuration (..)
    , RelativeDuration (..)
    -- * Data types
    , Time (..)
    ) where

import Data.Int (Int8, Int16, Int32, Int64)
import Data.Fixed (E6, E12, Fixed, HasResolution (resolution))
import Data.Proxy (Proxy (Proxy))
import Data.Ratio (Ratio)
import Data.Time.Clock (DiffTime, NominalDiffTime, picosecondsToDiffTime)
import Foreign.C.Types (CSUSeconds (CSUSeconds), CUSeconds (CUSeconds))
import Language.Haskell.TH.Syntax (Lift)

-- $setup
-- >>> :set -XQuasiQuotes
-- >>> import Data.Time.Clock.Duration.QQ

-- | The 'AbsoluteDuration' class provides how to convert the given 'Time' into a specific unit of time.
-- An instance should represent a quantity with
-- <https://en.wikipedia.org/wiki/Time_in_physics the dimension of T>.
--
-- 42 seconds in 'DiffTime' (seconds):
--
-- >>> [t| 42s |] :: DiffTime
-- 42s
--
-- 42 seconds in 'CUSeconds' (microseconds):
--
-- >>> [t| 42s |] :: CUSeconds
-- 42000000
class AbsoluteDuration a where
    toAbsoluteDuration :: Time -> a

instance AbsoluteDuration DiffTime where
    toAbsoluteDuration = picosecondsToDiffTime . round . inPsScale . toSeconds

instance AbsoluteDuration NominalDiffTime where
    toAbsoluteDuration = realToFrac . toAbsoluteDuration @DiffTime

-- | /Caution:/ the fractional part will be rounded.
instance AbsoluteDuration CUSeconds where
    toAbsoluteDuration = CUSeconds . round . inµsScale . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance AbsoluteDuration CSUSeconds where
    toAbsoluteDuration = CSUSeconds . round . inµsScale . toSeconds

-- | The 'RelativeDuration' class represents how to calculate how long the given 'Time' is when
-- measured in a specific unit of time.
-- An instance should represent a quantity with
-- <https://en.wikipedia.org/wiki/Dimensionless_quantity the dimension of 1>.
--
-- 42 minutes in seconds:
--
-- >>> [s| 42m |] :: Int
-- 2520
--
-- 3 seconds in microseconds:
--
-- >>> [µs| 3s |] :: Int
-- 3000000
class RelativeDuration a where
    toRelativeDuration :: HasResolution r => Proxy r -> Time -> a

-- | /Caution:/ the fractional part will be rounded.
instance RelativeDuration Int where
    toRelativeDuration proxy = round . convertScale proxy . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance RelativeDuration Int8 where
    toRelativeDuration proxy = round . convertScale proxy . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance RelativeDuration Int16 where
    toRelativeDuration proxy = round . convertScale proxy . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance RelativeDuration Int32 where
    toRelativeDuration proxy = round . convertScale proxy . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance RelativeDuration Int64 where
    toRelativeDuration proxy = round . convertScale proxy . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance RelativeDuration Integer where
    toRelativeDuration proxy = round . convertScale proxy . toSeconds

-- | /Caution:/ the fractional part will be rounded.
instance HasResolution a => RelativeDuration (Fixed a) where
    toRelativeDuration proxy = realToFrac . convertScale proxy . toSeconds

instance Integral a => RelativeDuration (Ratio a) where
    toRelativeDuration proxy = realToFrac . convertScale proxy . toSeconds

instance RelativeDuration Float where
    toRelativeDuration proxy = realToFrac . convertScale proxy . toSeconds

instance RelativeDuration Double where
    toRelativeDuration proxy = realToFrac . convertScale proxy . toSeconds

-- | The parsing result of a string inside a quasiquoter.
data Time
    = Picosec  Rational
    | Nanosec  Rational
    | Microsec Rational
    | Millisec Rational -- ^ Denoted by @ms@, @msec@, @msecs@, @millisecond@, or @milliseconds@
    | Second   Rational -- ^ Denoted by @s@, @sec@, @secs@, @second@, or @seconds@
    | Minute   Rational -- ^ Denoted by @m@, @min@, @mins@, @minute@, or @minutes@
    | Hour     Rational -- ^ Denoted by @h@, @hr@, @hrs@, @hour@, or @hours@
    | Day      Rational -- ^ Denoted by @d@, @day@, or @days@
    | Week     Rational -- ^ Denoted by @w@, @week@, or @weeks@
    | Year     Rational -- ^ Denoted by @y@, @yr@, @yrs@, @year@, or @years@
    deriving (Lift)

toSeconds :: Time -> Rational
toSeconds (Picosec  x) = x / 1000000000000
toSeconds (Nanosec  x) = x / 1000000000
toSeconds (Microsec x) = x / 1000000
toSeconds (Millisec x) = x / 1000
toSeconds (Second   x) = x
toSeconds (Minute   x) = x * 60
toSeconds (Hour     x) = x * 60 * 60
toSeconds (Day      x) = x * 60 * 60 * 24
toSeconds (Week     x) = x * 60 * 60 * 24 * 7
toSeconds (Year     x) = x * 60 * 60 * 24 * 365

convertScale :: forall r. (HasResolution r) => Proxy r -> Rational -> Rational
convertScale _ = (* fromIntegral (resolution (0 :: Fixed r)))

inPsScale :: Rational -> Rational
inPsScale = convertScale (Proxy :: Proxy E12)

inµsScale :: Rational -> Rational
inµsScale = convertScale (Proxy :: Proxy E6)
