module Sound.ALSA.Sequencer.RealTime
  ( RealTime.T(..)
  , fromDouble, fromFractional, fromInteger
  , toDouble, toFractional, toInteger
  ) where

import qualified Sound.ALSA.Sequencer.Marshal.RealTime as RealTime

import Prelude hiding (fromInteger, toInteger, )


nanoPerSecond :: Num a => a
nanoPerSecond = 10^(9::Int)

-- | Convert number of nanoseconds to 'RealTime.T'
fromInteger :: Integer -> RealTime.T
fromInteger t =
   let (s,n) = divMod t nanoPerSecond
   in  RealTime.Cons (fromIntegral s) (fromIntegral n)

{- |
Convert fractional number of seconds to 'RealTime.T'
Time must be non-negative.
-}
fromDouble :: Double -> RealTime.T
fromDouble = fromFractional

fromFractional :: (RealFrac a) => a -> RealTime.T
fromFractional t =
   let (s,n) = properFraction t
   in  RealTime.Cons s (floor $ n * nanoPerSecond)

-- | Convert number of nanoseconds to 'RealTime.T'
toInteger :: RealTime.T -> Integer
toInteger (RealTime.Cons s n) =
   fromIntegral s * nanoPerSecond + fromIntegral n

-- | Convert fractional number of seconds to 'RealTime.T'
toDouble :: RealTime.T -> Double
toDouble = toFractional

toFractional :: (RealFrac a) => RealTime.T -> a
toFractional (RealTime.Cons s n) =
   fromIntegral s + fromIntegral n / nanoPerSecond
