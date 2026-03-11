-----------------------------------------------------------------------------
-- |
-- Module      :  Data.RNG
-- Copyright   :  Soostone Inc, Snap Framework Authors
-- License     :  BSD3
--
-- Maintainer  :  Ozgun Ataman <ozgun.ataman@soostone.com>
-- Stability   :  experimental
--
-- Convenience thread-safe wrapper around mwc-random library for
-- practical supply of random numbers in a concurrent environment.
----------------------------------------------------------------------------

module Data.RNG
    ( RNG
    , mkRNG
    , seedRNG
    , packRNG
    , withRNG
    , randomToken
    , rngRIO
    , rngIO

    -- * Re-export Random for convenience.
    , module System.Random
    ) where

------------------------------------------------------------------------------
import           Data.IORef
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.Tuple            (swap)
import           Numeric
import           System.Random
-------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | Mutable random number generator state
newtype RNG = RNG (IORef StdGen)

------------------------------------------------------------------------------
-- | Perform given action, mutating the RNG state underneath.
withRNG :: RNG
        -> (StdGen -> (a, StdGen))
        -> IO a
withRNG (RNG rng) f = atomicModifyIORef' rng (swap . f)


-------------------------------------------------------------------------------
-- | Generate a random value from a range.
rngRIO
  :: (Random a)
  => RNG
  -> (a, a)
  -- Range of values
  -> IO a
rngRIO rng range = withRNG rng (randomR range)


-------------------------------------------------------------------------------
-- | Generate a random value.
rngIO
  :: (Random a)
  => RNG
  -> IO a
rngIO rng = withRNG rng random


------------------------------------------------------------------------------
-- | Create a new RNG in the IO monad using 'newStdGen'. Splits the
-- global random number generator, so 2 calls to 'mkRNG' will produce
-- different RNGs.
mkRNG :: IO RNG
mkRNG = packRNG =<< newStdGen


------------------------------------------------------------------------------
-- | Create a new RNG with a user-specified seed.
seedRNG :: Int -> IO RNG
seedRNG = packRNG . mkStdGen


-------------------------------------------------------------------------------
-- | Pack your own rng into the 'RNG' type.
packRNG :: StdGen -> IO RNG
packRNG = fmap RNG . newIORef


------------------------------------------------------------------------------
-- | Generates a random salt of given length
randomToken :: Int -> RNG -> IO ByteString
randomToken len rng = do
  is <- withRNG rng $ \gen -> go len ([], gen)
  return . B.pack . concat . map (flip showHex "") $ is
  where
    hexDigit :: StdGen -> (Int, StdGen)
    hexDigit = randomR (0,15)
    go :: Int -> ([Int], StdGen) -> ([Int], StdGen)
    go n acc@(is, gen)
      | n <= 0 = acc
      | otherwise = let (i, gen2) = hexDigit gen
                    in go (n - 1) (i:is, gen2)
