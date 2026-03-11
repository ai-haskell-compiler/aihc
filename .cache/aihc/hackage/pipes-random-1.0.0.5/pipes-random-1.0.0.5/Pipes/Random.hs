-- |
-- Module    : Pipes.Random
-- Copyright : (c) Colin Woodbury, 2016
-- License   : BSD3
-- Maintainer: Colin Woodbury <cwoodbury@azavea.com>
--
-- Producers for handling randomness.
--
-- == Random Numbers
-- Use functions like `uniform` and `normal` to generate endless streams
-- of random numbers of the standard `Num` types. For instance, you could
-- perform some IO action based on a threshold:
--
-- > {-# LANGUAGE TypeApplications #-}  -- GHC8 only. Provides the @ syntax.
-- >
-- > import qualified Pipes.Prelude as P
-- >
-- > perhaps :: Effect IO ()
-- > perhaps = uniform @Float >-> P.filter (> 0.1) >-> lift releaseTheHounds
--
-- == Random Elements from Containers
-- We expose the functions `finite` and `endless` for randomly Producing
-- elements from a collection.
--
-- `finite` will only Produce until each of its elements have been yielded once.
-- Making a shuffle function then is easy:
--
-- > import           Data.Vector (Vector)
-- > import qualified Pipes.Prelude as P
-- >
-- > shuffle :: Vector a -> IO [a]
-- > shuffle = P.toListM . finite
--
-- `endless` on the other hand will endlessly Produce elements in any order.
-- Repeats will likely appear long before each element has been yielded once.
-- You can limit the number of results with `P.take`:
--
-- > import           Data.Vector (Vector)
-- > import qualified Pipes.Prelude as P
-- >
-- > twenty :: Vector a -> Producer a IO ()
-- > twenty v = endless v >-> P.take 20
--
-- For the time being, only `V.Vector`s (all kinds) are supported.

module Pipes.Random
  ( -- * Pseudo-random Number Generators
    pool
    -- * Numbers
  , uniform
  , uniformR
  , normal
    -- * Collections
  , finite
  , endless
  ) where

import           Control.Monad (forever)
import qualified Data.Vector.Generic as V
import           Pipes
import qualified Pipes.Prelude as P
import qualified System.Random.MWC as R
import qualified System.Random.MWC.Distributions as R

---

-- | A pseudo-random number generator, produced using system randomness.
--
-- `lift` this to transform it into a `Producer`.
pool :: IO R.GenIO
pool = R.createSystemRandom

-- | Endlessly produce anything that's `R.Variate` from a uniform distribution.
--
-- * For integral types, the entire bit range is used.
-- * For floating types, the range is (0,1], where 0 is specifically excluded.
uniform :: R.Variate v => Producer v IO ()
uniform = lift pool >>= \g -> forever (lift (R.uniform g) >>= yield)

-- | Endlessly produce anything that's `R.Variate` from a uniform distribution,
-- within some given range of values.
--
-- * For integral types, inclusive range is used.
-- * For floating types, (a,b] is used.
uniformR :: R.Variate v => (v, v) -> Producer v IO ()
uniformR r = lift pool >>= \g -> forever (lift (R.uniformR r g) >>= yield)

-- | Given a mean and a standard deviation, endlessly produce values from
-- a normal (Gaussian) distribution.
normal :: Double -> Double -> Producer Double IO ()
normal m sd = lift pool >>= \g -> forever (lift (R.normal m sd g) >>= yield)

-- | Given some `V.Vector`, produce its elements in a random order, once each.
--
-- > >>> P.toListM $ finite (V.fromList @Data.Vector.Vector ['a'..'z'])
-- > "rkzpnwjfeqotvdlsaxiuhcbymg"
finite :: V.Vector v a => v a -> Producer a IO ()
finite l = lift pool >>= f l
  where f v g | V.null v = pure ()
              | otherwise = do
                  i <- lift $ R.uniformR (0, V.length v - 1) g
                  let v' = swap 0 i v
                  yield $ V.unsafeHead v'  -- It won't be empty.
                  f (V.tail v') g

-- | Swap two positions in a given `V.Vector`. Doesn't check bounds.
swap :: V.Vector v a => Int -> Int -> v a -> v a
swap i j v = v V.// [(i, V.unsafeIndex v j), (j, V.unsafeIndex v i)]

-- | Given some `V.Vector`, endlessly produce elements from it.
--
-- > >>> P.toListM $ endless (V.fromList @Data.Vector.Vector ['a'..'z']) >-> P.take 20
-- > "nvecotyjhestgrrlganj"
endless :: V.Vector v a => v a -> Producer a IO ()
endless v | V.null v = pure ()
          | otherwise = uniformR (0, V.length v - 1) >-> P.map (V.unsafeIndex v)
