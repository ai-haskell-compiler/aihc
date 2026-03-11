-- | List shuffling and sampling with optimal asymptotic time and space complexity using the imperative Fisher–Yates
-- algorithm.
module List.Shuffle
  ( -- * Shuffling
    shuffle,
    shuffle_,
    shuffleIO,

    -- * Sampling
    sample,
    sample_,
    sampleIO,

    -- * Adapting to other monads

    -- ** Reader monad
    -- $example-reader

    -- ** State monad
    -- $example-state
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.ST (runST)
import Control.Monad.ST.Strict (ST)
import Data.Foldable qualified as Foldable
import Data.Primitive.Array qualified as Array
import System.Random (RandomGen)
import System.Random qualified as Random

-- $example-reader
--
-- You are working in a reader monad, with access to a pseudo-random number generator somewhere in the environment,
-- in a mutable cell like an @IORef@ or @TVar@:
--
-- > import System.Random qualified as Random
-- > import System.Random.Stateful qualified as Random
-- >
-- > data MyMonad a
-- >
-- > instance MonadIO MyMonad
-- > instance MonadReader MyEnv MyMonad
-- >
-- > data MyEnv = MyEnv
-- >   { ...
-- >   , prng :: Random.AtomicGenM Random.StdGen
-- >   , ...
-- >   }
--
-- In this case, you can adapt 'shuffle' to work in your monad as follows:
--
-- > import List.Shuffle qualified as List
-- > import System.Random qualified as Random
-- >
-- > shuffleList :: [a] -> MyMonad [a]
-- > shuffleList list = do
-- >   MyEnv {prng} <- ask
-- >   Random.applyAtomicGen (List.shuffle list) prng

-- $example-state
--
-- You are working in a state monad with access to a pseudo-random number generator somewhere in the state type. You
-- also have a lens onto this field, which is commonly either provided by @generic-lens@/@optics@ or written manually:
--
-- > import System.Random qualified as Random
-- >
-- > data MyState = MyState
-- >   { ...
-- >   , prng :: Random.StdGen
-- >   , ...
-- >   }
-- >
-- > prngLens :: Lens' MyState Random.StdGen
--
-- In this case, you can adapt 'shuffle' to work in your monad as follows:
--
-- > import Control.Lens qualified as Lens
-- > import Control.Monad.Trans.State.Strict qualified as State
-- > import List.Shuffle qualified as List
-- >
-- > shuffleList :: Monad m => [a] -> StateT MyState m [a]
-- > shuffleList =
-- >   Lens.zoom prngLens . State.state . List.shuffle

-- | \(\mathcal{O}(n)\). Shuffle a list.
shuffle :: (RandomGen g) => [a] -> g -> ([a], g)
shuffle list gen0 =
  runST do
    array <- listToMutableArray list
    gen1 <- shuffleN (Array.sizeofMutableArray array - 1) array gen0
    array1 <- Array.unsafeFreezeArray array
    pure (Foldable.toList array1, gen1)
{-# SPECIALIZE shuffle :: [a] -> Random.StdGen -> ([a], Random.StdGen) #-}

-- `shuffleN n array g` shuffles the first `n` elements of `array`, i.e. it performs the Fisher-Yates algorithm, but
-- stopping after `n` elements, effectively leaving those `n` elements at the head of the array "shuffled" and the rest
-- in some random indeterminate order.
--
-- Call `len` the length of the array minus 1. When `n` is the len, the whole array gets shuffled, as shuffling `n-1` of
-- `n` elements is equivalent to shuffling all `n` elements.
--
-- It's fine to pass nonsense values for `n` - negative numbers are equivalent to 0, and numbers larger than `len` are
-- equivalent to `len`.
shuffleN :: forall a g s. (RandomGen g) => Int -> Array.MutableArray s a -> g -> ST s g
shuffleN n0 array =
  go 0
  where
    go :: Int -> g -> ST s g
    go !i gen0
      | i >= n = pure gen0
      | otherwise = do
          let (j, gen1) = Random.uniformR (i, m) gen0
          swapArrayElems i j array
          go (i + 1) gen1

    n = min n0 m
    m = Array.sizeofMutableArray array - 1
{-# SPECIALIZE shuffleN :: Int -> Array.MutableArray s a -> Random.StdGen -> ST s Random.StdGen #-}

-- | \(\mathcal{O}(n)\). Like 'shuffle', but discards the final generator.
shuffle_ :: (RandomGen g) => [a] -> g -> [a]
shuffle_ list g =
  fst (shuffle list g)
{-# SPECIALIZE shuffle_ :: [a] -> Random.StdGen -> [a] #-}

-- | \(\mathcal{O}(n)\). Like 'shuffle', but uses the global random number generator.
shuffleIO :: (MonadIO m) => [a] -> m [a]
shuffleIO list =
  shuffle_ list <$> Random.newStdGen
{-# SPECIALIZE shuffleIO :: [a] -> IO [a] #-}

-- | \(\mathcal{O}(n)\). Sample elements of a list, without replacement.
--
-- @sample_ c xs@ is equivalent to @take c . shuffle_ xs@, but with a constant factor that is proportional to @c@, not
-- the length of @xs@.
sample :: (RandomGen g) => Int -> [a] -> g -> ([a], g)
sample n list gen0 =
  runST do
    array <- listToMutableArray list
    gen1 <- shuffleN n array gen0
    array1 <- Array.unsafeFreezeArray array
    pure (take n (Foldable.toList array1), gen1)
{-# SPECIALIZE sample :: Int -> [a] -> Random.StdGen -> ([a], Random.StdGen) #-}

-- | \(\mathcal{O}(n)\). Like 'sample', but discards the final generator.
sample_ :: (RandomGen g) => Int -> [a] -> g -> [a]
sample_ n list g =
  fst (sample n list g)
{-# SPECIALIZE sample_ :: Int -> [a] -> Random.StdGen -> [a] #-}

-- | \(\mathcal{O}(n)\). Like 'sample', but uses the global random number generator.
sampleIO :: (MonadIO m) => Int -> [a] -> m [a]
sampleIO n list =
  sample_ n list <$> Random.newStdGen
{-# SPECIALIZE sampleIO :: Int -> [a] -> IO [a] #-}

-- Swap two elements in a mutable array.
swapArrayElems :: Int -> Int -> Array.MutableArray s a -> ST s ()
swapArrayElems i j array = do
  x <- Array.readArray array i
  y <- Array.readArray array j
  Array.writeArray array i y
  Array.writeArray array j x
{-# INLINE swapArrayElems #-}

-- Construct a mutable array from a list.
listToMutableArray :: forall a s. [a] -> ST s (Array.MutableArray s a)
listToMutableArray list = do
  array <- Array.newArray (length list) undefined
  let writeElems :: Int -> [a] -> ST s ()
      writeElems !i = \case
        [] -> pure ()
        x : xs -> do
          Array.writeArray array i x
          writeElems (i + 1) xs
  writeElems 0 list
  pure array
{-# INLINE listToMutableArray #-}
