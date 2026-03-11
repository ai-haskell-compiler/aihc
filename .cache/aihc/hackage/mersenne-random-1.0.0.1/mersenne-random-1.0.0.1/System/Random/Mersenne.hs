{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns #-}
--------------------------------------------------------------------
-- |
-- Module     : System.Random.Mersenne
-- Copyright  : Copyright (c) 2008, Don Stewart <dons@galois.com>
-- License    : BSD3
-- Maintainer : Don Stewart <dons@galois.com>
-- Stability  : experimental
-- Portability: CPP, FFI
-- Tested with: GHC 6.8.2
--
-- Generate pseudo-random numbers using the SIMD-oriented Fast Mersenne Twister(SFMT)
-- pseudorandom number generator. This is a /much/ faster generator than
-- the default 'System.Random' generator for Haskell (~50x faster
-- generation for Doubles, on a core 2 duo), however, it is not 
-- nearly as flexible.
--
-- This library may be compiled with the '-f use_sse2' or '-f
-- use_altivec' flags to configure, on intel and powerpc machines
-- respectively, to enable high performance vector instructions to be used.
-- This typically results in a 2-3x speedup in generation time.
--
-- This will work for newer intel chips such as Pentium 4s, and Core, Core2* chips.
--
module System.Random.Mersenne (

    -- * The random number generator
    MTGen

    -- ** Initialising the generator
    , newMTGen

    -- * Random values of various types

    -- $notes
    , MTRandom(..)

    -- $globalrng
    , getStdRandom
    , getStdGen
    , setStdGen

    -- * Miscellaneous
    , version

    -- $example

    ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

import Foreign.C.Types
import Foreign.C.String

import System.CPUTime	( getCPUTime )
import System.Time
import System.IO.Unsafe
-- import Control.Monad

import Data.Word
import Data.Int
import Data.Bits
-- import Data.Char
import Data.IORef

------------------------------------------------------------------------
-- $example
--
-- An example, calculation of pi via a monte carlo method:
--
-- > import System.Random.Mersenne
-- > import System.Environment
--
-- We'll roll the dice 'lim' times, 
--
-- > main = do
-- >    [lim] <- mapM readIO =<< getArgs
--
-- Now, define a loop that runs this many times, plotting a 'x' and 'y'
-- position, then working out if its in and outside the circle. 
-- The ratio of inside\/total points at then gives us an approximation
-- of pi.
--
-- > let go :: Int -> Int -> IO Double
-- >     go throws ins
-- >         | throws >= lim  = return ((4 * fromIntegral ins) / (fromIntegral throws))
-- >         | otherwise = do
-- >             x <- random g :: IO Double
-- >             y <- random g :: IO Double
-- >             if x * x + y * y < 1
-- >                 then go (throws+1) $! ins + 1
-- >                 else go (throws+1) ins
--
-- Compiling this, '-fexcess-precision', for accurate Doubles,
--
-- > $ ghc -fexcess-precision -fvia-C pi.hs -o pi
-- > $ ./pi 10000000                                                 
-- > 3.1417304
--

------------------------------------------------------------------------

-- | A single, global SIMD fast mersenne twister random number generator
-- This generator is evidence that you have initialised the generator,
--
data MTGen = MTGen

-- | Return an initialised SIMD Fast Mersenne Twister.
-- The generator is initialised based on the clock time, if Nothing
-- is passed as a seed. For deterministic behaviour, pass an explicit seed.
--
-- Due to the current SFMT library being vastly impure, currently only a single
-- generator is allowed per-program. Attempts to reinitialise it will fail.
--
newMTGen :: Maybe Word32 -> IO MTGen
newMTGen (Just n) = do
    dup <- c_get_initialized
    if dup == 0
       then do c_init_gen_rand (fromIntegral n)
               return MTGen

       else error $ "System.Random.Mersenne: " ++
                    "Only one mersenne twister generator can be created per process"

newMTGen Nothing = do
    ct             <- getCPUTime
    (TOD sec psec) <- getClockTime
    newMTGen (Just (fromIntegral $ sec * 1013904242 + psec + ct) )

------------------------------------------------------------------------

-- $notes
--
-- Instances MTRandom for Word, Word64, Word32, Word16, Word8
-- all return, quickly, a random inhabintant of that type, in its full
-- range. Similarly for Int types.
--
-- Int and Word will be 32 bits on a 32 bit machine, and 64 on a 64 bit
-- machine. The double precision will be 32 bits on a 32 bit machine,
-- and 53 on a 64 bit machine.
--
-- The MTRandom instance for Double returns a Double in the interval [0,1).
-- The Bool instance takes the lower bit off a random word.

-- | Given an initialised SFMT generator, the MTRandom
-- allows the programmer to extract values of a variety of 
-- types.
--
-- Minimal complete definition: 'random'.
--
class MTRandom a where

  -- | The same as 'randomR', but using a default range determined by the type:
  --
  -- * For bounded types (instances of 'Bounded', such as 'Char'),
  --   the range is normally the whole type.
  --
  -- * For fractional types, the range is normally the semi-closed interval
  -- @[0,1)@.
  --
  -- * For 'Integer', the range is (arbitrarily) the range of 'Int'.
  random :: MTGen -> IO a

  -- | Plural variant of 'random', producing an infinite list of
  -- random values instead of returning a new generator.
  randoms  :: MTGen -> IO [a]
  randoms !g = unsafeInterleaveIO $ do
                x  <- random g
                xs <- randoms g
                return (x : xs)
  -- There are real overheads here. Consider eagerly filling chunks
  -- and extracting elements piecewise.

{-
  -- | Takes a range /(lo,hi)/ and a random number generator
  -- /g/, and returns a random value uniformly distributed in the closed
  -- interval /[lo,hi]/, together with a new generator. It is unspecified
  -- what happens if /lo>hi/. For continuous types there is no requirement
  -- that the values /lo/ and /hi/ are ever produced, but they may be,
  -- depending on the implementation and the interval.
  randomR :: (a,a) -> MTGen -> IO a
-}

{-
  -- | Plural variant of 'random', producing an infinite list of
  -- random values instead of returning a new generator.
  randomRs  :: (a,a) -> MTGen -> IO [a]
  randomRs p !g = unsafeInterleaveIO $ do
                x  <- randomR p g
                xs <- randomRs p g
                return (x : xs)
-}

  -- | A variant of 'random' that uses the global random number generator
  -- (see "System.Random#globalrng").
  -- Essentially a convenience function if you're already in IO.
  --
  -- Note that there are performance penalties calling randomIO in an
  -- inner loop, rather than 'random' applied to a global generator. The
  -- cost comes in retrieving the random gen from an IORef, which is
  -- non-trivial. Expect a 3x slow down in speed of random generation.
  randomIO :: IO a
  randomIO = getStdRandom random
  {-# INLINE randomIO #-}

------------------------------------------------------------------------
-- Efficient basic instances

instance MTRandom Word    where
    random !_          = randomWord
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalWord g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Word64  where
    random !_ = fmap fromIntegral randomWord64
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalWord g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Word32  where
    random !_ = fmap fromIntegral randomWord
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalWord g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Word16  where
    random !_ = fmap fromIntegral randomWord
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalWord g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Word8   where
    random !_          = fmap fromIntegral randomWord
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalWord g (fromIntegral lo) (fromIntegral hi)

------------------------------------------------------------------------

instance MTRandom Double where
    random !_ = randomDouble
    {-# INLINE random #-}
--    randomR (lo,hi) g = randomIvalDouble g lo hi id

------------------------------------------------------------------------

instance MTRandom Int     where
    random !_          = randomInt
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalInt g lo hi

instance MTRandom Int64   where
    random !_ = fmap fromIntegral randomInt64
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalInt g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Int32   where
    random !_ = fmap fromIntegral randomInt
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalInt g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Int16   where
    random !_ = fmap fromIntegral randomInt
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalInt g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Int8    where
    random !_ = fmap fromIntegral randomInt
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalInt g (fromIntegral lo) (fromIntegral hi)

instance MTRandom Integer where
    random !_ = fmap fromIntegral randomInt
    {-# INLINE random #-}
--    randomR (lo,hi) !g = randomIvalInt g (fromIntegral lo) (fromIntegral hi)

------------------------------------------------------------------------

instance MTRandom Bool    where
    random !_ = do x <- randomWord; return $! x .&. 1 /= 0
    {-# INLINE random #-}

{-
    randomR (a,b) !g = int2Bool `fmap` randomIvalInt g (bool2Int a) (bool2Int b)

        where
            bool2Int :: Bool -> Int
            bool2Int False = 0
            bool2Int True  = 1

            int2Bool :: Int -> Bool
            int2Bool 0 = False
            int2Bool _ = True
-}

------------------------------------------------------------------------

{-
randomIvalInt :: (MTRandom a, Num a) => MTGen -> Int -> Int -> IO a
randomIvalInt g l h
  | l > h     = randomIvalInt g h l
  | otherwise = do
        v <- f n 1
        return $ (fromIntegral (l + v `mod` k))
     where
       k = h - l + 1
       b = maxBound :: Int
       n = iLogBase b k

       f 0 acc = return acc
       f i acc = do
           x <- random g :: IO Int
           f (i-1) (fromIntegral x + acc * b)
{-# INLINE randomIvalInt #-}

iLogBase :: Int -> Int -> Int
iLogBase b i = if i < b then 1 else 1 + iLogBase b (i `div` b)
-}

------------------------------------------------------------------------

{-
randomIvalWord :: (MTRandom a, Num a) => MTGen -> Word -> Word -> IO a
randomIvalWord g l h
  | l > h     = randomIvalWord g h l
  | otherwise = do
        v <- f n 1
        return $ (fromIntegral (l + v `mod` k))
     where
       k = h - l + 1
       b = maxBound :: Word
       n = iLogBaseWord b k

       f 0 acc = return acc
       f i acc = do
           x <- random g :: IO Word
           f (i-1) (fromIntegral x + acc * b)
{-# INLINE randomIvalWord #-}

iLogBaseWord :: Word -> Word -> Word
iLogBaseWord b i = if i < b then 1 else 1 + iLogBaseWord b (i `div` b)
-}

------------------------------------------------------------------------

{-
--
-- Too slow:
--
randomIvalDouble :: (MTRandom a, Fractional a) => MTGen -> Double -> Double -> (Double -> a) -> IO a
randomIvalDouble g l h fromDouble
  | l > h     = randomIvalDouble g h l fromDouble
  | otherwise = do
       x <- random g :: IO Int
       return $ fromDouble ((l+h)/2) +
                fromDouble ((h-l) / realToFrac intRange) *
                fromIntegral x
{-# INLINE randomIvalDouble #-}

intRange :: Integer
intRange = toInteger (maxBound::Int) - toInteger (minBound::Int)
-}
------------------------------------------------------------------------
--
-- Using a single global random number generator
--

{- $globalrng #globalrng#

There is a single, implicit, global random number generator of type
'StdGen', held in some global variable maintained by the 'IO' monad. It is
initialised automatically in some system-dependent fashion. To get
deterministic behaviour, use 'setStdGen'.
-}

theStdGen :: IORef MTGen
theStdGen  = unsafePerformIO $ do
   rng <- newMTGen Nothing
   newIORef rng
{-# NOINLINE theStdGen #-}

-- |Sets the global random number generator.
setStdGen :: MTGen -> IO ()
setStdGen = writeIORef theStdGen

-- |Gets the global random number generator.
getStdGen :: IO MTGen
getStdGen  = readIORef theStdGen

-- | Uses the supplied function to get a value from the current global
-- random generator, and updates the global generator with the new
-- generator returned by the function. For example, @rollDice@ gets a
-- random integer between 1 and 6:
--
-- >  rollDice :: IO Int
-- >  rollDice = getMTRandom (randomR (1,6))
--
getStdRandom :: (MTGen -> IO a) -> IO a
getStdRandom f = do
    st <- readIORef theStdGen
    f st
{-# INLINE getStdRandom #-}

------------------------------------------------------------------------

-- | Returns the identification string for the SMFT version. 
-- The string shows the word size, the Mersenne exponent, and all parameters of this generator.
version :: String
version = unsafePerformIO (peekCString =<< c_get_idstring)

------------------------------------------------------------------------
-- Safe primitives: depend on the word size. It's generally not a 
-- good idea to mix generation of different types, unless you commit
-- to either 32 or 64 bits only.
--
-- So you should only use these functions for getting at randoms.

randomInt :: IO Int
randomInt = fmap fromIntegral
#if WORD_SIZE_IN_BITS < 64
    c_gen_rand32
#else
    c_gen_rand64
#endif

-- TODO randomWord64, for 32 bit machines

randomWord :: IO Word
randomWord = fmap fromIntegral
#if WORD_SIZE_IN_BITS < 64
    c_gen_rand32
#else
    c_gen_rand64
#endif

randomWord64 :: IO Word64
randomWord64 = fmap fromIntegral
#if WORD_SIZE_IN_BITS < 64
    c_gen_rand64_mix
#else
    c_gen_rand64
#endif

randomInt64 :: IO Int64
randomInt64 = fmap fromIntegral
#if WORD_SIZE_IN_BITS < 64
    c_gen_rand64_mix
#else
    c_gen_rand64
#endif

randomDouble :: IO Double
randomDouble = fmap realToFrac
#if WORD_SIZE_IN_BITS < 64
    c_genrand_real2
#else
    c_genrand_res53
#endif

------------------------------------------------------------------------
-- Generating chunks at a time.
--

{-
min_array_size :: Int
min_array_size = fromIntegral . unsafePerformIO $ -- constant
#if WORD_SIZE_IN_BITS < 64
    c_get_min_array_size32
#else
    c_get_min_array_size64
#endif

-- | Fill an array with 'n' random Ints
fill_array :: Ptr Int -> Int -> IO ()
fill_array p n =
#if WORD_SIZE_IN_BITS < 64
    c_fill_array32 (castPtr p) (fromIntegral n)
#else
    c_fill_array64 (castPtr p) (fromIntegral n)
#endif
-}

------------------------------------------------------------------------
-- We can have only one mersenne supply in a program.

-- You have to commit at initialisation time to call either
-- rand_gen32 or rand_gen64, and correspondingly, real2 or res53
-- for doubles.
--

type UInt32 = CUInt
type UInt64 = CULLong


-- | This function initializes the internal state array with a 32-bit integer seed.
foreign import ccall unsafe "SFMT.h init_gen_rand"
    c_init_gen_rand         :: UInt32 -> IO ()

-- Getting a random int

-- This function generates and returns 64-bit pseudorandom number.
-- init_gen_rand or init_by_array must be called before this function.
-- The function gen_rand64 should not be called after gen_rand32,
-- unless an initialization is again executed. 

#if WORD_SIZE_IN_BITS < 64

foreign import ccall unsafe "SFMT.h gen_rand32"
    c_gen_rand32            :: IO UInt32

foreign import ccall unsafe "SFMT_wrap.h gen_rand64_mix_wrap"
    c_gen_rand64_mix        :: IO UInt64

#else

foreign import ccall unsafe "SFMT.h gen_rand64"
    c_gen_rand64            :: IO UInt64

#endif

-- Getting a random double

-- | Generates a random number on [0,1)-real-interval
-- calls gen_rand32
-- | Generates a random number on [0,1) with 53-bit resolution. Fast on 64 bit machines.
-- calls gen_rand64

-- | generates a random number on [0,1) with 53-bit resolution using
-- 32bit integer

#if WORD_SIZE_IN_BITS < 64
foreign import ccall unsafe "SFMT_wrap.h genrand_real2_wrap"
    c_genrand_real2         :: IO CDouble

-- foreign import ccall unsafe "SFMT.h genrand_res53_mix"
--     c_genrand_res53_mix     :: IO CDouble
#else
foreign import ccall unsafe "SFMT_wrap.h genrand_res53_wrap"
    c_genrand_res53         :: IO CDouble
#endif

------------------------------------------------------------------------

{-
-- Generates a random number on [0,1]-real-interval
-- calls gen_rand32
foreign import ccall unsafe "SFMT.h genrand_real1"
    c_genrand_real1         :: IO CDouble

-- | Generates a random number on (0,1)-real-interval
-- calls gen_rand32
foreign import ccall unsafe "SFMT.h genrand_real3"
    c_genrand_real3         :: IO CDouble
-}

------------------------------------------------------------------------

{-
foreign import ccall unsafe "SFMT.h get_min_array_size32"
    c_get_min_array_size32  :: IO CInt

foreign import ccall unsafe "SFMT.h get_min_array_size64"
    c_get_min_array_size64  :: IO CInt

foreign import ccall unsafe "SFMT.h fill_array32"
    c_fill_array32          :: Ptr UInt32 -> CInt -> IO ()

foreign import ccall unsafe "SFMT.h fill_array64"
    c_fill_array64          :: Ptr UInt64 -> CInt -> IO ()
-}

------------------------------------------------------------------------

foreign import ccall unsafe "SFMT.h get_idstring"
    c_get_idstring          :: IO CString

foreign import ccall unsafe "SFMT.h get_initialized"
    c_get_initialized        :: IO CInt

--
-- Invariant: we can never call rand32 if we're in 64 bit land,
-- and never call rand64 if in 32 bit land.
-- 
-- audit this!
--
