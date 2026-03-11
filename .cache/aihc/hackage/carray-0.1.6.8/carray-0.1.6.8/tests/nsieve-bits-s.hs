{-# OPTIONS -O2 -optc-O #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns#-}

--
-- The Computer Language Shootout
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
-- nsieve over an ST monad Bool array
--

import Data.Array.Storable (StorableArray)
import Data.Array.MArray (MArray, newArray)
import Data.Array.Base (unsafeRead, unsafeWrite)

import System.Environment (getArgs)
import Control.Monad (when)
import Data.Bits (shiftL)
import Text.Printf (printf)


main :: IO ()
main = do
    n <- getArgs >>= readIO . head :: IO Int
    mapM_ (sieve . (10000 *) . (2 ^)) [n, n-1, n-2]

sieve :: Int -> IO ()
sieve n = do
   r <- do
      a <- newArray (2,n) True :: IO (StorableArray Int Bool)
      go a n 2 0
   printf "Primes up to %8d %8d\n" (n::Int) (r::Int) :: IO ()

go ::
   (MArray array Bool m, Num a) =>
   array Int Bool -> Int -> Int -> a -> m a
go !a !m !n !c
    | n == m    = return c
    | otherwise = do
          e <- unsafeRead a n
          if e then let loop !j
                          | j < m     = do
                              x <- unsafeRead a j
                              when x $ unsafeWrite a j False
                              loop (j+n)

                          | otherwise = go a m (n+1) (c+1)
                    in loop (n `shiftL` 1)
               else go a m (n+1) c
