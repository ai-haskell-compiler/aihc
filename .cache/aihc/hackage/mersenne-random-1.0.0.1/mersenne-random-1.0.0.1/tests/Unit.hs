{- LANGUAGE BangPatterns, PatternSignatures, ScopedTypeVariables -}
{-# OPTIONS -fbang-patterns -fglasgow-exts -#include "SFMT.h" #-}

import Control.Exception
import Control.Monad
import Data.Int
import Data.Typeable
import Data.Word
import System.CPUTime
import System.Environment
import System.IO
import Text.Printf
import qualified System.Random as Old

import System.Random.Mersenne
import Control.Concurrent
import Control.Concurrent.MVar

main = do
    print  version
    g <- newMTGen (Just 5)

    s   <- newMVar 0 :: IO (MVar Int)
    putStr "Callibrating ... " >> hFlush stdout

    tid <- forkIO $ do
        let go !i = do !a <- random g :: IO Double
                       !_ <- swapMVar s i
                       go (i+1)
        go 0

    threadDelay (1000 * 1000)
    killThread tid
    putStrLn "done."
    n <- readMVar s -- 1 sec worth of generation
    print n

    time $ gen n g
    ranges_strict n g
--    ranges_strict_range g
    ranges_ty n g
--    ranges g

    speed n (undefined :: Int) g
    speed n (undefined :: Integer) g
    speed n (undefined :: Double) g

    time $ sums n g
    time $ sum_lazy n g

gen n g = do
    forM_ [0 .. n] $ \i -> do
        x <- random g :: IO Word
        when (i < 100) $ do
            printf "%12u " (fromIntegral x :: Int)
            when (i `rem` 4 == 3) $
                putChar '\n'


time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v


-- overhead cause by random's badness
sums lim g = do

    printf "Generating %d randoms ...\n" lim


    let go :: Int -> Int -> IO Int
        go !n !acc
            | n >= lim = return acc
            | otherwise     = do
                    a <- random g
                    go (n+1) (if a < acc then a else acc)
    print =<< go 0 0


sum_lazy lim g = do
    printf "Generating %d randoms lazily ...\n" lim

    xs <- randoms g :: IO [Int]
    print (minimum (take lim xs)) -- faster when it fuses.



{-
-- overhead cause by random's badness
ranges g = do
    let n = 10000000
    test g n (undefined :: Bool)
    test g n (undefined :: Word8)
    test g n (undefined :: Word16)
    test g n (undefined :: Word32)
    test g n (undefined :: Word64)
    test g n (undefined :: Word)
    test g n (undefined :: Int)
    test g n (undefined :: Int64)
    test g n (undefined :: Int32)
    test g n (undefined :: Int16)
    test g n (undefined :: Int8)
    test g n (undefined :: Integer)
-}

-- overhead cause by random's badness
ranges_ty n g = do
    test_type g n (undefined :: Bool)
    test_type g n (undefined :: Word8)
    test_type g n (undefined :: Word16)
    test_type g n (undefined :: Word32)
    test_type g n (undefined :: Word64)
    test_type g n (undefined :: Word)
    test_type g n (undefined :: Int)
    test_type g n (undefined :: Int64)
    test_type g n (undefined :: Int32)
    test_type g n (undefined :: Int16)
    test_type g n (undefined :: Int8)

-- overhead cause by random's badness
ranges_strict n g = do
    test_strict g n (undefined :: Word)
    test_strict g n (undefined :: Word8)
    test_strict g n (undefined :: Word16)
    test_strict g n (undefined :: Word32)
    test_strict g n (undefined :: Word64)
    test_strict g n (undefined :: Int)
    test_strict g n (undefined :: Int64)
    test_strict g n (undefined :: Int32)
    test_strict g n (undefined :: Int16)
    test_strict g n (undefined :: Int8)
    test_strict g n (undefined :: Double)
    test_strict g n (undefined :: Integer)
    test_strict g n (undefined :: Bool)

{-
-- overhead cause by random's badness
ranges_strict_range g = do
    let n = 100000000
    test_strict_range g n (undefined :: Word)
    test_strict_range g n (undefined :: Word8)
    test_strict_range g n (undefined :: Word16)
    test_strict_range g n (undefined :: Word32)
    test_strict_range g n (undefined :: Word64)
    test_strict_range g n (undefined :: Int)
    test_strict_range g n (undefined :: Int64)
    test_strict_range g n (undefined :: Int32)
    test_strict_range g n (undefined :: Int16)
    test_strict_range g n (undefined :: Int8)
    test_strict_range g n (undefined :: Double)
    test_strict_range g n (undefined :: Integer)
    test_strict_range g n (undefined :: Bool)
    -}


------------------------------------------------------------------------
-- check values are in range for randomRs

{-
test :: forall a . (Show a, Ord a, Typeable a, MTRandom a) => MTGen -> Int -> a -> IO ()
test g n ty = do
    a' <- random g :: IO a
    b' <- random g :: IO a
    let (a,b) = (min a' b', max a' b')

    printf "%d bounded :: %s ...\t" n (show $  typeOf ty)
    hFlush stdout

    time $ do
        xs <- randomRs (a,b) g :: IO [a]

        sequence_
            [ if x >= a && x <= b
                    then return ()
                    else error $ "Fail " ++ show (x,a,b)
            | x <- take n xs ]

--      printf " all good."

-}

test_type :: forall a . (Bounded a, Show a, Ord a, Typeable a, MTRandom a) => MTGen -> Int -> a -> IO ()
test_type g n ty = do

    printf "lazy   generation of %d  :: %s ...\t" n (show $  typeOf ty)
    hFlush stdout

    time $ do
        xs <- randoms g :: IO [a]

        sequence_
            [ if x >= minBound && x <= maxBound
                    then return ()
                    else error $ "Fail " ++ show x
            | x <- take n xs ]

--      printf "all good. "

test_strict :: forall a . (Show a, Ord a, Typeable a, MTRandom a) => MTGen -> Int -> a -> IO ()
test_strict g n ty = do

    printf "strict generation of %d :: type %s ...\t" n (show $  typeOf ty)
    hFlush stdout

    time $ do
        let go i
                | i > n = return ()
                | otherwise = do
                    x <- random g :: IO a
                    x `seq` go (i+1)

        go 0

--      printf "all good. "

{-
test_strict_range :: forall a . (Show a, Ord a, Typeable a, MTRandom a) => MTGen -> Int -> a -> IO ()
test_strict_range g n ty = do
    a' <- random g :: IO a
    b' <- random g :: IO a
    let (a,b) = (min a' b', max a' b')

    printf "strict, ranged generation of %d :: type %s ...\t" n (show $  typeOf ty)
    hFlush stdout

    time $ do
        let go i
                | i > n = return ()
                | otherwise = do
                    x <- randomR (a,b) g :: IO a
                    if x >= a && x <= b
                        then go (i+1)
                        else error $ "test_strict_range failed " ++ show (a,b,x)

        go 0

        printf "all good. "
-}

------------------------------------------------------------------------
-- compare with System.Random

-- overhead cause by random's badness
speed :: forall a . (Show a, Ord a, Typeable a, Old.Random a, Num a, MTRandom a)
                            => Int -> a -> MTGen -> IO ()
speed lim ty g  = do

--  x <- 
     time $ do
        putStrLn $ "System.Random: " ++ show lim ++ " " ++ show (show $ typeOf ty)
        let g = Old.mkStdGen 5

        let go :: Old.StdGen -> Int -> a -> a
            go !g !n !acc
                | n >= lim = acc
                | otherwise     =
                        let (a,g') = Old.random g
                        in go g' (n+1) (if a > acc then a else acc)
        print (go g 0 0)

--  y <-
     time $ do
        putStrLn $ "System.Random.Mersenne: " ++ show lim ++ " " ++ show (show $ typeOf ty)
        let go !n !acc
                | n >= lim = return acc
                | otherwise     = do
                        a <- random g :: IO a
                        go (n+1::Int) (if a > acc then a else acc)
        print =<< go 0 0

--    printf "MT is %s times faster generating %s\n" (show $x`div`y) (show (typeOf ty))
--    return ()

