{- OPTIONS -cpp -fglasgow-exts -fvia-C -optc-O2 -optc-msse2 -optc-march=core2 -optc-ffast-math -fexcess-precision -}

#if defined(FAST)
import System.Random.Mersenne
#else
import System.Random
#endif
import System.Environment

#if defined(FAST)

main = do
    [lim] <- mapM readIO =<< getArgs
    g <- newMTGen Nothing

    let go :: Int -> Int -> IO Double
        go throws ins
            | throws >= lim  = return ((4 * fromIntegral ins) / (fromIntegral throws))
            | otherwise = do
                x <- random g :: IO Double
                y <- random g :: IO Double
                if x * x + y * y < 1
                    then go (throws+1) $! ins + 1
                    else go (throws+1) ins

    print =<< go 0 0

{-
    $ time ./pi 100000000
    3.14166916
    ./pi 100000000  4.31s user 0.00s system 99% cpu 4.311 total
-}

#else

main = do
    [lim] <- mapM readIO =<< getArgs
    g <- newStdGen

    let go :: StdGen -> Int -> Int -> IO Double
        go g throws ins
            | throws >= lim  = return ((4 * fromIntegral ins) / (fromIntegral throws))
            | otherwise = do
                let (x::Double,g1) = random g
                    (y::Double,g2) = random g1
                if x * x + y * y < 1
                    then go g2 (throws+1) $! ins + 1
                    else go g2 (throws+1) ins

    print =<< go g 0 0

#endif
