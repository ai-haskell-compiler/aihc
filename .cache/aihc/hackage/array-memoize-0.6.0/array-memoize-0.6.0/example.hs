import Data.Function.ArrayMemoize

-- Example:

-- Fibonacci (pre-fixed point)

fib' :: (Int -> Int) -> Int -> Int
fib' _ 0 = 1
fib' _ 1 = 1
fib' rec n = rec (n - 1) + rec (n - 2)

-- Memoizes fib between 0 and 100 (after that it is a run-time error)

fib :: Int -> Int
fib = arrayMemoFix (0, 1000) fib'

-- IO variant

fibIO' :: (Int -> IO Int) -> Int -> IO Int
fibIO' _ 0 = return 1
fibIO' _ 1 = return 1
fibIO' rec n = do a <- rec (n - 1)
                  b <- rec (n - 2)
                  return (a + b)

fibIO :: Int -> IO Int
fibIO = uarrayMemoFixIO (0,1000) fibIO'

-- Manual fix versions 

fibA :: Int -> Int
fibA 0 = 1
fibA 1 = 1
fibA n = fibB (n - 1) + fibB (n - 2)

fibB = arrayMemo (0, 1000) fibA 
