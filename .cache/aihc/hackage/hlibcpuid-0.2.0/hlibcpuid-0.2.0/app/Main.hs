module Main where

import System.LibCPUID


main :: IO ()
main = do
  putStrLn "------------------------------------------"
  putStrLn "Available CPU information"
  putStrLn "------------------------------------------"
  exampleCPUID

  putStrLn "------------------------------------------"
  putStrLn "Measuring CPU clock frequency"
  putStrLn "------------------------------------------"
  exampleTSC

  putStrLn "------------------------------------------"
  putStrLn "Other utilities"
  putStrLn "------------------------------------------"
  exampleUtilities

exampleCPUID :: IO ()
exampleCPUID = cpuid >>= \case
  Left err -> error err
  Right CPUID {..} -> do
    mapM_ putStrLn
      [ "vendor string: " ++ vendorString
      , "brand string: " ++ brandString
      , "has a time-stamp counter (TSC): " ++  if hasTSC then "yes" else "no"
      , "# physical cores per processor: " ++ show physicalCores
      , "# logical cores per processor: " ++ show logicalCores
      , "# processors: " ++ show (div totalLogicalCores logicalCores)
      ]

exampleTSC :: IO ()
exampleTSC = do
  tscMark <- initialise

  mark tscMark
  let fib = scanl (+) 0 (1:fib)
  putStrLn $ "The 1 000 000th number in the Fibonacci sequence is " ++ show @Int (last (take 1000000 fib))
  unmark tscMark
  frequency <- clockBy tscMark

  putStrLn $ "calculated @" ++ show frequency ++ "MHz"

exampleUtilities :: IO ()
exampleUtilities = do
  cores <- getTotalLogicalCores
  putStrLn $ "Total number of logical cores: " ++ show cores

  let putMHzLn str n = putStrLn $ str ++ show n ++ "MHz"

  clockByOS >>= putMHzLn "clock by OS: "

  let quadruple n = clockMeasure n (ShouldQuadCheck True)
  quadruple 100 >>= putMHzLn "quadruple clock measure with a 100ms busy-wait cycle: "
  quadruple 400 >>= putMHzLn "quadruple clock measure with a 400ms busy-wait cycle: "
  quadruple 1000 >>= putMHzLn "quadruple clock measure with a 1000ms busy-wait cycle: "

  clock >>= putMHzLn "a fall-back method clock: "
