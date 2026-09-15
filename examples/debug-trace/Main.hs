module Main where

import Debug.Trace (trace, traceIO, traceM, traceShow, traceShowId, traceWith)

double :: Int -> Int
double value = trace ("double " ++ show value) (value * 2)

main :: IO ()
main = do
  print (double 21)
  print (traceShow "shown" (1 :: Int))
  print (traceShowId (2 :: Int))
  print (traceWith (\value -> "with " ++ show value) (3 :: Int))
  traceM "in the monad"
  traceIO "at the end"
