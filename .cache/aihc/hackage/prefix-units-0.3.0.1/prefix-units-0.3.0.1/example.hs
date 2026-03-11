module Main where

import Data.Prefix.Units

showValueAs :: Double -> FormatMode -> IO ()
showValueAs val mode = do
  putStrLn $ "Formatting " ++ show val ++ " in mode " ++ show mode ++
             " is: " ++ showValue mode val

main = do
  putStr $ "Enter a value (with optional units; case sensitive!): "
  in_str <- getLine
  case parseValue ParseExact in_str of
    Left err -> putStrLn $ "Error parsing the value: " ++ err
    Right v -> do
         putStrLn $ "You entered " ++ show v
         mapM_ (showValueAs v) [ FormatSiAll
                               , FormatSiSupraunitary
                               , FormatSiKMGT
                               , FormatBinary
                               , FormatUnscaled
                               ]
         mapM_ (showValueAs v . FormatFixed) [minBound..maxBound]
