
{-# LANGUAGE TemplateHaskell #-}

import System.Exit (exitSuccess, exitFailure)
import Development.IncludeFile
import qualified Data.ByteString as B

$(includeFileInSource "test/random" "test")

main :: IO ()
main = do
  b <- B.readFile "test/random"
  if b == test
     then exitSuccess
     else do putStrLn "Fail: inputs differ."
             putStrLn "Original:"
             print $ B.unpack b
             putStrLn "Included:"
             print $ B.unpack test
             exitFailure
