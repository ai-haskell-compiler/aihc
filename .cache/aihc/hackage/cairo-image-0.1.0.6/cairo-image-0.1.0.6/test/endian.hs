{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import System.TargetEndian

main :: IO ()
main = do
	putStrLn "test endian"
	putStrLn $(endian [e| "little endian" |] [e| "big endian" |])
