{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Codec.Compression.QuickLZ

main :: IO ()
main = do
	let xs = "LZ compression is based on finding repeated strings: Five, six, seven, eight, nine, fifteen, sixteen, seventeen, fifteen, sixteen, seventeen." :: ByteString

	putStrLn $ "length of original string: " ++ (show $ B.length xs)

	let ys = compress xs

	putStrLn $ "length of compressed string: " ++ (show $ B.length ys)

	let zs = decompress ys

	putStr $ "decompressed back to " ++ (show $ B.length zs) ++ " bytes, got: "
	B.putStrLn zs 