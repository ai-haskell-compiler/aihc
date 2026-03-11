module Main where

import Data.Word
import Criterion.Main
import Data.ByteString ( ByteString )
import Data.ByteString.Base58

import qualified Data.ByteString as BS

symChars :: [Word8]
symChars = cycle
           $ BS.unpack
           $ unAlphabet bitcoinAlphabet

symN :: Int -> ByteString
symN n = BS.pack
         $ take n
         $ symChars

nameVal :: [(String, Int)]
nameVal = [ ("10 bytes", 10)
          , ("42 bytes", 100)
          , ("100 bytes", 1000) ]

main :: IO ()
main = defaultMain
       [ bgroup "encodeBase58I"
         [ bench "approx 10 bytes (1e24)" $  nf (encodeBase58I bitcoinAlphabet) 1000000000000000000000000
         , bench "approx 42 bytes (1e100)" $ nf (encodeBase58I bitcoinAlphabet) 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
         , bench "approx 100 bytes (1e240)" $ nf (encodeBase58I bitcoinAlphabet) 100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
         ]

       , bgroup "decodeBase58I"
         $ map (\(name, val) -> bench name $ nf (decodeBase58I bitcoinAlphabet) $ symN val) nameVal

       , bgroup "encodeBase58"
         $ map (\(name, val) -> bench name $ nf (encodeBase58 bitcoinAlphabet) $ symN val) nameVal

       , bgroup "decodeBase58"
         $ map (\(name, val) -> bench name $ nf (decodeBase58 bitcoinAlphabet) $ symN val) nameVal
       ]
