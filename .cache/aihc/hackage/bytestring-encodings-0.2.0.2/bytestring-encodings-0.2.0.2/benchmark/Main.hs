module Main (main) where

import Gauge.Main
import Data.ByteString.Ascii (isAscii)
import Data.ByteString.Utf8  (isUtf8)
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text.Encoding as TE

main :: IO ()
main = do
  let bs = BC.pack (L.take 50000 (L.cycle ['a'..'z']))
      bsAscii = bs
      bsUtf8  = BC.pack (L.take 50000 (L.cycle
        [ '¿', 'D', 'ó', 'n', 'd', 'e'
        ]))

  defaultMain
    [ 
      bgroup "ASCII: naive versus isAscii"
      [ bench "Data.ByteString.all" $ whnf standardIsAscii bs
      , bench "isAscii" $ whnf isAscii bs
      ]
    , bgroup "ASCII: Data.Text.Encoding.decodeUtf8 versus isUtf8" 
      [ bench "Data.Text.Encoding.decodeUtf8" $ whnf TE.decodeUtf8 bsAscii
      , bench "isUtf8" $ whnf isUtf8 bsAscii
      ]
    , bgroup "UTF-8: isUtf8"
      [ 
        bench "isUtf8" $ whnf isUtf8 bsUtf8 
      ]
    ]

standardIsAscii :: ByteString -> Bool
standardIsAscii bs = B.all (\x -> x < 127) bs
