{-# LANGUAGE PackageImports #-}
module Main
( main
)where


import Criterion
import Criterion.Main

import "memory" Data.ByteArray.Encoding as Mem
import Data.ByteString
import "base32" Data.ByteString.Base32 as B32
import Data.ByteString.Random (random)


main :: IO ()
main =
  defaultMain
    [ env bs $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "encode"
      [ bgroup "25"
        [ bench "memory" $ whnf ctob bs25
        , bench "base32" $ whnf B32.encodeBase32' bs25
        ]
      , bgroup "100"
        [ bench "memory" $ whnf ctob bs100
        , bench "base32" $ whnf B32.encodeBase32' bs100
        ]
      , bgroup "1k"
        [ bench "memory" $ whnf ctob bs1k
        , bench "base32" $ whnf B32.encodeBase32' bs1k
        ]
      , bgroup "10k"
        [ bench "memory" $ whnf ctob bs10k
        , bench "base32" $ whnf B32.encodeBase32' bs10k
        ]
      , bgroup "100k"
        [ bench "memory" $ whnf ctob bs100k
        , bench "base32" $ whnf B32.encodeBase32' bs100k
        ]
      , bgroup "1mm"
        [ bench "memory" $ whnf ctob bs1mm
        , bench "base32" $ whnf B32.encodeBase32' bs1mm
        ]
      ]
    , env bs' $ \ ~(bs25,bs100,bs1k,bs10k,bs100k,bs1mm) ->
      bgroup "decode"
      [ bgroup "25"
        [ bench "memory" $ whnf btoc bs25
        , bench "base32" $ whnf B32.decodeBase32 bs25
        ]
      , bgroup "100"
        [ bench "memory" $ whnf btoc bs100
        , bench "base32" $ whnf B32.decodeBase32 bs100
        ]
      , bgroup "1k"
        [ bench "memory" $ whnf btoc bs1k
        , bench "base32" $ whnf B32.decodeBase32 bs1k
        ]
      , bgroup "10k"
        [ bench "memory" $ whnf btoc bs10k
        , bench "base32" $ whnf B32.decodeBase32 bs10k
        ]
      , bgroup "100k"
        [ bench "memory" $ whnf btoc bs100k
        , bench "base32" $ whnf B32.decodeBase32 bs100k
        ]
      , bgroup "1mm"
        [ bench "memory" $ whnf btoc bs1mm
        , bench "base32" $ whnf B32.decodeBase32 bs1mm
        ]
      ]
    ]
  where
    ctob :: ByteString -> ByteString
    ctob = Mem.convertToBase Mem.Base32
    btoc :: ByteString -> Either String ByteString
    btoc = Mem.convertFromBase Mem.Base32

    bs = do
      a <- random 25
      b <- random 100
      c <- random 1000
      d <- random 10000
      e <- random 100000
      f <- random 1000000
      return (a,b,c,d,e,f)

    bs' = do
      a <- B32.encodeBase32' <$> random 25
      b <- B32.encodeBase32' <$> random 100
      c <- B32.encodeBase32' <$> random 1000
      d <- B32.encodeBase32' <$> random 10000
      e <- B32.encodeBase32' <$> random 100000
      f <- B32.encodeBase32' <$> random 1000000
      return (a,b,c,d,e,f)
