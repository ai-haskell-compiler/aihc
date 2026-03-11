{-# LANGUAGE PackageImports #-}
module Main
( main
)where


import Criterion
import Criterion.Main

import Data.Base16.Types
import Data.ByteString
import Data.ByteString.Short
import "base16" Data.ByteString.Base16 as B16
import "base16" Data.ByteString.Short.Base16 as BS16
import "base16-bytestring" Data.ByteString.Base16 as Bos
import Data.ByteString.Random (random)


main :: IO ()
main =
  defaultMain
    [ env bs $ \ ~((bs25,bs100,bs1k,bs10k,bs100k,bs1mm),(bs25L,bs100L,bs1kL,bs10kL,bs100kL,bs1mmL)) ->
      bgroup "encode"
      [ bgroup "25"
        [ bench "base16-short" $ whnf BS16.encodeBase16' bs25L
        , bench "base16-bytestring" $ whnf Bos.encode bs25
        , bench "base16" $ whnf B16.encodeBase16' bs25
        ]
      , bgroup "100"
        [ bench "base16-short" $ whnf BS16.encodeBase16' bs100L
        , bench "base16-bytestring" $ whnf Bos.encode bs100
        , bench "base16" $ whnf B16.encodeBase16' bs100
        ]
      , bgroup "1k"
        [ bench "base16-short" $ whnf BS16.encodeBase16' bs1kL
        , bench "base16-bytestring" $ whnf Bos.encode bs1k
        , bench "base16" $ whnf B16.encodeBase16' bs1k
        ]
      , bgroup "10k"
        [ bench "base16-short" $ whnf BS16.encodeBase16' bs10kL
        , bench "base16-bytestring" $ whnf Bos.encode bs10k
        , bench "base16" $ whnf B16.encodeBase16' bs10k
        ]
      , bgroup "100k"
        [ bench "base16-short" $ whnf BS16.encodeBase16' bs100kL
        , bench "base16-bytestring" $ whnf Bos.encode bs100k
        , bench "base16" $ whnf B16.encodeBase16' bs100k
        ]
      , bgroup "1mm"
        [ bench "base16-short" $ whnf BS16.encodeBase16' bs1mmL
        , bench "base16-bytestring" $ whnf Bos.encode bs1mm
        , bench "base16" $ whnf B16.encodeBase16' bs1mm
        ]
      ]
    , env bs' $ \ ~((bs25,bs100,bs1k,bs10k,bs100k,bs1mm),(bs25L,bs100L,bs1kL,bs10kL,bs100kL,bs1mmL)) ->
      bgroup "decode"
      [ bgroup "25"
        [ bench "base16-short" $ whnf BS16.decodeBase16 bs25L
        , bench "base16-bytestring" $ whnf Bos.decode $ extractBase16 bs25
        , bench "base16" $ whnf B16.decodeBase16 bs25
        , bench "base16-untyped" $ whnf B16.decodeBase16Untyped $ extractBase16 bs25
        ]
      , bgroup "100"
        [ bench "base16-short" $ whnf BS16.decodeBase16 bs100L
        , bench "base16-bytestring" $ whnf Bos.decode $ extractBase16 bs100
        , bench "base16" $ whnf B16.decodeBase16 bs100
        , bench "base16-untyped" $ whnf B16.decodeBase16Untyped $ extractBase16 bs100
        ]
      , bgroup "1k"
        [ bench "base16-short" $ whnf BS16.decodeBase16 bs1kL
        , bench "base16-bytestring" $ whnf Bos.decode $ extractBase16 bs1k
        , bench "base16" $ whnf B16.decodeBase16 bs1k
        , bench "base16-untyped" $ whnf B16.decodeBase16Untyped $ extractBase16 bs1k
        ]
      , bgroup "10k"
        [ bench "base16-short" $ whnf BS16.decodeBase16 bs10kL
        , bench "base16-bytestring" $ whnf Bos.decode $ extractBase16 bs10k
        , bench "base16" $ whnf B16.decodeBase16 bs10k
        , bench "base16-untyped" $ whnf B16.decodeBase16Untyped $ extractBase16 bs10k
        ]
      , bgroup "100k"
        [ bench "base16-short" $ whnf BS16.decodeBase16 bs100kL
        , bench "base16-bytestring" $ whnf Bos.decode $ extractBase16 bs100k
        , bench "base16" $ whnf B16.decodeBase16 bs100k
        , bench "base16-untyped" $ whnf B16.decodeBase16Untyped $ extractBase16 bs100k
        ]
      , bgroup "1mm"
        [ bench "base16-short" $ whnf BS16.decodeBase16 bs1mmL
        , bench "base16-bytestring" $ whnf Bos.decode $ extractBase16 bs1mm
        , bench "base16" $ whnf B16.decodeBase16 bs1mm
        , bench "base16-untyped" $ whnf B16.decodeBase16Untyped $ extractBase16 bs1mm
        ]
      ]
    ]
  where
    bs = do
      a <- random 25
      b <- random 100
      c <- random 1000
      d <- random 10000
      e <- random 100000
      f <- random 1000000
      return ((a,b,c,d,e,f),(toShort a,toShort b,toShort c,toShort d,toShort e,toShort f))

    bs' = do
      let k = fmap toShort
      a <- B16.encodeBase16' <$> random 25
      b <- B16.encodeBase16' <$> random 100
      c <- B16.encodeBase16' <$> random 1000
      d <- B16.encodeBase16' <$> random 10000
      e <- B16.encodeBase16' <$> random 100000
      f <- B16.encodeBase16' <$> random 1000000
      return ((a,b,c,d,e,f),(k a,k b,k c,k d,k e,k f))
