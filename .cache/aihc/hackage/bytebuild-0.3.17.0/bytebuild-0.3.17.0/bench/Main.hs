{-# LANGUAGE OverloadedStrings #-}

import Data.Primitive (ByteArray)
import Data.Word (Word64)
import Gauge (bench, bgroup, whnf)
import Gauge.Main (defaultMain)

import qualified Arithmetic.Nat as Nat
import qualified Data.Bytes.Builder as B
import qualified Data.Bytes.Builder.Bounded as U

import qualified Cell
import qualified HexWord64
import qualified SimpleCsv
import qualified Word16Tree

main :: IO ()
main =
  defaultMain
    [ bgroup
        "w64"
        [ bgroup
            "hex"
            [ bench "library" (whnf encodeHexWord64s w64s)
            , bench "loop" (whnf encodeHexWord64sLoop w64s)
            ]
        ]
    , bgroup
        "unbounded"
        [ bench "csv-no-escape" $
            whnf
              (\x -> B.run 4080 (SimpleCsv.encodeRows x))
              Cell.cells
        , bench "word-16-tree-small" $
            whnf
              (\x -> B.run 4080 (Word16Tree.encode x))
              Word16Tree.exampleSmall
        , bench "word-16-tree-2000" $
            whnf
              (\x -> B.run ((4096 * 16) - 16) (Word16Tree.encode x))
              Word16Tree.example2000
        , bench "word-16-tree-9000" $
            whnf
              (\x -> B.run ((4096 * 64) - 16) (Word16Tree.encode x))
              Word16Tree.example9000
        ]
    ]

w64s :: Word64s
w64s =
  Word64s
    0xde2b8a480cf77113
    0x48f1668ca2a68b45
    0xd262fbaa0b2f473c
    0xbab20547f4919d9f
    0xb7ec16121704db43
    0x9c259f5bfa90e1eb
    0xd451eca11d9873ad
    0xbd927e8d4c879d02

data Word64s
  = Word64s
      !Word64
      !Word64
      !Word64
      !Word64
      !Word64
      !Word64
      !Word64
      !Word64

encodeHexWord64s :: Word64s -> ByteArray
{-# NOINLINE encodeHexWord64s #-}
encodeHexWord64s (Word64s a b c d e f g h) =
  U.run Nat.constant $
    U.word64PaddedUpperHex a
      `U.append` U.word64PaddedUpperHex b
      `U.append` U.word64PaddedUpperHex c
      `U.append` U.word64PaddedUpperHex d
      `U.append` U.word64PaddedUpperHex e
      `U.append` U.word64PaddedUpperHex f
      `U.append` U.word64PaddedUpperHex g
      `U.append` U.word64PaddedUpperHex h

encodeHexWord64sLoop :: Word64s -> ByteArray
{-# NOINLINE encodeHexWord64sLoop #-}
encodeHexWord64sLoop (Word64s a b c d e f g h) =
  U.run Nat.constant $
    HexWord64.word64PaddedUpperHex a
      `U.append` HexWord64.word64PaddedUpperHex b
      `U.append` HexWord64.word64PaddedUpperHex c
      `U.append` HexWord64.word64PaddedUpperHex d
      `U.append` HexWord64.word64PaddedUpperHex e
      `U.append` HexWord64.word64PaddedUpperHex f
      `U.append` HexWord64.word64PaddedUpperHex g
      `U.append` HexWord64.word64PaddedUpperHex h
