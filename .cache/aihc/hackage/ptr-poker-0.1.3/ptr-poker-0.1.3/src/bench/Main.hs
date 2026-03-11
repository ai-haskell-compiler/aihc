module Main where

import Criterion.Main
import qualified Data.Text.Encoding as Text
import qualified PtrPoker.Write as Write
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup
        "bWord32"
        [ bench "4"
            $ nf
              ( \(a, b, c, d) ->
                  Write.toByteString
                    $ Write.bWord32 a
                    <> Write.bWord32 b
                    <> Write.bWord32 c
                    <> Write.bWord32 d
              )
              (1, 2, 3, 4)
        ],
      bgroup "textUtf8"
        $ let latinSampleBySize size =
                enumFromTo 'a' 'z'
                  & replicate size
                  & concat
                  & fromString
              greekSampleBySize size =
                enumFromTo 'Α' 'Ω'
                  & replicate size
                  & concat
                  & fromString
              !latinSample1 = latinSampleBySize 1
              !latinSample10 = latinSampleBySize 10
              !latinSample100 = latinSampleBySize 100
              !greekSample1 = greekSampleBySize 1
              !greekSample10 = greekSampleBySize 10
              !greekSample100 = greekSampleBySize 100
           in [ bgroup
                  "ptr-poker"
                  [ bgroup
                      "latin"
                      [ bench "1" (nf (Write.toByteString . Write.textUtf8) latinSample1),
                        bench "10" (nf (Write.toByteString . Write.textUtf8) latinSample10),
                        bench "100" (nf (Write.toByteString . Write.textUtf8) latinSample100)
                      ],
                    bgroup
                      "greek"
                      [ bench "1" (nf (Write.toByteString . Write.textUtf8) greekSample1),
                        bench "10" (nf (Write.toByteString . Write.textUtf8) greekSample10),
                        bench "100" (nf (Write.toByteString . Write.textUtf8) greekSample100)
                      ]
                  ],
                bgroup
                  "text"
                  [ bgroup
                      "latin"
                      [ bench "1" (nf Text.encodeUtf8 latinSample1),
                        bench "10" (nf Text.encodeUtf8 latinSample10),
                        bench "100" (nf Text.encodeUtf8 latinSample100)
                      ],
                    bgroup
                      "greek"
                      [ bench "1" (nf Text.encodeUtf8 greekSample1),
                        bench "10" (nf Text.encodeUtf8 greekSample10),
                        bench "100" (nf Text.encodeUtf8 greekSample100)
                      ]
                  ]
              ]
    ]
