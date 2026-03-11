module Main where

import Data.Function
import qualified Data.Text.Builder.Linear
import qualified Data.Text.Encoding as D
import qualified Data.Text.Lazy as C
import qualified Data.Text.Lazy.Builder as B
import Test.Tasty.Bench
import qualified TextBuilderCore as A
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup "Features" features,
      bgroup "Competition" competition
    ]
  where
    features =
      [ bench "string" $ whnf (A.toText . A.string) "фывапролдж",
        bench "text" $ whnf (A.toText . A.text) "фывапролдж",
        bench "lazyText" $ whnf (A.toText . A.lazyText) "фывапролдж",
        bench "char" $ whnf (A.toText . A.char) 'ф',
        bench "unicodeCodepoint" $ whnf (A.toText . A.unicodeCodepoint) 1092
      ]

    competition =
      [ bgroup "Left-biased mappend" $ byConcat $ foldl' (<>) mempty,
        bgroup "Right-biased mappend" $ byConcat $ foldl' (flip (<>)) mempty,
        bgroup "mconcat" $ byConcat $ mconcat
      ]
      where
        byConcat (concat :: forall a. (Monoid a) => [a] -> a) =
          [ bgroup "100B" $ byTexts $ replicate 10 "фывапролдж",
            bgroup "1kB" $ byTexts $ replicate 100 "фывапролдж",
            bgroup "10kB" $ byTexts $ replicate 1_000 "фывапролдж",
            bgroup "100kB" $ byTexts $ replicate 10_000 "фывапролдж"
          ]
          where
            byTexts texts =
              [ bench
                  "TextBuilderCore.TextBuilder"
                  ( whnf
                      (A.toText . concat)
                      (fmap A.text texts)
                  ),
                bench
                  "Data.Text.Encoding.StrictTextBuilder"
                  ( whnf
                      (D.strictBuilderToText . concat)
                      (fmap D.textToStrictBuilder texts)
                  ),
                bench
                  "Data.Text.Lazy.Builder.Builder"
                  ( whnf
                      (C.toStrict . B.toLazyText . concat)
                      (fmap B.fromText texts)
                  ),
                bench
                  "Data.Text.Text"
                  ( whnf
                      concat
                      texts
                  ),
                bench
                  "Data.Text.Lazy.Text"
                  ( whnf
                      (C.toStrict . concat)
                      (fmap C.fromStrict texts)
                  ),
                bench
                  "Data.Text.Builder.Linear"
                  ( whnf
                      (Data.Text.Builder.Linear.runBuilder . concat)
                      (fmap Data.Text.Builder.Linear.fromText texts)
                  )
              ]
