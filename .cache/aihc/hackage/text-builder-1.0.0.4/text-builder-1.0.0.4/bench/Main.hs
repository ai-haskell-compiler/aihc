module Main where

import Data.Function
import qualified Data.Text as E
import qualified Data.Text.Builder.Linear
import qualified Data.Text.Encoding as D
import qualified Data.Text.Lazy as C
import qualified Data.Text.Lazy.Builder as B
import Test.Tasty.Bench
import qualified TextBuilder as A
import Prelude

main :: IO ()
main =
  defaultMain
    [ bgroup "Competition" competition,
      bgroup "Features" features
    ]
  where
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
                  "TextBuilder.TextBuilder"
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

    features =
      [ bgroup "intercalate" $
          [ bench "TextBuilder" $ whnf (A.toText . A.intercalate "фывапролдж") ["a", "b", "c", "d", "e", "f"],
            bench "Text" $ whnf (E.intercalate "фывапролдж") ["a", "b", "c", "d", "e", "f"]
          ],
        bench "binary" $ whnf (A.toText . A.binary) (123456 :: Int),
        bench "octal" $ whnf (A.toText . A.octal) (123456 :: Int),
        bench "decimal" $ whnf (A.toText . A.decimal) (123456 :: Int),
        bench "hexadecimal" $ whnf (A.toText . A.hexadecimal) (123456 :: Int)
      ]
