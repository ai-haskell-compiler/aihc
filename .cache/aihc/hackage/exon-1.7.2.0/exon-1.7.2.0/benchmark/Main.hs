module Main where

import Criterion.Main (bench, bgroup, defaultMain, whnf)
import qualified Data.Text.Lazy.Builder as Text
import Data.Text.Lazy.Builder (toLazyText)
import Exon.Class.Exon (exonProcess)
import Exon.Data.Segment (Segment)
import qualified Exon.Data.Segment as Segment (Segment (Expression))

newtype Str =
  Str Text
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

newtype Nam =
  Nam Str
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString)

s2 :: IsString a => a
s2 =
  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

segments :: NonEmpty (Segment Nam)
segments =
  (const (Segment.Expression s2)) <$> [1 :: Int .. 1000]

strings :: [Text]
strings =
  replicate 1000 s2

bench_manual :: [Text] -> Text
bench_manual s =
  toStrict (toLazyText (foldMap Text.fromText s))

main :: IO ()
main =
  defaultMain [
    bgroup "exon" [
      bench "build" (whnf exonProcess segments),
      bench "manual" (whnf bench_manual strings)
    ]
  ]
