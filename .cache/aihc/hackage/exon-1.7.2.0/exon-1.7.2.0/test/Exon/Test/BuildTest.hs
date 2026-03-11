module Exon.Test.BuildTest where

import Hedgehog (TestT, (===))

import Exon.Class.Exon (exonProcess, exonProcessWith)
import Exon.Class.ToSegment (toSegment)
import Exon.Data.Segment (Segment (Expression, Whitespace))
import Exon.SkipWs (SkipWs (SkipWs), skipWs)

newtype Str =
  Str String
  deriving stock (Eq, Show)
  deriving newtype (IsString, Semigroup, Monoid)

segments ::
  IsString a =>
  NonEmpty (Segment a)
segments =
  [
    Whitespace " ",
    "pre",
    Whitespace " ",
    Expression (toSegment "exp"),
    Whitespace "  ",
    "post",
    Whitespace " "
  ]

test_build :: TestT IO ()
test_build = do
  " pre exp  post " === exonProcess @Text segments
  "preexppost" === exonProcessWith @(SkipWs Text) SkipWs skipWs segments
  " pre exp  post " === exonProcess @Str segments
  "preexppost" === exonProcessWith @(SkipWs Str) SkipWs skipWs segments
