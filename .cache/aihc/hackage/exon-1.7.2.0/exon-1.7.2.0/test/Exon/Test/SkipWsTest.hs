module Exon.Test.SkipWsTest where

import Hedgehog (TestT, (===))

import Exon.Quote (exon)
import Exon.SkipWs (intron)

newtype Mon =
  Mon String
  deriving stock (Eq, Show)
  deriving newtype (IsString, Semigroup, Monoid)

test_skipWs :: TestT IO ()
test_skipWs = do
  Mon "foo\n    and bar" === [exon|foo
    #{var} bar|]
  Mon "fooandbar" === [intron|foo
    #{var} bar|]
  where
    var :: IsString a => a
    var =
      "and"
