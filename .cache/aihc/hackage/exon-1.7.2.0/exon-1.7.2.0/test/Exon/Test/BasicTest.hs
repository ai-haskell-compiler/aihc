module Exon.Test.BasicTest where

import Data.Text (toUpper)
import Hedgehog (TestT, (===))

import Exon.Class.Exon (ExonSegment (exonSegment))
import Exon.Data.Result (Result (Result))
import qualified Exon.Data.Segment as Segment
import Exon.Quote (exon, exonws)
import Exon.SkipWs (SkipWs (SkipWs), intron, skipWs)

newtype Mon =
  Mon String
  deriving stock (Eq, Show)
  deriving newtype (IsString)

instance Semigroup Mon where
  Mon l <> Mon r =
    Mon (l <> r)

instance Monoid Mon where
  mempty =
    Mon ""

newtype Name =
  Name Text
  deriving newtype (Eq, Show, Monoid, IsString)

instance Semigroup Name where
  Name l <> Name r =
    Name (l <> " | " <> r)

lastName :: Name
lastName = "Fry"

up :: Name -> Name
up (Name name) =
  Name (toUpper name)

test_basic :: TestT IO ()
test_basic = do
  Mon "fooandbar" === [intron|foo
    #{var} bar|]
  "Philip | J. | FRY" === [intron|Philip J. #{up lastName}|]
  "Philip | J. | FRY" === skipWs [exonws|Philip J. #{SkipWs (up lastName)}|]
  "Philip J. | FRY" === [exon|Philip J.#{up lastName}|]
  ("abc" :: Text) === skipWs [exonws|a  ##{"b" :: Text} #{"c"}|]
  ("one #" :: Text) === [exon|one #|]
  ("one ##bug} two three" :: Text) === [exon|one ##bug} two #{"three"}|]
  where
    var :: IsString a => a
    var =
      "and"

newtype Thing =
  Thing String
  deriving stock (Generic)
  deriving newtype (IsString, Show, Eq)

instance ExonSegment Thing inner String where
  exonSegment builder = \case
    Segment.String s -> Result s
    Segment.Expression thing -> Result (builder thing)
    Segment.Whitespace _ -> Result " >>> "

test_customWhitespace :: TestT IO ()
test_customWhitespace =
  Thing "1 >>> 2 >>> 3" === [exonws|1 #{Thing "2"}    3|]
