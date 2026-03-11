module Exon.Test.ShowsPrecTest where

import Hedgehog (TestT, (===))

import Exon.Quote (exon)

data Value =
  Value {
    x :: Int,
    y :: Int
  }
  deriving stock (Eq, Show)

data Record =
  Record {
    number :: Int,
    maybeNumber :: Maybe Int,
    value :: Value
  }
  deriving stock (Eq)

data Wrap =
  Wrap Int Record
  deriving stock (Eq, Show)

instance Show Record where
  showsPrec d Record {..} =
    showParen (d > 10) $
      [exon|Record #{showsPrec 11 number} #{showsPrec 11 maybeNumber} #{showsPrec 11 value}|]

test_showsPrec :: TestT IO ()
test_showsPrec =
  "Wrap 1 (Record 5 (Just 10) (Value {x = 3, y = 4}))" === show @Text (Wrap 1 (Record 5 (Just 10) (Value 3 4)))
