module Exon.Test.ParseTest where

import Hedgehog (TestT, assert)
import Exon.Parse (parse)

test_parser :: TestT IO ()
test_parser = do
  assert (isLeft (parse "pre #{incomplete"))
  assert (isLeft (parse "pre ##{incomplete"))
