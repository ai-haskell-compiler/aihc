{-# LANGUAGE OverloadedStrings #-}

module PrettySpec where

import SpecHelper
import qualified Data.Map

sIdSimple = MetricId "name" "help" mempty
sIdLabels = MetricId "name" "help" (Data.Map.fromList [("a", "b")])
sC  = Counter 666
sG  = Gauge 123.456

spec :: Spec
spec = do
  it "pretty prints ID with labels" $ do
    prettyId sIdLabels `shouldBe` "name{a=\"b\"}"

  it "pretty prints simple ID" $ do
    prettyId sIdSimple `shouldBe` "name"

  it "pretty prints full metric with help and type" $ do
    prettyMetric sIdLabels sC `shouldBe` "# HELP name help\n# TYPE name counter\nname{a=\"b\"} 666.0\n"

  it "pretty prints metric" $ do
    prettyMetricShort sIdLabels sG `shouldBe` "name{a=\"b\"} 123.456"

main :: IO ()
main = do
  hspec spec
