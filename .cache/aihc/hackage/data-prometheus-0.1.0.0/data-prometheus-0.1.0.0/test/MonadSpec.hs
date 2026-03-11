{-# LANGUAGE OverloadedStrings #-}

module MonadSpec where

import SpecHelper
import Data.Function ((&))
import qualified Data.Map as M

complex :: MetricId -> MetricId
complex =
    sub "sub2"
  . sub "sub1"
  . desc "desc"
  . label "a" "b"

complexExpected :: MetricId
complexExpected = complex $ metric "test"

spec :: Spec
spec = do
  it "generates metrics" $ do
    runMetrics (metric "test") $ addMetric' id (mkCounter 7)
    `shouldBe`
    "# HELP test \n# TYPE test counter\ntest 7.0\n"

  it "generates complex metrics" $ do
    runMetrics (metric "test") $ addMetric' complex (mkCounter 7)
    `shouldBe`
    "# HELP test_sub1_sub2 desc\n# TYPE test_sub1_sub2 counter\ntest_sub1_sub2{a=\"b\"} 7.0\n"

  it "adds errors" $ do
    runMetrics (metric "test") $ addMetric' id (Counter 7) >> logError "fail"
    `shouldBe`
    "# HELP test \n# TYPE test counter\ntest 7.0\n# ERROR fail\n"

  it "roundtrips complex" $ do
    parseProm (runMetrics (metric "test") (addMetric' complex (Counter 7)))
    `shouldBe`
    Right (M.fromList [(complexExpected, Counter 7)])

  it "roundtrips complex handles errors" $ do
    parseProm (runMetrics (metric "test") (addMetric' complex (Counter 7) >> logError "nada"))
    `shouldBe`
    Right (M.fromList [(complexExpected, Counter 7)])

  it "roundtrips multiple complex" $ do
    parseProm
      (runMetrics (metric "test") $ do
        addMetric' complex (Counter 7)
        addMetric' (sub "xy" . complex) (Gauge 13.37)
      )
    `shouldBe`
    Right (M.fromList [(complexExpected, Counter 7), (complexExpected & sub "xy", Gauge 13.37)])

main :: IO ()
main = do
  hspec spec
