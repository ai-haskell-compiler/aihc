module RichEnv.SettersSpec (spec) where

import Data.HashMap.Strict qualified as HM
import RichEnv.Setters (mappingsToValues, prefixesToValues)
import RichEnv.Types (Environment, Mappings (..), Prefixes (..), Values (..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "mappings" $ do
    it "returns an empty environment when given an empty set" $
      mappingsToValues mempty mempty
        `shouldBe` mempty
    it "remaps a single environment variable" $
      mappingsToValues exampleEnv (Mappings $ HM.singleton "SOME" "FOO")
        `shouldBe` Values (HM.singleton "SOME" "bar")
  describe "prefixes" $ do
    it "returns an empty environment when given an empty set" $
      prefixesToValues mempty mempty
        `shouldBe` mempty
    it "remaps a single environment variable" $
      prefixesToValues exampleEnv (Prefixes $ HM.singleton "NEW_" ["PREFIXED_"])
        `shouldBe` Values (HM.fromList [("NEW_VAR", "content"), ("NEW_VAR2", "content2")])
    it "passes all environment variables adding a prefix" $
      prefixesToValues exampleEnv (Prefixes $ HM.singleton "NEWPREFIX_" [""])
        `shouldBe` Values (HM.fromList [("NEWPREFIX_FOO", "bar"), ("NEWPREFIX_BAZ", "qux"), ("NEWPREFIX_PREFIXED_VAR", "content"), ("NEWPREFIX_PREFIXED_VAR2", "content2")])
    it "remaps prefixed environment variables removing the prefix" $
      prefixesToValues exampleEnv (Prefixes $ HM.singleton "" ["PREFIXED_"])
        `shouldBe` Values (HM.fromList [("VAR", "content"), ("VAR2", "content2")])
    it "passes only prefixed environment variables preserving the prefix" $
      prefixesToValues exampleEnv (Prefixes $ HM.singleton "PREFIXED_" ["PREFIXED_"])
        `shouldBe` Values (HM.fromList [("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")])
    it "passes all environment variables" $
      prefixesToValues exampleEnv mempty
        `shouldBe` Values mempty
    it "passes multiple prefixes to multiple new prefixes" $
      prefixesToValues exampleEnv (Prefixes $ HM.fromList [("NEWPREFIX_", [""]), ("", ["PREFIXED_"])])
        `shouldBe` Values (HM.fromList [("NEWPREFIX_FOO", "bar"), ("NEWPREFIX_BAZ", "qux"), ("NEWPREFIX_PREFIXED_VAR", "content"), ("NEWPREFIX_PREFIXED_VAR2", "content2"), ("VAR", "content"), ("VAR2", "content2")])
    it "passes multiple prefixes to a single new prefix" $
      prefixesToValues exampleEnv (Prefixes $ HM.fromList [("NEWPREFIX_", ["BA", "PREFIXED_"])])
        `shouldBe` Values (HM.fromList [("NEWPREFIX_Z", "qux"), ("NEWPREFIX_VAR", "content"), ("NEWPREFIX_VAR2", "content2")])
  where
    exampleEnv :: Environment
    exampleEnv = [("FOO", "bar"), ("BAZ", "qux"), ("PREFIXED_VAR", "content"), ("PREFIXED_VAR2", "content2")]
