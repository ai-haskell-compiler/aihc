{-# OPTIONS_GHC -Wno-orphans #-}

module ArbitraryInstances () where

import Data.HashMap.Strict qualified as HM
import RichEnv.Types (Mappings (..), Prefixes (..), RichEnv (..), Values (..))
import Test.QuickCheck (Arbitrary (arbitrary), Gen)
import Test.QuickCheck.Instances.Text ()

instance Arbitrary RichEnv where
  arbitrary :: Gen RichEnv
  arbitrary = RichEnv <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Values where
  arbitrary :: Gen Values
  arbitrary = Values . HM.fromList <$> arbitrary

instance Arbitrary Mappings where
  arbitrary :: Gen Mappings
  arbitrary = Mappings . HM.fromList <$> arbitrary

instance Arbitrary Prefixes where
  arbitrary :: Gen Prefixes
  arbitrary = Prefixes . HM.fromList <$> arbitrary
