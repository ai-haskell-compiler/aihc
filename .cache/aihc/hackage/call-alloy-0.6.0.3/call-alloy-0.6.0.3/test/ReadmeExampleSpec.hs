{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module ReadmeExampleSpec (spec) where

import Data.FileEmbed                   (embedStringFile)
import Data.Map                         (Map)
import Data.Set                         (Set)
import Test.Hspec

import Language.Alloy.Call              (getInstances)
import ReadmeExample                    (instanceToNames)

import Language.Alloy.Types (
  Annotation (..),
  Entry (..),
  Object (..),
  Relation (..),
  Signature (..),
  )

deriving instance Eq (Entry Map Set)
deriving instance Eq (Relation Set)
deriving instance Read Annotation
deriving instance Read (Entry Map Set)
deriving instance Read Object
deriving instance Read (Relation Set)
deriving instance Read Signature
deriving instance Show (Entry Map Set)
deriving instance Show (Relation Set)

spec :: Spec
spec =
  describe "readme example" $ do
    it "returns expected instance" $
      getFirstInstance
        `shouldReturn`
        read $(embedStringFile "test/unit/readmeExampleInstance.hs")
    it "returns expected result" $ do
      i <- getFirstInstance
      instanceToNames i
        `shouldReturn`
        read $(embedStringFile "test/unit/readmeExampleResult.hs")
  where
    getFirstInstance = do
      i:_ <- getInstances
        (Just 1)
        $(embedStringFile "test/unit/readmeExampleSpecification.als")
      pure i
