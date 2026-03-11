{-# LANGUAGE FlexibleInstances #-}

module Test.Data.Radius.ArbitrariesNoVSA (
  EmptyVSA (),
  ) where

import Test.QuickCheck (Arbitrary (..), oneof, elements)

import Test.Data.Radius.ArbitrariesBase
  (genSizedString, genAtText, genAtString, genPacket)

import Control.Applicative ((<$>), pure, (<*>))
import qualified Data.Set as Set

import Data.Radius.Scalar
  (AtText (..), AtString (..), AtInteger (..))
import Data.Radius.Packet (Packet)
import Data.Radius.Attribute
  (NumberAbstract (..), Attribute' (..), Attribute (..), TypedNumberSets (..),
   numbersText, numbersString, numbersInteger)
import qualified Data.Radius.StreamPut as Put


data EmptyVSA

instance Eq EmptyVSA where
  _ == _  =  True

instance Ord EmptyVSA where
  _ `compare` _  =  EQ

instance Show EmptyVSA where
  show _  =  "<EmptyVSA>"


instance TypedNumberSets EmptyVSA where
  attributeNumbersText     = numbersText
  attributeNumbersString   = numbersString
  attributeNumbersInteger  = numbersInteger
  attributeNumbersIpV4     = Set.empty


instance Arbitrary (NumberAbstract EmptyVSA) where
  arbitrary = oneof [ Standard <$> arbitrary ]

instance Arbitrary (Attribute' EmptyVSA) where
  arbitrary =
    Attribute' . Standard <$> arbitrary <*> genSizedString (255 - 1 - 1)


instance Arbitrary (Attribute EmptyVSA AtText) where
  arbitrary =
    Attribute
    <$> elements (Set.toList numbersText)
    <*> genAtText (255 - 1 - 1)

instance Arbitrary (Attribute EmptyVSA AtString) where
  arbitrary =
    Attribute
    <$> elements (Set.toList numbersString)
    <*> genAtString (255 - 1 - 1)

instance Arbitrary (Attribute EmptyVSA AtInteger) where
  arbitrary =
    Attribute
    <$> elements (Set.toList numbersInteger)
    <*> arbitrary

{-
-- attributeNumbersIpV4 is empty list
instance Arbitrary (Attribute AtIpV4) where
  arbitrary =
    Attribute
    <$> elements (Set.toList attributeNumbersIpV4)
    <*> arbitrary
 -}

instance Arbitrary (Packet [Attribute' EmptyVSA]) where
  arbitrary = genPacket $ Put.attribute' $ \_ _ -> pure ()
