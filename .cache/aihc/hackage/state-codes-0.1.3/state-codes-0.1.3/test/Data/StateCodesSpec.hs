module Data.StateCodesSpec where

import           Data.Aeson
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Data.StateCodes

spec :: Spec
spec =
  describe "StateCode" $ do
    prop "fromName . toName" $ forAll arbitraryBoundedEnum $ \code ->
      (fromName . toName) code == (code :: StateCode)

    prop "fromText . toText" $ forAll arbitraryBoundedEnum $ \code ->
      (fromText . toText) code == (code :: StateCode)

    prop "decode . encode" $ forAll arbitraryBoundedEnum $ \code ->
      (decode . encode) code == Just (code :: StateCode)
