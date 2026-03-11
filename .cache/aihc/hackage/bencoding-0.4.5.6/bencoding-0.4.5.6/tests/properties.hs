{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS  -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative as A
import Data.ByteString as BS
import Data.ByteString.Lazy as BL
import GHC.Generics
import Test.QuickCheck
import Test.Hspec

import Data.BEncode
import qualified Data.BEncode.BDict as BE

smaller :: Gen a -> Gen a
smaller = scale (* factor)
  where
    factor = 9 `div` 10

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink = fmap BS.pack . shrink . BS.unpack

instance Arbitrary a => Arbitrary (BE.BDictMap a) where
  arbitrary = frequency
    [ (90, A.pure BE.Nil)
    , (10, BE.Cons <$> arbitrary <*> arbitrary <*> arbitrary)
    ]
  shrink = genericShrink

instance Arbitrary BValue where
  arbitrary = frequency
    [ (30, BInteger <$> arbitrary)
    , (30, BString  <$> arbitrary)
    , (20, BList    <$> smaller arbitrary)
    , (20, BDict    <$> smaller arbitrary)
    ]
  shrink = genericShrink

data List a = Cons a (List a) | Nil
              deriving (Show, Eq, Generic)

instance BEncode a => BEncode (List a)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency
    [ (90, pure Nil)
    , (10, Cons <$> arbitrary <*> arbitrary)
    ]
  shrink = genericShrink

data FileInfo = FileInfo
  { _fiLength :: !Integer
  , _fiPath   :: [BS.ByteString]
  , _fiMD5Sum :: BS.ByteString
  } deriving (Show, Eq, Generic)

instance BEncode FileInfo

instance Arbitrary FileInfo where
  arbitrary = FileInfo <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

data T a = T

prop_bencodable :: Eq a => BEncode a => T a -> a -> Bool
prop_bencodable _ x = decode (BL.toStrict (encode x)) == Right x

emptyGet :: Get Int
emptyGet = A.empty

main :: IO ()
main = hspec $ do
  describe "BValue" $ do
    it "properly encoded" $ property $
       prop_bencodable (T :: T BValue)

  describe "BEncode" $ do
    it "generic recordless" $ property $
      prop_bencodable (T :: T (List Int))

    it "generic records" $ property $
      prop_bencodable (T :: T FileInfo)

  describe "Get" $ do
    it "catchable from pure code" $ do
      fromDict (fail "fatal error" :: Get Int) (BDict BE.Nil)
           `shouldBe`
       Left "fatal error"
    it "empty alternative is empty string" $ do
      fromDict emptyGet (BDict BE.Nil) `shouldBe` Left ""
    it "alternative composition" $ do
      fromDict (emptyGet <|> emptyGet) (BDict BE.Nil) `shouldBe` Left ""
      fromDict (pure 5 <|> emptyGet) (BDict BE.Nil) `shouldBe` Right 5
      fromDict (emptyGet <|> pure 3) (BDict BE.Nil) `shouldBe` Right 3
