module Data.TrieSpec (main, spec) where

import Control.Applicative ((<$>))
import Data.List (nub, sort)
import Data.Trie
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

newtype T = T Trie deriving (Show, Eq)

instance Arbitrary T where
  arbitrary = anything >>= \s -> return . T $ insert s empty

-- Just for the 0-255 range.
newtype ASCII = ASCII Trie deriving (Show, Eq)
newtype Anything = Anything { _unA :: String } deriving (Show, Eq)

instance Arbitrary Anything where
  arbitrary = Anything <$> anything

instance Arbitrary ASCII where
  arbitrary = arbitrary >>= \s -> return . ASCII $ insert s empty


-- | Generates chars from the full allowed range then sticks them
-- together. This lets us check that our tree works for any garbage
-- thrown at it.
anything :: Gen String
anything = listOf $ choose (minBound, maxBound)

main :: IO ()
main = hspec spec

-- Resizes a property to 10000 runs
size :: Spec -> Spec
size = modifyMaxSuccess (const 10000)

spec :: Spec
spec = do
  describe "fromList . toList ≡ id" $ do
    size $ prop "anything" $ \(T x) ->
      fromList (toList x) `shouldBe` x

    size $ prop "ASCII" $ \(ASCII x) ->
      fromList (toList x) `shouldBe` x

  describe "sort . nub . toList . fromList ≡ sort . nub" $ do
    size $ prop "anything" $ \xs ->
      let ys = sort . nub $ map _unA xs
      in toList (fromList ys) `shouldBe` ys

    size $ prop "ASCII" $ \xs ->
      let ys = sort $ nub xs
      in toList (fromList ys) `shouldBe` ys

  describe "toList (fromString x) ≡ [x]" $ do
    size $ prop "anything" $ \(Anything x) ->
      toList (fromString x) `shouldBe` [x]

    size $ prop "ASCII" $ \x ->
      toList (fromString x) `shouldBe` [x]
