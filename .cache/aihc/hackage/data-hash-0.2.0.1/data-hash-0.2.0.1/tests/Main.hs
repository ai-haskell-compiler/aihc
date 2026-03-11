module Main where

import Control.Applicative
import Test.QuickCheck
import Data.List

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.Hash

main :: IO ()
main = defaultMain tests

instance (Arbitrary a, Hashable a) => Arbitrary (RollingHash a) where
  arbitrary = do Positive n <- arbitrary
                 foldl' addAndRoll (rollingHash n) <$> (arbitrary :: Arbitrary a => Gen [a])

foldH :: [Int] -> Hash
foldH (i:xs) = foldl combine (hash i) $ map hash xs
foldH _ = error "Cannot happen"

rhArr :: Int -> [Int] -> RollingHash Int
rhArr x xs = foldl addAndRoll (rollingHash (length (x:xs))) (x:xs)

prop_cycle :: Int -> [Int] -> Bool
prop_cycle x xs = (currentHash rh) == (currentHash $ foldl addAndRoll rh $ x:x:xs)
    where rh = rhArr x xs

prop_curr :: RollingHash Int -> Bool
prop_curr rh = currentHash rh == foldl1' combine (lastHashes rh)

prop_len1 :: RollingHash Int -> Bool
prop_len1 rh = length (lastHashes rh) <= windowSize rh

prop_len2 :: RollingHash Int -> Int -> Bool
prop_len2 rh x
  | length (lastHashes rh) == n = length (lastHashes $ addAndRoll rh x) == n
  | otherwise                   = length (lastHashes $ addAndRoll rh x) == length (lastHashes rh) + 1
  where n = windowSize rh

prop_last :: RollingHash Int -> Int -> Bool
prop_last rh x = last (lastHashes $ addAndRoll rh x) == hash x

prop_suff :: RollingHash Int -> Int -> Bool
prop_suff rh x = init (lastHashes $ addAndRoll rh x) `isSuffixOf` (lastHashes rh)


tests :: [TF.Test]
tests = [ testGroup "QuickCheck tests" [
              testProperty "curr" prop_curr
            , testProperty "len1" prop_len1
            , testProperty "len2" prop_len2
            , testProperty "last" prop_last
            , testProperty "suff" prop_suff
            , testProperty "cycle" prop_cycle
          ]
        ]
