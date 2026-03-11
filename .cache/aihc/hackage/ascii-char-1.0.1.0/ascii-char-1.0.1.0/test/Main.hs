module Main (main) where

import ASCII.Char

import Data.Function (($))
import Data.List (length, sort)
import Data.Maybe (Maybe (..))
import Prelude (maxBound, minBound)
import System.IO (IO)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do

    describe "toInt" $ do
        let c ~> n = toInt c `shouldBe` n

        it "Null   → 0"   $ Null           ~> 0
        it "A      → 65"  $ CapitalLetterA ~> 65
        it "a      → 97"  $ SmallLetterA   ~> 97
        it "Delete → 127" $ Delete         ~> 127

    describe "toWord8" $ do
        let c ~> n = toWord8 c `shouldBe` n

        it "Null   → 0"   $ Null           ~> 0
        it "A      → 65"  $ CapitalLetterA ~> 65
        it "a      → 97"  $ SmallLetterA   ~> 97
        it "Delete → 127" $ Delete         ~> 127

    describe "fromIntMaybe" $ do
        let n ~> c = fromIntMaybe n `shouldBe` c

        it "-1  → Nothing" $ (-1) ~> Nothing
        it "0   → Null"    $ 0    ~> Just Null
        it "65  → A"       $ 65   ~> Just CapitalLetterA
        it "127 → Delete"  $ 127  ~> Just Delete
        it "128 → Nothing" $ 128  ~> Nothing

    describe "fromWord8Maybe" $ do
        let n ~> c = fromWord8Maybe n `shouldBe` c

        it "0   → Null"    $ 0    ~> Just Null
        it "65  → A"       $ 65   ~> Just CapitalLetterA
        it "127 → Delete"  $ 127  ~> Just Delete
        it "128 → Nothing" $ 128  ~> Nothing

    describe "fromIntUnsafe" $ do
        let n ~> c = fromIntUnsafe n `shouldBe` c

        it "0  → Null"    $ 0  ~> Null
        it "65 → A"       $ 65 ~> CapitalLetterA
        it "66 → B"       $ 66 ~> CapitalLetterB
        it "67 → C"       $ 67 ~> CapitalLetterC
        it "127 → Delete" $ 127  ~> Delete

    describe "fromWord8Unsafe" $ do
        let n ~> c = fromWord8Unsafe n `shouldBe` c

        it "0  → Null"    $ 0  ~> Null
        it "65 → A"       $ 65 ~> CapitalLetterA
        it "66 → B"       $ 66 ~> CapitalLetterB
        it "67 → C"       $ 67 ~> CapitalLetterC
        it "127 → Delete" $ 127  ~> Delete

    describe "allCharacters" $ do
        let x = allCharacters

        it "has 128 items" $ length x `shouldBe` 128
        it "is sorted"     $ sort x `shouldBe` x

    describe "Bounded" $ do
        it "min bound is Null"   $ minBound `shouldBe` Null
        it "max bound is Delete" $ maxBound `shouldBe` Delete
