{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.List (nub)

import Data.Voting.BordaCount

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "duplicate checks" $
        prop "properly detects duplicates" $ \(xs :: [Int]) ->
            let dup = length xs /= length (nub xs) in
            case findFirstDuplicateBy (==) xs of
                Just (x, _) -> length (filter (== x) xs) > 1
                Nothing     -> not dup

    describe "ballot" $ do
        it "rejects duplicate options" $ do
            ballot "" [Vote 0 "a", Vote 2 "b", Vote 1 "a"] `shouldBe` Left (DuplicateOption "a")
            ballot "" [Vote 0 "a", Vote 2 "a", Vote 1 "a"] `shouldBe` Left (DuplicateOption "a")
            ballot "" [Vote 0 "b", Vote 2 "a", Vote 1 "a"] `shouldBe` Left (DuplicateOption "a")

        it "rejects duplicate rankings" $ do
            ballot "" [Vote 0 "a", Vote 2 "b", Vote 2 "c"] `shouldBe` Left (DuplicateRanking "b" "c")
            ballot "" [Vote 0 "a", Vote 3 "b", Vote 3 "c"] `shouldBe` Left (DuplicateRanking "b" "c")

        it "accepts valid ballots" $ do
            ballot "" [Vote 0 "a", Vote 0 "b", Vote 1 "c", Vote 4 "d"] `shouldSatisfy` anyRight
            ballot "" ([] :: [Vote String])                            `shouldSatisfy` anyRight
            ballot "" [Vote 1 "c", Vote 2 "d", Vote 0 "x"]             `shouldSatisfy` anyRight

    describe "election" $ do
        it "rejects duplicate participants" $
            election' [Ballot "henri"  [Vote 0 "x"],
                       Ballot "viktor" [Vote 1 "x"],
                       Ballot "henri"  [Vote 0 "x"]] `shouldBe` Left (DuplicateParticipant "henri")

        let ballots = [Ballot "henri"  [Vote 0 "x", Vote 1 "y", Vote 2 "z"],
                       Ballot "viktor" [Vote 1 "x", Vote 2 "y", Vote 3 "z"],
                       Ballot "arno"   [Vote 0 "x", Vote 0 "y", Vote 1 "z"]]

        it "accepts valid ballot (uniform weight)" $
            election' ballots `shouldBe` Right [
                Result "x" 1 2
              , Result "y" 3 1
              , Result "z" 6 0]

        it "accepts valid ballot (weighted)" $
            let w "henri"  = 5.0
                w "viktor" = 2.0
                w "arno"   = 1.0
                w p        = error $ "unknown participant: " ++ p
            in
            election w ballots `shouldBe` Right [
                Result "x" 2  6
              , Result "y" 9  1
              , Result "z" 17 0]
  where
    anyRight (Left _)  = False
    anyRight (Right _) = True
