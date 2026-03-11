{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.Hspec
import Test.QuickCheck

import Data.Functor.Identity
import Data.Maybe (maybeToList)

import Data.SuchThat

main :: IO ()
main = hspec $ do
  describe "Data.SuchThat.ambiguate" $ do
    it "ambiguates a [Int] to a `SuchThat '[Show] []` such that we can show it and get the string rep" $
      property $ \(xs :: [Int]) -> ambiguously (map show) (ambiguate xs :: SuchThat '[Show] []) `shouldBe` map show xs
    it "ambiguates a [Int] to a `SuchThat '[Num] []` such that we can still sum the list inside" $
      property $ \(xs :: [Int]) -> ambiguously (show . sum) (ambiguate xs :: SuchThat '[Show,Num] []) `shouldBe` show (sum xs)
  describe "Data.SuchThat.ambiguously" $
    it "disambiguates using the function provided" $
      property $ \(x :: Int) -> ambiguously ((*5) . runIdentity) (some x :: Satisfies ((~) Int)) `shouldBe` 5 * x
  describe "Data.SuchThat.some" $ do
    it "cancels with `ambiguously runIdentity` when using `Satisfies ((~) x)`" $
      property $ \x -> ambiguously runIdentity (some x :: Satisfies ((~) Int)) `shouldBe` x
    it "is the same as using Identity directly" $
      ambiguously show (some 'a' :: Satisfies Show) `shouldBe` ambiguously show (ambiguate (Identity 'a') :: Satisfies Show)
  describe "Data.SuchThat.discoverStructure" $ do
    it "gives back the correct length of unit list when provided a list" $
      property $ \x -> discoverStructure (ambiguate (replicate x 'a') :: ForAny []) `shouldBe` replicate x ()
    it "discovers the correct side of an Either" $
      property $ \(x :: Either () ()) -> discoverStructure (ambiguate x :: ForAny (Either ())) `shouldBe` x
  describe "Data.SuchThat.naturalTransformation" $ do
    it "correctly applies the eitherUnitToMaybe natural transformation" $
      property $ \x -> discoverStructure (naturalTransformation eitherUnitToMaybe (ambiguate x :: ForAny (Either ()))) `shouldBe` case x of {Left () -> Nothing; Right x' -> Just x'}
    it "correctly applies the maybeToList natural transformation" $
      property $ \x -> discoverStructure (naturalTransformation maybeToList (ambiguate x :: ForAny Maybe)) `shouldBe` case x of {Nothing -> [];Just x' -> [x']}
  describe "Data.SuchThat.unNaturalTransformation" $ do
    it "inverts naturalTransformation" $ 
      property $ \(x :: Maybe ()) -> (unNaturalTransformation (naturalTransformation maybeToList)) x `shouldBe` maybeToList x


-- Here is a natural isomorphism between `Either () x` and `Maybe x`, since both of them are just `x + 1`
eitherUnitToMaybe :: forall x. Either () x -> Maybe x
eitherUnitToMaybe (Left ()) = Nothing
eitherUnitToMaybe (Right x) = Just x

maybeToEitherUnit :: forall x. Maybe x -> Either () x
maybeToEitherUnit Nothing = Left ()
maybeToEitherUnit (Just x) = Right x



