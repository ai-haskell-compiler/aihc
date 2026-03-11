{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.Hspec
import Test.QuickCheck

import Data.Monoid
import Data.Group
import Data.Act

import qualified Data.Semidirect.Lazy as Lazy
import qualified Data.Semidirect.Strict as Strict

newtype Days = Days Int
        deriving Show

newtype Duration = Duration Days
  deriving Show
  deriving (Semigroup, Monoid, Group) via Sum Int
  deriving (LAct Days, LActSg Days, LActMn Days, LTorsor Days)
           via (ActSelf' (Sum Int))
  deriving (RAct Days, RActSg Days, RActMn Days, RTorsor Days)
           via (ActSelf' (Sum Int))

main :: IO ()
main = hspec $ do
  describe "Semidirect" $ do
    describe "LSemidirect" $ do
      describe "Lazy" $ do
        it "Product on Sum Semigroup" $ property $
          \x s y t ->
            Lazy.LSemidirect (Sum (x :: Int)) (Product (s :: Int))
            <> Lazy.LSemidirect (Sum y) (Product t)
            `shouldBe`
            Lazy.LSemidirect (Sum (x + s*y)) (Product (s*t))
        it "Product on Sum Monoid" $
          mempty `shouldBe`
            Lazy.LSemidirect (mempty :: Sum Int) (mempty :: Product Int)
      describe "Strict" $ do
        it "Product on Sum Semigroup" $ property $
          \x s y t ->
            Strict.LSemidirect (Sum (x :: Int)) (Product (s :: Int))
            <> Strict.LSemidirect (Sum y) (Product t)
            `shouldBe`
            Strict.LSemidirect (Sum (x + s*y)) (Product (s*t))
        it "Product on Sum Monoid" $
          mempty `shouldBe`
            Strict.LSemidirect (mempty :: Sum Int) (mempty :: Product Int)
    describe "RSemidirect" $ do
      describe "Lazy" $ do
        it "Product on Sum Semigroup" $ property $
          \x s y t ->
            Lazy.RSemidirect (Sum (x :: Int)) (Product (s :: Int))
            <> Lazy.RSemidirect (Sum y) (Product t)
            `shouldBe`
            Lazy.RSemidirect (Sum (x + s*y)) (Product (s*t))
        it "Product on Sum Monoid" $
          mempty `shouldBe`
            Lazy.RSemidirect (mempty :: Sum Int) (mempty :: Product Int)
      describe "Strict" $ do
        it "Product on Sum Semigroup" $ property $
          \x s y t ->
            Strict.RSemidirect (Sum (x :: Int)) (Product (s :: Int))
            <> Strict.RSemidirect (Sum y) (Product t)
            `shouldBe`
            Strict.RSemidirect (Sum (x + s*y)) (Product (s*t))
        it "Product on Sum Monoid" $
          mempty `shouldBe`
            Strict.RSemidirect (mempty :: Sum Int) (mempty :: Product Int)

  describe "Action" $ do
    describe "ActSelf" $ do
      it "Int acts on unit" $ property $
        \x -> (x :: Int) <>$ () `shouldBe` ()
      it "Unit acts on char" $ property $
        \x -> () <>$ (x :: Char) `shouldBe` x
