{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Data.TypeNums.IntsSpec where

import Data.TypeNums
import Test.Hspec
import Test.QuickCheck(property)

import Data.Proxy
import Data.Ratio
import GHC.Exts(Proxy#, proxy#)

spec :: Spec
spec = do
  describe "intVal" $ do
    it "correctly gets the value of a natural number" $
      intVal (Proxy @3) `shouldBe` 3
    it "correctly gets the value of zero" $
      intVal (Proxy @0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      intVal (Proxy @('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      intVal (Proxy @('Neg 10)) `shouldBe` (-10)
    it "correctly gets the value of a positive integer" $
      intVal (Proxy @('Pos 53)) `shouldBe` 53
  describe "intVal'" $ do
    it "correctly gets the value of a natural number" $
      intVal' (proxy# :: Proxy# 3) `shouldBe` 3
    it "correctly gets the value of zero" $
      intVal' (proxy# :: Proxy# 0) `shouldBe` 0
    it "correctly gets the value of Neg 0" $
      intVal' (proxy# :: Proxy# ('Neg 0)) `shouldBe` 0
    it "correctly gets the value of a negative integer" $
      intVal' (proxy# :: Proxy# ('Neg 10)) `shouldBe` (-10)
    it "correctly gets the value of a positive integer" $
      intVal' (proxy# :: Proxy# ('Pos 52)) `shouldBe` 52
  describe "someIntVal" $
    it "fetches the correct value using intVal" $ property $
      \x -> case someIntVal x of
        SomeInt y -> intVal y `shouldBe` x
