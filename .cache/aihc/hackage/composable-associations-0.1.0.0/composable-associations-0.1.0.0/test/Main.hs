{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Lens
import Data.Proxy

import Data.ComposableAssociation

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "Assoc Getter" $ setKeyTest ^. _value @?= "object"
  , testCase "Assoc Modify" $ (setKeyTest & _value %~ (++ "1")) @?= Association Proxy "object1"
  , testCase "Assoc Set" $ (setKeyTest & _value .~ True) @?= Association Proxy True
  , testCase ":<> Getter1" $ withObjTest ^. _base @?= "a"
  , testCase ":<> Modify1" $ (withObjTest & _base %~ (++ "1")) @?= "a1" :<> "b"
  , testCase ":<> Set1" $ (withObjTest & _base .~ True) @?= True :<> "b"
  , testCase ":<> Getter2" $ withObjTest ^. _assoc @?= "b"
  , testCase ":<> Modify2" $ (withObjTest & _assoc %~ (++ "1")) @?= "a" :<> "b1"
  , testCase ":<> Set2" $ (withObjTest & _assoc .~ True) @?= "a" :<> True
  , testCase "Assoc :<> Assoc Base-Getter" $ withSetKeyTest ^. _base . _value @?=  "a"
  , testCase "Assoc :<> Assoc Key-Getter" $ withSetKeyTest ^. _assoc . _value @?= "b"
  , testCase "Assoc :<> Assoc Base-Modify" $
      (withSetKeyTest & _base . _value %~ (++ "1")) @?= asValue "a1" :<> asValue "b"
  , testCase "Assoc :<> Assoc Key-Modify" $
      (withSetKeyTest & _assoc . _value %~ (++ "1")) @?= asValue "a" :<> asValue "b1" ]

setKeyTest :: Association "test" String
setKeyTest = asValue "object"

withObjTest :: String :<> String
withObjTest = "a" :<> "b"

withSetKeyTest :: Association "test_key_1" String :<> Association "test_key_2" String
withSetKeyTest = asValue "a" :<> asValue "b"
