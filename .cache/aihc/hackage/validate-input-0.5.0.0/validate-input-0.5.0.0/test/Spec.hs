{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Validator

import Test.Hspec
import Test.QuickCheck

errMsg :: String
errMsg = "oops"

main :: IO ()
main =
    hspec $
    do describe "conformsPred" $
        do it "should result in error when value does not conform pred" $
            property $ \(i :: Int) ->
                i <= 5 ==> runValidator (conformsPred (>5) errMsg) i == Left errMsg
           it "should result in error when value does conform pred" $
            property $ \(i :: Int) ->
                i > 5 ==> runValidator (conformsPred (>5) errMsg) i == Right i
       describe "minLength" $
        do it "should result in error when string is to small" $
            property $ \(str :: String) (i :: Int64) ->
                fromIntegral (length str) < i ==>
                runValidator (minLength i errMsg) str == Left errMsg
           it "should allow strings with correct size to pass" $
            property $ \(str :: String) (i :: Int64) ->
                            fromIntegral (length str) >= i ==>
                            runValidator (minLength i errMsg) str == Right str
       describe "maxLength" $
        do it "should result in error when string is to large" $
            property $ \(str :: String) (i :: Int64) ->
                fromIntegral (length str) > i ==>
                runValidator (maxLength i errMsg) str == Left errMsg
           it "should allow strings with correct size to pass" $
            property $ \(str :: String) (i :: Int64) ->
                fromIntegral (length str) <= i ==>
                runValidator (maxLength i errMsg) str == Right str