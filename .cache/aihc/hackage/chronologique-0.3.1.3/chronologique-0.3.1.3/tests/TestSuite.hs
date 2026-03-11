{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec

import CheckTimeStamp
import CheckVectorOperations

main :: IO ()
main = do
    hspec suite
    putStrLn "."

suite :: Spec
suite = do
    checkTimeStamp
    checkVectorUnboxed
