{-# LANGUAGE OverloadedStrings #-}
module Data.Text.Format.NumbersSpec (spec) where

import Data.Text.Format.Numbers

import Data.Monoid
import Test.Hspec
import qualified Data.Text as T

formatTest :: (Double -> T.Text) -> Double -> T.Text -> SpecWith ()
formatTest f i o =
    it ("formats " <> show i <> " correctly as " <> T.unpack o) $
    f i `shouldBe` o

formatTestI :: (Int -> T.Text) -> Int -> T.Text -> SpecWith ()
formatTestI f i o =
    it ("formats " <> show i <> " correctly as " <> T.unpack o) $
    f i `shouldBe` o

spec :: Spec
spec =
    do describe "pretty" $
           do let p dec t s = prettyF (PrettyCfg dec (Just t) s)
              formatTest (p 2 ',' '.') 81601710123.338023 "81,601,710,123.34"
              formatTest (p 3 ' ' '.') 81601710123.338023 "81 601 710 123.338"
              formatTest (p 2 ' ' '.') (-81601710123.338023) "-81 601 710 123.34"
              formatTest (p 0 ' ' ',') 12 "12"
              formatTest (p 0 ' ' ',') 12.1 "12"
              formatTest (p 1 ' ' ',') 12.1 "12,1"
              formatTest (p 2 ' ' ',') 12.1 "12,10"
              formatTest (prettyF $ PrettyCfg 2 Nothing ',') 1200.1 "1200,10"
              formatTest (prettyF $ PrettyCfg 2 Nothing '.') 1200.01 "1200.01"
              formatTest (prettyF $ PrettyCfg 3 Nothing '.') 1200.01 "1200.010"
              formatTest (prettyF $ PrettyCfg 3 Nothing '.') 1200.001 "1200.001"
       describe "prettyInt" $
           do formatTestI (prettyI $ Just ',') 81601710123 "81,601,710,123"
              formatTestI (prettyI $ Just ' ') 81601710123 "81 601 710 123"
              formatTestI (prettyI $ Just ' ') (-81601710123) "-81 601 710 123"
              formatTestI (prettyI Nothing) (-81601710123) "-81601710123"
              formatTestI (prettyI Nothing) 81601710123 "81601710123"
