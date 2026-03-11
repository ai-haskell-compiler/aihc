{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ENIG.Premise where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit.Base

import qualified Data.Text as T
import Data.Text.Normalize

import Data.Text.ENIG.Data

tests = $(testGroupGenerator)

ga   = "가"
gag  = "각"
ha   = "하"
heuh = "헣"
g    = "ㄱ"
_g   = T.singleton . T.index (nfd gag) $ 0
__g  = T.singleton . T.index (nfd gag) $ 2
_a   = "ㅏ"
__a  = T.singleton . T.index (nfd gag) $ 1

sampleHangul = [g, _g, __g, _a, __a, ga, gag, ha, heuh] :: [T.Text]

nfc  = normalize NFC
nfkc = normalize NFKC
nfd  = normalize NFD
nfkd = normalize NFKD

nfcd   = nfc  . nfd
nfkcd  = nfkc . nfd
nfckd  = nfc  . nfkd
nfkckd = nfkc . nfkd

-- # Result
-- * NFC/NFD does not handle "ㅏ" or "ㄱ" as a component.
-- * When handling them different, NFC/NFD should be used.
-- * However, because of their pronouces, do not need to handle then differently.

c_normalizationNFCD
  = assertBool
      "nfc .  nfd /= nfkc .  nfd"
      (not $ all  (\x ->  nfcd x ==  nfkcd x) sampleHangul)
c_normalizationNFCKD
  = assertBool
      "nfc . nfkd == nfkc . nfkd"
      (all  (\x -> nfckd x == nfkckd x) sampleHangul)

test_NormalizationComparison =
  [ testCase ("nfc .  nfd == nfkc . nfkd with " ++ (T.unpack $ T.intercalate ", " sampleHangul)) c_normalizationNFCD
  , testCase ("nfc . nfkd == nfkc . nfkd with " ++ (T.unpack $ T.intercalate ", " sampleHangul)) c_normalizationNFCKD
  ]
