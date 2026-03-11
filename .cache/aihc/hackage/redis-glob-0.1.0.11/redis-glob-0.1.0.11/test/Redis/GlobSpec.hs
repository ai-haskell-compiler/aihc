{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Module      : Redis.GlobSpec
Copyright   : (c) 2022 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module Redis.GlobSpec (spec, genWithOkPatterns) where

import qualified ASCII.Char as A
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8)
import Redis.Glob
import Redis.Glob.Internal
import Test.Hspec
import Test.QuickCheck


spec :: Spec
spec = describe "Glob" $ do
  context "parsePart" $
    it "should roundtrip with 'fromPart'" prop_trip

  context "parseParts" $
    it "should roundtrip with 'fromParts'" prop_multi_trip

  context "matches" $ do
    context "generated targets" $ do
      it "should match patterns" prop_match


prop_multi_trip :: Property
prop_multi_trip =
  withMaxSuccess 15000 $
    forAll (listOf genTwoParts) $ \ps -> do
      let allParts = concat ps
      parseParts (fromParts allParts) == Just allParts


prop_trip :: Property
prop_trip =
  withMaxSuccess 15000 $
    forAll genParts $
      \p -> parsePart (fromPart p) == Just p


prop_match :: Property
prop_match = forAll genWithOkPatterns checkOkPatterns


-- Avoid generating:
-- An InRange that starts with hat; this parses correctly except when it's the first item
-- A single that just contains a dash; surrounded by other singles this mimicks InRange
-- An InRange with that starts with a dash, when preceded by a single this parses as the wrong InRange
-- A single 'hat', when it is first char, it changes a Squared to a Negated
genInSquare :: Gen InSquare
genInSquare =
  frequency
    [ (3, Single <$> avoidElem [A.Caret, A.HyphenMinus] genInnerChar)
    , (1, InRange <$> avoidElem [A.Caret, A.HyphenMinus, A.Backslash] genInnerChar <*> genInnerChar)
    ]


genParts :: Gen Part
genParts =
  frequency
    [ (3, pure Any)
    , (1, pure Many)
    , (1, Squared <$> genInSquare <*> smallListOf genInSquare)
    , (1, Negated <$> genInSquare <*> smallListOf genInSquare)
    , (10, Unescaped <$> genPrintable <*> smallListOf genPrintable)
    ]


-- when testing the parsing of list of parts, to ensure that no two succesive
-- parts are unescaped, the list of parts is generated in twos, where the first
-- part cannot be unescaped
genTwoParts :: Gen [Part]
genTwoParts = do
  part <- genParts
  special <- genSpecials
  pure [special, part]


checkOkPatterns :: (BL.ByteString, [BL.ByteString]) -> Bool
checkOkPatterns (target, patterns) = all (target `matches`) patterns


genWithOkPatterns :: Gen (BL.ByteString, [BL.ByteString])
genWithOkPatterns = do
  target <- genTarget
  isThisOk <- listOf1 $ replaceUsing' replacers target
  pure (BL.pack target, map BL.pack isThisOk)


-- TODO: Ideally, to test matches:
-- generate target, a [Word8] of length n that will be packed into a ByteString
-- determine m < n
-- generate m [Part]
-- changedIndices <- shuffle the list [0..n], pick the first m
-- zip changeIndices with type of replacement
-- yield the pattern bytes obtained by applying each replacement in turn
--
-- At the moment, only one Word8 is changed at a time, i.e, each pattern has
-- just one match

replaceUsing' :: [Word8 -> Gen Part] -> [Word8] -> Gen [Word8]
replaceUsing' fs xs = do
  n <- chooseInt (0, length xs - 1)
  f <- elements fs
  ys <- f $ xs !! n
  pure $ take n xs <> BL.unpack (fromPart ys) <> drop (n + 1) xs


goodChoice :: Word8 -> Gen Part
goodChoice x = Squared (Single x) <$> smallListOf genInSquare


goodRange :: Word8 -> Gen Part
goodRange x = Squared <$> goodInRange x <*> smallListOf genInSquare


smallListOf :: Gen a -> Gen [a]
smallListOf aGen = do
  small <- choose (1, 5)
  resize small (listOf aGen)


goodInRange :: Word8 -> Gen InSquare
goodInRange x = do
  start <- genSimpleChar `suchThat` (<= x)
  end <- genSimpleChar `suchThat` (>= x)
  pure $ InRange start end


replacers :: [Word8 -> Gen Part]
replacers = [goodChoice, goodRange, const $ pure Many, const $ pure Any]


genTarget :: Gen [Word8]
genTarget = BL.unpack . fromPart <$> genBase


genBase :: Gen Part
genBase = Unescaped <$> genSimpleChar <*> listOf genSimpleChar


genSpecials :: Gen Part
genSpecials =
  frequency
    [ (3, pure Any)
    , (1, pure Many)
    , (1, Squared <$> genInSquare <*> smallListOf genInSquare)
    , (1, Negated <$> genInSquare <*> smallListOf genInSquare)
    ]


genPrintable :: Gen Word8
genPrintable = chooseEnum (32, 127)


genInnerChar :: Gen Word8
genInnerChar = genPrintable `suchThat` (/= fromChar' A.RightSquareBracket)


genSimpleChar :: Gen Word8
genSimpleChar = avoidElem notSimpleChars genInnerChar


notSimpleChars :: [A.Char]
notSimpleChars =
  [ A.Backslash
  , A.LeftSquareBracket
  , A.QuestionMark
  , A.Caret
  , A.HyphenMinus
  , A.Asterisk
  ]


avoidElem :: [A.Char] -> Gen Word8 -> Gen Word8
avoidElem xs = (`suchThat` (\x -> x `notElem` map fromChar' xs))


fromChar' :: A.Char -> Word8
fromChar' = fromIntegral . A.toInt
