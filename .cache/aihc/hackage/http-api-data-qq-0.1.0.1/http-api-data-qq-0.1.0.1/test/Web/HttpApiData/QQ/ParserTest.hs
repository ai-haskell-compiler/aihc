{-# LANGUAGE TupleSections #-}

module Web.HttpApiData.QQ.ParserTest (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Web.HttpApiData.QQ.Parser

tests :: TestTree
tests =
  testGroup
    "Web.HttpApiData.QQ.Parser"
    [ testCase "Sanity check" $ do
        let expected = [RawString "/foo/", InterpolatedName "bar", RawString "/baz"]
        parseUrlPieces "/foo/#{bar}/baz" @?= Right expected
    , testCase "Allows special characters in non-interpolation settings" $
        parseUrlPieces "/foo/ba{r}/baz#asdf" @?= Right [RawString "/foo/ba{r}/baz#asdf"]
    , testProperty "Parses any raw string" $
        forAll rawString1 $ \s ->
          parseUrlPieces s === Right [RawString s]
    , testProperty "Parses any combination of raw/interpolated strings" $
        forAll (alternatingListOf ((,True) <$> rawString1) ((,False) <$> rawString1)) $ \parts ->
          let input =
                flip concatMap parts $ \(s, isRaw) ->
                  if isRaw then s else "#{" ++ s ++ "}"
              expected =
                flip map parts $ \(s, isRaw) ->
                  if isRaw then RawString s else InterpolatedName s
           in parseUrlPieces input === Right expected
    ]

-- |
-- For two generators A and B, generates a list where each element
-- alternates between the two generators.
--
-- e.g.
--   * []
--   * [A]
--   * [B]
--   * [A, B]
--   * [B, A]
--   * [A, B, A]
--   * [B, A, B]
alternatingListOf :: Gen a -> Gen a -> Gen [a]
alternatingListOf genA genB = do
  startOnB <- arbitrary
  NonNegative n <- arbitrary
  sequence $ take n $ cycle $ if startOnB then [genB, genA] else [genA, genB]

-- | String that won't contain any interpolation characters.
rawString :: Gen String
rawString = listOf (arbitrary `suchThat` (`notElem` "#{}"))

rawString1 :: Gen String
rawString1 = rawString `suchThat` (/= "")
