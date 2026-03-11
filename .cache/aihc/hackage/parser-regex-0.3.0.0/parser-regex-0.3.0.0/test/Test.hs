{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}  -- Arbitrary instances

import Control.Applicative (Alternative(..))
import qualified Control.Applicative as Ap
import Control.Monad (guard, void)
import Data.Char (isDigit, isHexDigit)
import qualified Data.List as L
import Data.Maybe (isJust, isNothing)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.String (fromString)
import qualified Numeric as Num
import Numeric.Natural (Natural)
import Data.Text (Text)
import qualified Data.Text as T

import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.HUnit ((@?=), testCase, assertBool, assertFailure)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes.Base (Laws(..))
import qualified Test.QuickCheck.Classes.Base as Laws
import Test.QuickCheck.Poly (A, OrdA)

import qualified Data.CharSet as CS
import qualified Regex.Base as R
import qualified Regex.List as RL
import qualified Regex.Text as RT

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 5000) $ testGroup "Tests"
  [ testGroup "Regex.Text"
    [ textReTests
    , combinatorTests
    , compileTests
    , textOpTests
    ]
  , testGroup "Regex.List"
    [ listReTests
    , listCombinatorTests
    , stringOpTests
    ]
  , testGroup "Regex.Base"
    [ earlyFailureTests
    ]
  , manyTests
  , charSetTests
  ]

----------------
-- Various REs
---------------

textReTests :: TestTree
textReTests = testGroup "Text RE"
  [ testGroup "char"
    [ testPM "a, a, ok" (RT.char 'a') "a" (Just 'a')
    , testPM "a, b, fail" (RT.char 'a') "b" Nothing
    , testPM "a, <e>, fail" (RT.char 'a') "" Nothing
    , testProperty "random" $ \c1 c2 ->
        RT.reParse (RT.char c1) (T.singleton c2) ===
        if c1 == c2 then Just c1 else Nothing
    ]
  , testGroup "charIgnoreCase" $
    let f c1 c2 = testPM ([c1] <> ", " <> [c2] <> ", ok")
                         (RT.charIgnoreCase c1) (T.singleton c2) (Just c2)
    in ["aA", "Ǳǲǳ", "θϴϑΘ"] >>= \cs -> Ap.liftA2 f cs cs
  , testGroup "anyChar"
    [ testProperty "random" $ \c ->
        RT.reParse RT.anyChar (T.singleton c) === Just c
    ]
  , testGroup "oneOf"
    [ testProperty "random" $ \cs c ->
        RT.reParse (RT.oneOf cs) (T.singleton c) ===
        if CS.member c cs then Just c else Nothing
    ]
  , testGroup "text"
    [ testPM "foo, foo, ok" (RT.text "foo") "foo" (Just "foo")
    , testPM "foo, bar, fail" (RT.text "foo") "bar" Nothing
    , testPM "foo, <e>, fail" (RT.text "foo") "" Nothing
    , testProperty "random" $ \t ->
        RT.reParse (RT.text t) t === Just t
    ]
  , testGroup "textIgnoreCase"
    [ testPM "foo, foo, ok" (RT.textIgnoreCase "foo") "foo" (Just "foo")
    , testPM "foo, FOO, ok" (RT.textIgnoreCase "foo") "FOO" (Just "FOO")
    , testPM "foo, fOO, ok" (RT.textIgnoreCase "foo") "fOO" (Just "fOO")
    , testPM "foo, bar, fail" (RT.textIgnoreCase "foo") "bar" Nothing
    , testPM "foo, <e>, fail" (RT.textIgnoreCase "foo") "" Nothing
    , testPM "ǳnuts, ǲNuts, ok" (RT.textIgnoreCase "ǳnuts") "ǲNuts" (Just "ǲNuts")
    , testPM ":)" (RT.textIgnoreCase ":)") ":)" (Just ":)")
    , testProperty "random theta" $
      let gt = T.pack <$> listOf (elements "θϴϑΘ") in
      forAll ((,) <$> gt <*> gt) $ \(t1,t2) ->
        RT.reParse (RT.textIgnoreCase t1 <* RT.manyText) t2 ===
        if T.length t1 <= T.length t2 then Just (T.take (T.length t1) t2) else Nothing
    ]
  , testGroup "manyText"
    [ testProperty "random" $ \t ->
        RT.reParse RT.manyText t === Just t
    ]
  , testGroup "someText"
    [ testProperty "random" $ \t ->
        RT.reParse RT.someText t === if T.null t then Nothing else Just t
    ]
  , testGroup "manyTextMin"
    [ testProperty "random" $ \t ->
        RT.reParse RT.manyTextMin t === Just t
    ]
  , testGroup "someTextMin"
    [ testProperty "random" $ \t ->
        RT.reParse RT.someTextMin t === if T.null t then Nothing else Just t
    ]
  , testGroup "many some Text bias" $
    let f (name,re1,re2,g) = testProperty name $ \t ->
          RT.reParse (Ap.liftA2 (,) re1 re2) t === g t
    in map f
      [ ("manyText manyText", RT.manyText, RT.manyText, \t -> Just (t,""))
      , ("manyText someText", RT.manyText, RT.someText, \t ->
          fmap (fmap T.singleton) (T.unsnoc t))
      , ("manyText manyTextMin", RT.manyText, RT.manyTextMin, \t -> Just (t,""))
      , ("manyText someTextMin", RT.manyText, RT.someTextMin,  \t ->
          fmap (fmap T.singleton) (T.unsnoc t))
      , ("someText manyText", RT.someText, RT.manyText, \t ->
          if T.null t then Nothing else Just (t,""))
      , ("someText someText", RT.someText, RT.someText, \t -> do
          (t',c) <- T.unsnoc t
          _ <- T.uncons t'
          pure (t', T.singleton c))
      , ("someText manyTextMin", RT.someText, RT.manyTextMin, \t ->
          if T.null t then Nothing else Just (t,""))
      , ("someText someTextMin", RT.someText, RT.someTextMin, \t -> do
          (t',c) <- T.unsnoc t
          _ <- T.uncons t'
          pure (t', T.singleton c))
      , ("manyTextMin manyText", RT.manyTextMin, RT.manyText, \t -> Just ("",t))
      , ("manyTextMin someText", RT.manyTextMin, RT.someText, \t ->
          if T.null t then Nothing else Just ("",t))
      , ("manyTextMin manyTextMin", RT.manyTextMin, RT.manyTextMin, \t -> Just ("",t))
      , ("manyTextMin someTextMin", RT.manyTextMin, RT.someTextMin,  \t ->
          if T.null t then Nothing else Just ("",t))
      , ("someTextMin manyText", RT.someTextMin, RT.manyText, \t ->
          fmap (\(c,t') -> (T.singleton c,t')) (T.uncons t))
      , ("someTextMin someText", RT.someTextMin, RT.someText, \t -> do
          (c,t') <- T.uncons t
          _ <- T.uncons t'
          pure (T.singleton c,t'))
      , ("someTextMin manyTextMin", RT.someTextMin, RT.manyTextMin, \t ->
          fmap (\(c,t') -> (T.singleton c,t')) (T.uncons t))
      , ("someTextMin someTextMin", RT.someTextMin, RT.someTextMin, \t -> do
          (c,t') <- T.uncons t
          _ <- T.uncons t'
          pure (T.singleton c,t'))
      ]
  , testGroup "manyTextOf"
    [ testProperty "random" $
      forAll zeroOneText $ \t ->
        RT.reParse (RT.manyTextOf "0") t ===
        if T.all (=='0') t then Just t else Nothing
    , testPM "bias" (RT.manyTextOf "0" <* RT.manyText) "0000" (Just "0000")
    ]
  , testGroup "someTextOf"
    [ testProperty "random" $
      forAll zeroOneText $ \t ->
        RT.reParse (RT.someTextOf "0") t ===
        if not (T.null t) && T.all (=='0') t then Just t else Nothing
    , testPM "bias" (RT.someTextOf "0" <* RT.manyText) "0000" (Just "0000")
    ]
  , testGroup "manyTextOfMin"
    [ testProperty "random" $
      forAll zeroOneText $ \t ->
        RT.reParse (RT.manyTextOfMin "0") t ===
        if T.all (=='0') t then Just t else Nothing
    , testPM "bias" (RT.manyTextOfMin "0" <* RT.manyText) "0000" (Just "")
    ]
  , testGroup "someTextOfMin"
    [ testProperty "random" $
      forAll zeroOneText $ \t ->
        RT.reParse (RT.someTextOfMin "0") t ===
        if not (T.null t) && T.all (=='0') t then Just t else Nothing
    , testPM "bias" (RT.someTextOfMin "0" <* RT.manyText) "0000" (Just "0")
    ]
  , textNumericTests
  ]

listReTests :: TestTree
listReTests = testGroup "List RE"
  [ testGroup "single"
    [ testLPM "a, a, ok" (RL.single 'a') "a" (Just 'a')
    , testLPM "a, b, fail" (RL.single 'a') "b" Nothing
    , testLPM "a, <e>, fail" (RL.single 'a') "" Nothing
    , testProperty "random" $ \c1 c2 ->
        RL.reParse @Char (RL.single c1) [c2] ===
        if c1 == c2 then Just c1 else Nothing
    ]
  , testGroup "charIgnoreCase" $
    let f c1 c2 = testLPM ([c1] <> ", " <> [c2] <> ", ok")
                          (RL.charIgnoreCase c1) [c2] (Just c2)
    in ["aA", "Ǳǲǳ", "θϴϑΘ"] >>= \cs -> Ap.liftA2 f cs cs
  , testGroup "anyChar"
    [ testProperty "random" $ \c ->
        RL.reParse @Char RL.anySingle [c] === Just c
    ]
  , testGroup "oneOf"
    [ testProperty "random" $ \cs c ->
        RL.reParse (RL.oneOfChar cs) [c] ===
        if CS.member c cs then Just c else Nothing
    ]
  , testGroup "list"
    [ testLPM "foo, foo, ok" (RL.list "foo") "foo" (Just "foo")
    , testLPM "foo, bar, fail" (RL.list "foo") "bar" Nothing
    , testLPM "foo, <e>, fail" (RL.list "foo") "" Nothing
    , testProperty "random" $ \t ->
        RL.reParse @Char (RL.list t) t === Just t
    ]
  , testGroup "stringIgnoreCase"
    [ testLPM "foo, foo, ok" (RL.stringIgnoreCase "foo") "foo" (Just "foo")
    , testLPM "foo, FOO, ok" (RL.stringIgnoreCase "foo") "FOO" (Just "FOO")
    , testLPM "foo, fOO, ok" (RL.stringIgnoreCase "foo") "fOO" (Just "fOO")
    , testLPM "foo, bar, fail" (RL.stringIgnoreCase "foo") "bar" Nothing
    , testLPM "foo, <e>, fail" (RL.stringIgnoreCase "foo") "" Nothing
    , testLPM "ǳnuts, ǲNuts, ok" (RL.stringIgnoreCase "ǳnuts") "ǲNuts" (Just "ǲNuts")
    , testLPM ":)" (RL.stringIgnoreCase ":)") ":)" (Just ":)")
    , testProperty "random theta" $
      let gt = listOf (elements "θϴϑΘ") in
      forAll ((,) <$> gt <*> gt) $ \(t1,t2) ->
        RL.reParse (RL.stringIgnoreCase t1 <* RL.manyList) t2 ===
        if length t1 <= length t2 then Just (take (length t1) t2) else Nothing
    ]
  , testGroup "manyList"
    [ testProperty "random" $ \t ->
        RL.reParse @Char RL.manyList t === Just t
    ]
  , testGroup "someList"
    [ testProperty "random" $ \t ->
        RL.reParse @Char RL.someList t === if null t then Nothing else Just t
    ]
  , testGroup "manyListMin"
    [ testProperty "random" $ \t ->
        RL.reParse @Char RL.manyListMin t === Just t
    ]
  , testGroup "someListMin"
    [ testProperty "random" $ \t ->
        RL.reParse @Char RL.someListMin t === if null t then Nothing else Just t
    ]
  , testGroup "many some Text bias" $
    let f (name,re1,re2,g) = testProperty name $ \t ->
          RL.reParse (Ap.liftA2 (,) re1 re2) t === g t
    in map f
      [ ("manyList manyList", RL.manyList, RL.manyList, \t -> Just (t,""))
      , ("manyList someList", RL.manyList, RL.someList, \t ->
          fmap (fmap (:[])) (unsnoc t))
      , ("manyList manyListMin", RL.manyList, RL.manyListMin, \t -> Just (t,""))
      , ("manyList someListMin", RL.manyList, RL.someListMin,  \t ->
          fmap (fmap (:[])) (unsnoc t))
      , ("someList manyList", RL.someList, RL.manyList, \t ->
          if null t then Nothing else Just (t,""))
      , ("someList someList", RL.someList, RL.someList, \t -> do
          (t',c) <- unsnoc t
          _ <- L.uncons t'
          pure (t', [c]))
      , ("someList manyListMin", RL.someList, RL.manyListMin, \t ->
          if null t then Nothing else Just (t,""))
      , ("someList someListMin", RL.someList, RL.someListMin, \t -> do
          (t',c) <- unsnoc t
          _ <- L.uncons t'
          pure (t', [c]))
      , ("manyListMin manyList", RL.manyListMin, RL.manyList, \t -> Just ("",t))
      , ("manyListMin someList", RL.manyListMin, RL.someList, \t ->
          if null t then Nothing else Just ("",t))
      , ("manyListMin manyListMin", RL.manyListMin, RL.manyListMin, \t -> Just ("",t))
      , ("manyListMin someListMin", RL.manyListMin, RL.someListMin,  \t ->
          if null t then Nothing else Just ("",t))
      , ("someListMin manyList", RL.someListMin, RL.manyList, \t ->
          fmap (\(c,t') -> ([c],t')) (L.uncons t))
      , ("someListMin someList", RL.someListMin, RL.someList, \t -> do
          (c,t') <- L.uncons t
          _ <- L.uncons t'
          pure ([c],t'))
      , ("someListMin manyListMin", RL.someListMin, RL.manyListMin, \t ->
          fmap (\(c,t') -> ([c],t')) (L.uncons t))
      , ("someListMin someListMin", RL.someListMin, RL.someListMin, \t -> do
          (c,t') <- L.uncons t
          _ <- L.uncons t'
          pure ([c],t'))
      ]
  , testGroup "manyListOf"
    [ testProperty "random" $
      forAll zeroOneString $ \t ->
        RL.reParse (RL.manyStringOf "0") t ===
        if all (=='0') t then Just t else Nothing
    , testLPM "bias" (RL.manyStringOf "0" <* RL.manyList) "0000" (Just "0000")
    ]
  , testGroup "someListOf"
    [ testProperty "random" $
      forAll zeroOneString $ \t ->
        RL.reParse (RL.someStringOf "0") t ===
        if not (null t) && all (=='0') t then Just t else Nothing
    , testLPM "bias" (RL.someStringOf "0" <* RL.manyList) "0000" (Just "0000")
    ]
  , testGroup "manyListOfMin"
    [ testProperty "random" $
      forAll zeroOneString $ \t ->
        RL.reParse (RL.manyStringOfMin "0") t ===
        if all (=='0') t then Just t else Nothing
    , testLPM "bias" (RL.manyStringOfMin "0" <* RL.manyList) "0000" (Just "")
    ]
  , testGroup "someListOfMin"
    [ testProperty "random" $
      forAll zeroOneString $ \t ->
        RL.reParse (RL.someStringOfMin "0") t ===
        if not (null t) && all (=='0') t then Just t else Nothing
    , testLPM "bias" (RL.someStringOfMin "0" <* RL.manyList) "0000" (Just "0")
    ]
  , stringNumericTests
  ]

------------------
-- Numeric tests
------------------

textNumericTests :: TestTree
textNumericTests = testGroup "Text numeric"
  [ testGroup "naturalDec"
    [ testPM "<empty>, fail" RT.naturalDec "" Nothing
    , testPM "1a, fail" RT.naturalDec "1a" Nothing
    , testPM "1a2, fail" RT.naturalDec "1a2" Nothing
    , testPM "0, ok" RT.naturalDec "0" (Just 0)
    , testPM "1, ok" RT.naturalDec "1" (Just 1)
    , testPM "-1, fail" RT.naturalDec "-1" Nothing
    , testPM "+1, fail" RT.naturalDec "+1" Nothing
    , testPM "01, fail" RT.naturalDec "01" Nothing
    , testPM "123456789123456789123456789, ok" RT.naturalDec "123456789123456789123456789" (Just 123456789123456789123456789)
    , testPM "18446744073709551615, ok" RT.naturalDec "18446744073709551615" (Just 18446744073709551615)
    , testPM "18446744073709551616, ok" RT.naturalDec "18446744073709551616" (Just 18446744073709551616)
    , testProperty "random dec" $
      forAll decText $ \t ->
        RT.reParse RT.naturalDec t === Just (read (T.unpack t))
    , testProperty "random" $
      forAll abDecText $ \t ->
        let ex = parseDecNoLz (T.unpack t)
        in classify (isJust ex) "ok" $
          RT.reParse RT.naturalDec t === ex
    ]
  , testGroup "integerDec"
    [ testPM "pure (), 1, ok" (RT.integerDec (pure ())) "1" (Just 1)
    , testPM "pure (), +1, ok" (RT.integerDec (pure ())) "+1" (Just 1)
    , testPM "pure (), -1, ok" (RT.integerDec (pure ())) "-1" (Just (-1))
    , testPM "pure (), 001, fail" (RT.integerDec (pure ())) "001" Nothing
    , testPM "pure (), +001, fail" (RT.integerDec (pure ())) "+001" Nothing
    , testPM "pure (), -001, fail" (RT.integerDec (pure ())) "-001" Nothing
    , testPM "lz, 1, ok" (RT.integerDec (many (RT.char '0'))) "1" (Just 1)
    , testPM "lz, +1, ok" (RT.integerDec (many (RT.char '0'))) "+1" (Just 1)
    , testPM "lz, -1, ok" (RT.integerDec (many (RT.char '0'))) "-1" (Just (-1))
    , testPM "lz, 001, ok" (RT.integerDec (many (RT.char '0'))) "001" (Just 1)
    , testPM "lz, +001, ok" (RT.integerDec (many (RT.char '0'))) "+001" (Just 1)
    , testPM "lz, -001, ok" (RT.integerDec (many (RT.char '0'))) "-001" (Just (-1))
    , testProperty "random" $
      forAll (Ap.liftA2 (<>) (elements ["-","+",""]) abDecText) $ \t ->
        let ex = parseInteger parseDecNoLz (T.unpack t)
        in classify (isJust ex) "ok" $
          RT.reParse (RT.integerDec (pure ())) t === ex
    ]
  , testGroup "naturalHex"
    [ testPM "<empty>, fail" RT.naturalHex "" Nothing
    , testPM "1g, fail" RT.naturalHex "1g" Nothing
    , testPM "1g2, fail" RT.naturalHex "1g2" Nothing
    , testPM "0, ok" RT.naturalHex "0" (Just 0)
    , testPM "1, ok" RT.naturalHex "1" (Just 1)
    , testPM "f, ok" RT.naturalHex "f" (Just 15)
    , testPM "F, ok" RT.naturalHex "F" (Just 15)
    , testPM "-1, fail" RT.naturalHex "-1" Nothing
    , testPM "+1, fail" RT.naturalHex "+1" Nothing
    , testPM "01, fail" RT.naturalHex "01" Nothing
    , testPM "123456789abcdef123456789abcdef, ok" RT.naturalHex "123456789abcdef123456789abcdef" (Just 0x123456789abcdef123456789abcdef)
    , testPM "ffffffffffffffff, ok" RT.naturalHex "ffffffffffffffff" (Just 0xffffffffffffffff)
    , testPM "10000000000000000, ok" RT.naturalHex "10000000000000000" (Just 0x10000000000000000)
    , testProperty "random hex" $
      forAll hexText $ \t ->
        RT.reParse RT.naturalHex t === Just (read ("0x" ++ T.unpack t))
    , testProperty "random" $
      forAll pqHexText $ \t ->
        let ex = parseHexNoLz (T.unpack t)
        in classify (isJust ex) "ok" $
          RT.reParse RT.naturalHex t === ex
    ]
  , testGroup "integerHex"
    [ testPM "pure (), 1, ok" (RT.integerHex (pure ())) "1" (Just 1)
    , testPM "pure (), +1, ok" (RT.integerHex (pure ())) "+1" (Just 1)
    , testPM "pure (), -1, ok" (RT.integerHex (pure ())) "-1" (Just (-1))
    , testPM "pure (), 001, fail" (RT.integerHex (pure ())) "001" Nothing
    , testPM "pure (), +001, fail" (RT.integerHex (pure ())) "+001" Nothing
    , testPM "pure (), -001, fail" (RT.integerHex (pure ())) "-001" Nothing
    , testPM "lz, 1, ok" (RT.integerHex (many (RT.char '0'))) "1" (Just 1)
    , testPM "lz, +1, ok" (RT.integerHex (many (RT.char '0'))) "+1" (Just 1)
    , testPM "lz, -1, ok" (RT.integerHex (many (RT.char '0'))) "-1" (Just (-1))
    , testPM "lz, 001, ok" (RT.integerHex (many (RT.char '0'))) "001" (Just 1)
    , testPM "lz, +001, ok" (RT.integerHex (many (RT.char '0'))) "+001" (Just 1)
    , testPM "lz, -001, ok" (RT.integerHex (many (RT.char '0'))) "-001" (Just (-1))
    , testProperty "random" $
      forAll (Ap.liftA2 (<>) (elements ["-","+",""]) pqHexText) $ \t ->
        let ex = parseInteger parseHexNoLz (T.unpack t)
        in classify (isJust ex) "ok" $
          RT.reParse (RT.integerHex (pure ())) t === ex
    ]
  , testGroup "wordRangeDec"
    [ testPM "(0,0) 0 ok" (RT.wordRangeDec (0,0)) "0" (Just 0)
    , testPM "(0,0) 1 fail" (RT.wordRangeDec (0,0)) "1" Nothing
    , testPM "(0,0) -1 fail" (RT.wordRangeDec (0,0)) "-1" Nothing
    , testPM "(1,0) 0 fail" (RT.wordRangeDec (1,0)) "1" Nothing
    , testPM "(1,0) 1 fail" (RT.wordRangeDec (1,0)) "0" Nothing
    , testPM "(0,19) 00 fail" (RT.wordRangeDec (0,19)) "00" Nothing
    , testPM "(100,999) 0 fail" (RT.wordRangeDec (100,999)) "0" Nothing
    , testPM "(100,999) 1 fail" (RT.wordRangeDec (100,999)) "1" Nothing
    , testPM "(100,999) 123 ok" (RT.wordRangeDec (100,999)) "123" (Just 123)
    , testPM "(100,999) 1234 fail" (RT.wordRangeDec (100,999)) "1234" Nothing
    , testPM "(0,1) 01 fail" (RT.wordRangeDec (0,1)) "01" Nothing
    , testPM "(0,maxBound) maxBound ok"
             (RT.wordRangeDec (0,maxBound))
             (T.pack (show (maxBound :: Word)))
             (Just maxBound)
    , testPM "(0,maxBound) (maxBound+1) fail"
             (RT.wordRangeDec (0,maxBound))
             (T.pack (show (fromIntegral (maxBound :: Word) + 1 :: Integer)))
             Nothing
    , testPM "(maxBound,maxBound) (maxBound-1) fail"
             (RT.wordRangeDec (maxBound,maxBound))
             (T.pack (show (maxBound - 1 :: Word)))
             Nothing
    , testGroup "bias"
      [ let re = RT.wordRangeDec (1,999) in
        testPM "(1,999) 2222 (222,2)" (Ap.liftA2 (,) re re) "2222" (Just (222,2))
      , let re = RT.wordRangeDec (1,1000) in
        testPM "(1,1000) 1111, (111,1)" (Ap.liftA2 (,) re re) "1111" (Just (111,1))
      ]
    , testProperty "any word" $ \(Large n) ->
        RT.reParse (RT.wordRangeDec (minBound,maxBound)) (T.pack (show n)) ===
        Just n
    , testGroup "random dec" $
      let f low high n =
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RT.reParse (RT.wordRangeDec (low,high)) (T.pack (show n)) ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll abDecText $ \t ->
          let ex = do
                x <- parseDecNoLz (T.unpack t)
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RT.reParse (RT.wordRangeDec (low,high)) t === ex
    ]
  , testGroup "intRangeDec"
    [ testPM "pure (), (-1,1), 1, ok" (RT.intRangeDec (pure ()) (-1,1)) "1" (Just 1)
    , testPM "pure (), (-1,1), +1, ok" (RT.intRangeDec (pure ()) (-1,1)) "+1" (Just 1)
    , testPM "pure (), (-1,1), -1, ok" (RT.intRangeDec (pure ()) (-1,1)) "-1" (Just (-1))
    , testPM "pure (), (-1,1), 001, fail" (RT.intRangeDec (pure ()) (-1,1)) "001" Nothing
    , testPM "pure (), (-1,1), +001, fail" (RT.intRangeDec (pure ()) (-1,1)) "+001" Nothing
    , testPM "pure (), (-1,1), -001, fail" (RT.intRangeDec (pure ()) (-1,1)) "-001" Nothing
    , testPM "lz, (-1,1), 1, ok" (RT.intRangeDec (many (RT.char '0')) (-1,1)) "1" (Just 1)
    , testPM "lz, (-1,1), +1, ok" (RT.intRangeDec (many (RT.char '0')) (-1,1)) "+1" (Just 1)
    , testPM "lz, (-1,1), -1, ok" (RT.intRangeDec (many (RT.char '0')) (-1,1)) "-1" (Just (-1))
    , testPM "lz, (-1,1), 001, ok" (RT.intRangeDec (many (RT.char '0')) (-1,1)) "001" (Just 1)
    , testPM "lz, (-1,1), +001, ok" (RT.intRangeDec (many (RT.char '0')) (-1,1)) "+001" (Just 1)
    , testPM "lz, (-1,1), -001, ok" (RT.intRangeDec (many (RT.char '0')) (-1,1)) "-001" (Just (-1))
    , testPM "(minBound,maxBound) maxBound ok"
             (RT.intRangeDec (pure ()) (minBound,maxBound))
             (T.pack (show (maxBound :: Int)))
             (Just maxBound)
    , testPM "(minBound,maxBound) minBound ok"
             (RT.intRangeDec (pure ()) (minBound,maxBound))
             (T.pack (show (minBound :: Int)))
             (Just minBound)
    , testPM "(minBound,maxBound) (maxBound+1) fail"
             (RT.intRangeDec (pure ()) (minBound,maxBound))
             (T.pack (show (fromIntegral (maxBound :: Int) + 1 :: Integer)))
             Nothing
    , testPM "(minBound,maxBound) (minBound+1) fail"
             (RT.intRangeDec (pure ()) (minBound,maxBound))
             (T.pack (show (fromIntegral (minBound :: Int) - 1 :: Integer)))
             Nothing
    , testPM "(maxBound,maxBound) (maxBound-1) fail"
             (RT.intRangeDec (pure ()) (maxBound,maxBound))
             (T.pack (show (maxBound - 1 :: Int)))
             Nothing
    , testPM "(minBound,minBound) (minBound+1) fail"
             (RT.intRangeDec (pure ()) (minBound,minBound))
             (T.pack (show (minBound + 1 :: Int)))
             Nothing
    , testProperty "any int" $ \(Large n) ->
        RT.reParse (RT.intRangeDec (pure ()) (minBound,maxBound)) (T.pack (show n)) === Just n
    , testGroup "random dec" $
      let f low high n = forAll (showIntDecExtraSign n) $ \nstr ->
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RT.reParse (RT.intRangeDec (pure ()) (low,high)) (T.pack nstr) ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll (Ap.liftA2 (<>) (elements ["-","+",""]) abDecText) $ \t ->
          let ex = do
                x <- parseInteger parseDecNoLz (T.unpack t)
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RT.reParse (RT.intRangeDec (pure ()) (low,high)) t === ex
    ]
  , testGroup "wordRangeHex"
    [ testPM "(0,0) 0 ok" (RT.wordRangeHex (0,0)) "0" (Just 0)
    , testPM "(0,0) 1 fail" (RT.wordRangeHex (0,0)) "1" Nothing
    , testPM "(0,0) -1 fail" (RT.wordRangeHex (0,0)) "-1" Nothing
    , testPM "(1,0) 0 fail" (RT.wordRangeHex (1,0)) "1" Nothing
    , testPM "(1,0) 1 fail" (RT.wordRangeHex (1,0)) "0" Nothing
    , testPM "(0,1f) 00 fail" (RT.wordRangeHex (0,0x1f)) "00" Nothing
    , testPM "(100,fff) 0 fail" (RT.wordRangeHex (0x100,0xfff)) "0" Nothing
    , testPM "(100,fff) 1 fail" (RT.wordRangeHex (0x100,0xfff)) "1" Nothing
    , testPM "(100,fff) 123 ok" (RT.wordRangeHex (0x100,0xfff)) "123" (Just 0x123)
    , testPM "(100,fff) 1234 fail" (RT.wordRangeHex (0x100,0xfff)) "1234" Nothing
    , testPM "(0,1) 01 fail" (RT.wordRangeHex (0,1)) "01" Nothing
    , testPM "(0,maxBound) maxBound ok"
             (RT.wordRangeHex (0,maxBound))
             (T.pack (showHex (maxBound :: Word)))
             (Just maxBound)
    , testPM "(0,maxBound) (maxBound+1) fail"
             (RT.wordRangeHex (0,maxBound))
             (T.pack (showHex (fromIntegral (maxBound :: Word) + 1 :: Integer)))
             Nothing
    , testPM "(maxBound,maxBound) (maxBound-1) fail"
             (RT.wordRangeHex (maxBound,maxBound))
             (T.pack (showHex (maxBound - 1 :: Word)))
             Nothing
    , testGroup "bias"
      [ let re = RT.wordRangeHex (0x1,0x999) in
        testPM "(1,999) 2222 (222,2)" (Ap.liftA2 (,) re re) "2222" (Just (0x222,0x2))
      , let re = RT.wordRangeHex (0x1,0x1000) in
        testPM "(1,1000) 1111, (111,1)" (Ap.liftA2 (,) re re) "1111" (Just (0x111,0x1))
      ]
    , testProperty "any word" $ \(Large n) ->
        RT.reParse (RT.wordRangeHex (minBound,maxBound)) (T.pack (showHex n)) ===
        Just n
    , testGroup "random hex" $
      let f low high n =
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RT.reParse (RT.wordRangeHex (low,high)) (T.pack (showHex n)) ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll pqHexText $ \t ->
          let ex = do
                x <- parseHexNoLz (T.unpack t)
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RT.reParse (RT.wordRangeHex (low,high)) t === ex
    ]
  , testGroup "intRangeHex"
    [ testPM "pure (), (-1,1), 1, ok" (RT.intRangeHex (pure ()) (-1,1)) "1" (Just 1)
    , testPM "pure (), (-1,1), +1, ok" (RT.intRangeHex (pure ()) (-1,1)) "+1" (Just 1)
    , testPM "pure (), (-1,1), -1, ok" (RT.intRangeHex (pure ()) (-1,1)) "-1" (Just (-1))
    , testPM "pure (), (-1,1), 001, fail" (RT.intRangeHex (pure ()) (-1,1)) "001" Nothing
    , testPM "pure (), (-1,1), +001, fail" (RT.intRangeHex (pure ()) (-1,1)) "+001" Nothing
    , testPM "pure (), (-1,1), -001, fail" (RT.intRangeHex (pure ()) (-1,1)) "-001" Nothing
    , testPM "lz, (-1,1), 1, ok" (RT.intRangeHex (many (RT.char '0')) (-1,1)) "1" (Just 1)
    , testPM "lz, (-1,1), +1, ok" (RT.intRangeHex (many (RT.char '0')) (-1,1)) "+1" (Just 1)
    , testPM "lz, (-1,1), -1, ok" (RT.intRangeHex (many (RT.char '0')) (-1,1)) "-1" (Just (-1))
    , testPM "lz, (-1,1), 001, ok" (RT.intRangeHex (many (RT.char '0')) (-1,1)) "001" (Just 1)
    , testPM "lz, (-1,1), +001, ok" (RT.intRangeHex (many (RT.char '0')) (-1,1)) "+001" (Just 1)
    , testPM "lz, (-1,1), -001, ok" (RT.intRangeHex (many (RT.char '0')) (-1,1)) "-001" (Just (-1))
    , testPM "(minBound,maxBound) maxBound ok"
             (RT.intRangeHex (pure ()) (minBound,maxBound))
             (T.pack (showHex (maxBound :: Int)))
             (Just maxBound)
    , testPM "(minBound,maxBound) minBound ok"
             (RT.intRangeHex (pure ()) (minBound,maxBound))
             (T.pack (showHex (minBound :: Int)))
             (Just minBound)
    , testPM "(minBound,maxBound) (maxBound+1) fail"
             (RT.intRangeHex (pure ()) (minBound,maxBound))
             (T.pack (showHex (fromIntegral (maxBound :: Int) + 1 :: Integer)))
             Nothing
    , testPM "(minBound,maxBound) (minBound+1) fail"
             (RT.intRangeHex (pure ()) (minBound,maxBound))
             (T.pack (showHex (fromIntegral (minBound :: Int) - 1 :: Integer)))
             Nothing
    , testPM "(maxBound,maxBound) (maxBound-1) fail"
             (RT.intRangeHex (pure ()) (maxBound,maxBound))
             (T.pack (showHex (maxBound - 1 :: Int)))
             Nothing
    , testPM "(minBound,minBound) (minBound+1) fail"
             (RT.intRangeHex (pure ()) (minBound,minBound))
             (T.pack (showHex (minBound + 1 :: Int)))
             Nothing
    , testProperty "any int" $ \(Large n) ->
        RT.reParse (RT.intRangeHex (pure ()) (minBound,maxBound)) (T.pack (showHex n)) === Just n
    , testGroup "random hex" $
      let f low high n = forAll (showIntHexExtraSign n) $ \nstr ->
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RT.reParse (RT.intRangeHex (pure ()) (low,high)) (T.pack nstr) ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll (Ap.liftA2 (<>) (elements ["-","+",""]) pqHexText) $ \t ->
          let ex = do
                x <- parseInteger parseHexNoLz (T.unpack t)
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RT.reParse (RT.intRangeHex (pure ()) (low,high)) t === ex
    ]
  , testGroup "wordDecN"
    [ let n = maxBound :: Word
          t = T.pack (show n)
      in
      testPM "maxBound ok" (RT.wordDecN (T.length t)) t (Just n)
    , let n = fromIntegral (maxBound :: Word) + 1 :: Integer
          t = T.pack (show n)
      in
      testPM "(maxBound+1) fail" (RT.wordDecN (T.length t)) t Nothing
    , testPM "<29 0, 1 1> ok" (RT.wordDecN 30) (T.replicate 29 "0" <> "1") (Just 1)
    , testProperty "random dec" $ \n ->
        forAll (let d = elements decDigits
                in frequency [(3, vectorOf n d), (1, listOf d)]) $ \s ->
          let ok = n > 0
                && length s == n
                && (read s :: Integer) < fromIntegral (maxBound :: Word)
          in classify ok "ok" $
            RT.reParse (RT.wordDecN n) (T.pack s) ===
            if ok then Just (read s) else Nothing
    , testProperty "random" $ \n ->
        forAll abDecText $ \t ->
          let ex = do
                guard $ n > 0 && T.length t == n
                x <- if T.all (=='0') t
                     then Just 0
                     else parseDecNoLz $ dropWhile (=='0') $ T.unpack t
                guard $ x <= fromIntegral (maxBound :: Word)
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RT.reParse (RT.wordDecN n) t === ex
    ]
  , testGroup "wordHexN"
    [ let n = maxBound :: Word
          t = T.pack (showHex n)
      in
      testPM "maxBound ok" (RT.wordHexN (T.length t)) t (Just n)
    , let n = fromIntegral (maxBound :: Word) + 1 :: Integer
          t = T.pack (showHex n)
      in
      testPM "(maxBound+1) fail" (RT.wordHexN (T.length t)) t Nothing
    , testPM "<29 0, 1 1> ok" (RT.wordHexN 30) (T.replicate 29 "0" <> "1") (Just 1)
    , testProperty "random hex" $ \n ->
        forAll (let d = elements hexDigits
                in frequency [(3, vectorOf n d), (1, listOf d)]) $ \s ->
          let ok = n > 0
                && length s == n
                && (read ("0x" ++ s) :: Integer) < fromIntegral (maxBound :: Word)
          in
          classify ok "ok" $
            RT.reParse (RT.wordHexN n) (T.pack s) ===
            if ok then Just (read ("0x" ++ s)) else Nothing
    , testProperty "random" $ \n ->
        forAll pqHexText $ \t ->
          let ex = do
                guard $ n > 0 && T.length t == n
                x <- if T.all (=='0') t
                     then Just 0
                     else parseHexNoLz $ dropWhile (=='0') $ T.unpack t
                guard $ x <= fromIntegral (maxBound :: Word)
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RT.reParse (RT.wordHexN n) t === ex
    ]
  ]

stringNumericTests :: TestTree
stringNumericTests = testGroup "Text numeric"
  [ testGroup "naturalDec"
    [ testLPM "<empty>, fail" RL.naturalDec "" Nothing
    , testLPM "1a, fail" RL.naturalDec "1a" Nothing
    , testLPM "1a2, fail" RL.naturalDec "1a2" Nothing
    , testLPM "0, ok" RL.naturalDec "0" (Just 0)
    , testLPM "1, ok" RL.naturalDec "1" (Just 1)
    , testLPM "-1, fail" RL.naturalDec "-1" Nothing
    , testLPM "+1, fail" RL.naturalDec "+1" Nothing
    , testLPM "01, fail" RL.naturalDec "01" Nothing
    , testLPM "123456789123456789123456789, ok" RL.naturalDec "123456789123456789123456789" (Just 123456789123456789123456789)
    , testLPM "18446744073709551615, ok" RL.naturalDec "18446744073709551615" (Just 18446744073709551615)
    , testLPM "18446744073709551616, ok" RL.naturalDec "18446744073709551616" (Just 18446744073709551616)
    , testProperty "random dec" $
      forAll decString $ \t ->
        RL.reParse RL.naturalDec t === Just (read t)
    , testProperty "random" $
      forAll abDecString $ \t ->
        let ex = parseDecNoLz t
        in classify (isJust ex) "ok" $
          RL.reParse RL.naturalDec t === ex
    ]
  , testGroup "integerDec"
    [ testLPM "pure (), 1, ok" (RL.integerDec (pure ())) "1" (Just 1)
    , testLPM "pure (), +1, ok" (RL.integerDec (pure ())) "+1" (Just 1)
    , testLPM "pure (), -1, ok" (RL.integerDec (pure ())) "-1" (Just (-1))
    , testLPM "pure (), 001, fail" (RL.integerDec (pure ())) "001" Nothing
    , testLPM "pure (), +001, fail" (RL.integerDec (pure ())) "+001" Nothing
    , testLPM "pure (), -001, fail" (RL.integerDec (pure ())) "-001" Nothing
    , testLPM "lz, 1, ok" (RL.integerDec (many (RL.single '0'))) "1" (Just 1)
    , testLPM "lz, +1, ok" (RL.integerDec (many (RL.single '0'))) "+1" (Just 1)
    , testLPM "lz, -1, ok" (RL.integerDec (many (RL.single '0'))) "-1" (Just (-1))
    , testLPM "lz, 001, ok" (RL.integerDec (many (RL.single '0'))) "001" (Just 1)
    , testLPM "lz, +001, ok" (RL.integerDec (many (RL.single '0'))) "+001" (Just 1)
    , testLPM "lz, -001, ok" (RL.integerDec (many (RL.single '0'))) "-001" (Just (-1))
    , testProperty "random" $
      forAll (Ap.liftA2 (<>) (elements ["-","+",""]) abDecString) $ \t ->
        let ex = parseInteger parseDecNoLz t
        in classify (isJust ex) "ok" $
          RL.reParse (RL.integerDec (pure ())) t === ex
    ]
  , testGroup "naturalHex"
    [ testLPM "<empty>, fail" RL.naturalHex "" Nothing
    , testLPM "1g, fail" RL.naturalHex "1g" Nothing
    , testLPM "1g2, fail" RL.naturalHex "1g2" Nothing
    , testLPM "0, ok" RL.naturalHex "0" (Just 0)
    , testLPM "1, ok" RL.naturalHex "1" (Just 1)
    , testLPM "f, ok" RL.naturalHex "f" (Just 15)
    , testLPM "F, ok" RL.naturalHex "F" (Just 15)
    , testLPM "-1, fail" RL.naturalHex "-1" Nothing
    , testLPM "+1, fail" RL.naturalHex "+1" Nothing
    , testLPM "01, fail" RL.naturalHex "01" Nothing
    , testLPM "123456789abcdef123456789abcdef, ok" RL.naturalHex "123456789abcdef123456789abcdef" (Just 0x123456789abcdef123456789abcdef)
    , testLPM "ffffffffffffffff, ok" RL.naturalHex "ffffffffffffffff" (Just 0xffffffffffffffff)
    , testLPM "10000000000000000, ok" RL.naturalHex "10000000000000000" (Just 0x10000000000000000)
    , testProperty "random hex" $
      forAll hexString $ \t ->
        RL.reParse RL.naturalHex t === Just (read ("0x" ++ t))
    , testProperty "random" $
      forAll pqHexString $ \t ->
        let ex = parseHexNoLz t
        in classify (isJust ex) "ok" $
          RL.reParse RL.naturalHex t === ex
    ]
  , testGroup "integerHex"
    [ testLPM "pure (), 1, ok" (RL.integerHex (pure ())) "1" (Just 1)
    , testLPM "pure (), +1, ok" (RL.integerHex (pure ())) "+1" (Just 1)
    , testLPM "pure (), -1, ok" (RL.integerHex (pure ())) "-1" (Just (-1))
    , testLPM "pure (), 001, fail" (RL.integerHex (pure ())) "001" Nothing
    , testLPM "pure (), +001, fail" (RL.integerHex (pure ())) "+001" Nothing
    , testLPM "pure (), -001, fail" (RL.integerHex (pure ())) "-001" Nothing
    , testLPM "lz, 1, ok" (RL.integerHex (many (RL.single '0'))) "1" (Just 1)
    , testLPM "lz, +1, ok" (RL.integerHex (many (RL.single '0'))) "+1" (Just 1)
    , testLPM "lz, -1, ok" (RL.integerHex (many (RL.single '0'))) "-1" (Just (-1))
    , testLPM "lz, 001, ok" (RL.integerHex (many (RL.single '0'))) "001" (Just 1)
    , testLPM "lz, +001, ok" (RL.integerHex (many (RL.single '0'))) "+001" (Just 1)
    , testLPM "lz, -001, ok" (RL.integerHex (many (RL.single '0'))) "-001" (Just (-1))
    , testProperty "random" $
      forAll (Ap.liftA2 (<>) (elements ["-","+",""]) pqHexString) $ \t ->
        let ex = parseInteger parseHexNoLz t
        in classify (isJust ex) "ok" $
          RL.reParse (RL.integerHex (pure ())) t === ex
    ]
  , testGroup "wordRangeDec"
    [ testLPM "(0,0) 0 ok" (RL.wordRangeDec (0,0)) "0" (Just 0)
    , testLPM "(0,0) 1 fail" (RL.wordRangeDec (0,0)) "1" Nothing
    , testLPM "(0,0) -1 fail" (RL.wordRangeDec (0,0)) "-1" Nothing
    , testLPM "(1,0) 0 fail" (RL.wordRangeDec (1,0)) "1" Nothing
    , testLPM "(1,0) 1 fail" (RL.wordRangeDec (1,0)) "0" Nothing
    , testLPM "(0,19) 00 fail" (RL.wordRangeDec (0,19)) "00" Nothing
    , testLPM "(100,999) 0 fail" (RL.wordRangeDec (100,999)) "0" Nothing
    , testLPM "(100,999) 1 fail" (RL.wordRangeDec (100,999)) "1" Nothing
    , testLPM "(100,999) 123 ok" (RL.wordRangeDec (100,999)) "123" (Just 123)
    , testLPM "(100,999) 1234 fail" (RL.wordRangeDec (100,999)) "1234" Nothing
    , testLPM "(0,1) 01 fail" (RL.wordRangeDec (0,1)) "01" Nothing
    , testLPM "(0,maxBound) maxBound ok"
             (RL.wordRangeDec (0,maxBound))
             (show (maxBound :: Word))
             (Just maxBound)
    , testLPM "(0,maxBound) (maxBound+1) fail"
             (RL.wordRangeDec (0,maxBound))
             (show (fromIntegral (maxBound :: Word) + 1 :: Integer))
             Nothing
    , testLPM "(maxBound,maxBound) (maxBound-1) fail"
             (RL.wordRangeDec (maxBound,maxBound))
             (show (maxBound - 1 :: Word))
             Nothing
    , testGroup "bias"
      [ let re = RL.wordRangeDec (1,999) in
        testLPM "(1,999) 2222 (222,2)" (Ap.liftA2 (,) re re) "2222" (Just (222,2))
      , let re = RL.wordRangeDec (1,1000) in
        testLPM "(1,1000) 1111, (111,1)" (Ap.liftA2 (,) re re) "1111" (Just (111,1))
      ]
    , testProperty "any word" $ \(Large n) ->
        RL.reParse (RL.wordRangeDec (minBound,maxBound)) (show n) ===
        Just n
    , testGroup "random dec" $
      let f low high n =
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RL.reParse (RL.wordRangeDec (low,high)) (show n) ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll abDecString $ \t ->
          let ex = do
                x <- parseDecNoLz t
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RL.reParse (RL.wordRangeDec (low,high)) t === ex
    ]
  , testGroup "intRangeDec"
    [ testLPM "pure (), (-1,1), 1, ok" (RL.intRangeDec (pure ()) (-1,1)) "1" (Just 1)
    , testLPM "pure (), (-1,1), +1, ok" (RL.intRangeDec (pure ()) (-1,1)) "+1" (Just 1)
    , testLPM "pure (), (-1,1), -1, ok" (RL.intRangeDec (pure ()) (-1,1)) "-1" (Just (-1))
    , testLPM "pure (), (-1,1), 001, fail" (RL.intRangeDec (pure ()) (-1,1)) "001" Nothing
    , testLPM "pure (), (-1,1), +001, fail" (RL.intRangeDec (pure ()) (-1,1)) "+001" Nothing
    , testLPM "pure (), (-1,1), -001, fail" (RL.intRangeDec (pure ()) (-1,1)) "-001" Nothing
    , testLPM "lz, (-1,1), 1, ok" (RL.intRangeDec (many (RL.single '0')) (-1,1)) "1" (Just 1)
    , testLPM "lz, (-1,1), +1, ok" (RL.intRangeDec (many (RL.single '0')) (-1,1)) "+1" (Just 1)
    , testLPM "lz, (-1,1), -1, ok" (RL.intRangeDec (many (RL.single '0')) (-1,1)) "-1" (Just (-1))
    , testLPM "lz, (-1,1), 001, ok" (RL.intRangeDec (many (RL.single '0')) (-1,1)) "001" (Just 1)
    , testLPM "lz, (-1,1), +001, ok" (RL.intRangeDec (many (RL.single '0')) (-1,1)) "+001" (Just 1)
    , testLPM "lz, (-1,1), -001, ok" (RL.intRangeDec (many (RL.single '0')) (-1,1)) "-001" (Just (-1))
    , testLPM "(minBound,maxBound) maxBound ok"
             (RL.intRangeDec (pure ()) (minBound,maxBound))
             (show (maxBound :: Int))
             (Just maxBound)
    , testLPM "(minBound,maxBound) minBound ok"
             (RL.intRangeDec (pure ()) (minBound,maxBound))
             (show (minBound :: Int))
             (Just minBound)
    , testLPM "(minBound,maxBound) (maxBound+1) fail"
             (RL.intRangeDec (pure ()) (minBound,maxBound))
             (show (fromIntegral (maxBound :: Int) + 1 :: Integer))
             Nothing
    , testLPM "(minBound,maxBound) (minBound+1) fail"
             (RL.intRangeDec (pure ()) (minBound,maxBound))
             (show (fromIntegral (minBound :: Int) - 1 :: Integer))
             Nothing
    , testLPM "(maxBound,maxBound) (maxBound-1) fail"
             (RL.intRangeDec (pure ()) (maxBound,maxBound))
             (show (maxBound - 1 :: Int))
             Nothing
    , testLPM "(minBound,minBound) (minBound+1) fail"
             (RL.intRangeDec (pure ()) (minBound,minBound))
             (show (minBound + 1 :: Int))
             Nothing
    , testProperty "any int" $ \(Large n) ->
        RL.reParse (RL.intRangeDec (pure ()) (minBound,maxBound)) (show n) === Just n
    , testGroup "random dec" $
      let f low high n = forAll (showIntDecExtraSign n) $ \nstr ->
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RL.reParse (RL.intRangeDec (pure ()) (low,high)) nstr ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll (Ap.liftA2 (<>) (elements ["-","+",""]) abDecString) $ \t ->
          let ex = do
                x <- parseInteger parseDecNoLz t
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RL.reParse (RL.intRangeDec (pure ()) (low,high)) t === ex
    ]
  , testGroup "wordRangeHex"
    [ testLPM "(0,0) 0 ok" (RL.wordRangeHex (0,0)) "0" (Just 0)
    , testLPM "(0,0) 1 fail" (RL.wordRangeHex (0,0)) "1" Nothing
    , testLPM "(0,0) -1 fail" (RL.wordRangeHex (0,0)) "-1" Nothing
    , testLPM "(1,0) 0 fail" (RL.wordRangeHex (1,0)) "1" Nothing
    , testLPM "(1,0) 1 fail" (RL.wordRangeHex (1,0)) "0" Nothing
    , testLPM "(0,1f) 00 fail" (RL.wordRangeHex (0,0x1f)) "00" Nothing
    , testLPM "(100,fff) 0 fail" (RL.wordRangeHex (0x100,0xfff)) "0" Nothing
    , testLPM "(100,fff) 1 fail" (RL.wordRangeHex (0x100,0xfff)) "1" Nothing
    , testLPM "(100,fff) 123 ok" (RL.wordRangeHex (0x100,0xfff)) "123" (Just 0x123)
    , testLPM "(100,fff) 1234 fail" (RL.wordRangeHex (0x100,0xfff)) "1234" Nothing
    , testLPM "(0,1) 01 fail" (RL.wordRangeHex (0,1)) "01" Nothing
    , testLPM "(0,maxBound) maxBound ok"
             (RL.wordRangeHex (0,maxBound))
             (showHex (maxBound :: Word))
             (Just maxBound)
    , testLPM "(0,maxBound) (maxBound+1) fail"
             (RL.wordRangeHex (0,maxBound))
             (showHex (fromIntegral (maxBound :: Word) + 1 :: Integer))
             Nothing
    , testLPM "(maxBound,maxBound) (maxBound-1) fail"
             (RL.wordRangeHex (maxBound,maxBound))
             (showHex (maxBound - 1 :: Word))
             Nothing
    , testGroup "bias"
      [ let re = RL.wordRangeHex (0x1,0x999) in
        testLPM "(1,999) 2222 (222,2)" (Ap.liftA2 (,) re re) "2222" (Just (0x222,0x2))
      , let re = RL.wordRangeHex (0x1,0x1000) in
        testLPM "(1,1000) 1111, (111,1)" (Ap.liftA2 (,) re re) "1111" (Just (0x111,0x1))
      ]
    , testProperty "any word" $ \(Large n) ->
        RL.reParse (RL.wordRangeHex (minBound,maxBound)) (showHex n) ===
        Just n
    , testGroup "random hex" $
      let f low high n =
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RL.reParse (RL.wordRangeHex (low,high)) (showHex n) ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll pqHexString $ \t ->
          let ex = do
                x <- parseHexNoLz t
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RL.reParse (RL.wordRangeHex (low,high)) t === ex
    ]
  , testGroup "intRangeHex"
    [ testLPM "pure (), (-1,1), 1, ok" (RL.intRangeHex (pure ()) (-1,1)) "1" (Just 1)
    , testLPM "pure (), (-1,1), +1, ok" (RL.intRangeHex (pure ()) (-1,1)) "+1" (Just 1)
    , testLPM "pure (), (-1,1), -1, ok" (RL.intRangeHex (pure ()) (-1,1)) "-1" (Just (-1))
    , testLPM "pure (), (-1,1), 001, fail" (RL.intRangeHex (pure ()) (-1,1)) "001" Nothing
    , testLPM "pure (), (-1,1), +001, fail" (RL.intRangeHex (pure ()) (-1,1)) "+001" Nothing
    , testLPM "pure (), (-1,1), -001, fail" (RL.intRangeHex (pure ()) (-1,1)) "-001" Nothing
    , testLPM "lz, (-1,1), 1, ok" (RL.intRangeHex (many (RL.single '0')) (-1,1)) "1" (Just 1)
    , testLPM "lz, (-1,1), +1, ok" (RL.intRangeHex (many (RL.single '0')) (-1,1)) "+1" (Just 1)
    , testLPM "lz, (-1,1), -1, ok" (RL.intRangeHex (many (RL.single '0')) (-1,1)) "-1" (Just (-1))
    , testLPM "lz, (-1,1), 001, ok" (RL.intRangeHex (many (RL.single '0')) (-1,1)) "001" (Just 1)
    , testLPM "lz, (-1,1), +001, ok" (RL.intRangeHex (many (RL.single '0')) (-1,1)) "+001" (Just 1)
    , testLPM "lz, (-1,1), -001, ok" (RL.intRangeHex (many (RL.single '0')) (-1,1)) "-001" (Just (-1))
    , testLPM "(minBound,maxBound) maxBound ok"
             (RL.intRangeHex (pure ()) (minBound,maxBound))
             (showHex (maxBound :: Int))
             (Just maxBound)
    , testLPM "(minBound,maxBound) minBound ok"
             (RL.intRangeHex (pure ()) (minBound,maxBound))
             (showHex (minBound :: Int))
             (Just minBound)
    , testLPM "(minBound,maxBound) (maxBound+1) fail"
             (RL.intRangeHex (pure ()) (minBound,maxBound))
             (showHex (fromIntegral (maxBound :: Int) + 1 :: Integer))
             Nothing
    , testLPM "(minBound,maxBound) (minBound+1) fail"
             (RL.intRangeHex (pure ()) (minBound,maxBound))
             (showHex (fromIntegral (minBound :: Int) - 1 :: Integer))
             Nothing
    , testLPM "(maxBound,maxBound) (maxBound-1) fail"
             (RL.intRangeHex (pure ()) (maxBound,maxBound))
             (showHex (maxBound - 1 :: Int))
             Nothing
    , testLPM "(minBound,minBound) (minBound+1) fail"
             (RL.intRangeHex (pure ()) (minBound,minBound))
             (showHex (minBound + 1 :: Int))
             Nothing
    , testProperty "any int" $ \(Large n) ->
        RL.reParse (RL.intRangeHex (pure ()) (minBound,maxBound)) (showHex n) === Just n
    , testGroup "random hex" $
      let f low high n = forAll (showIntHexExtraSign n) $ \nstr ->
            let ex = inRange n (low,high) in
            classify ex "inRange" $
              RL.reParse (RL.intRangeHex (pure ()) (low,high)) nstr ===
              if ex then Just n else Nothing
      in
      [ testProperty "small" f
      , testProperty "large" $ \(Large low) (Large high) (Large n) -> f low high n
      ]
    , testProperty "random" $ \low high ->
        forAll (Ap.liftA2 (<>) (elements ["-","+",""]) pqHexString) $ \t ->
          let ex = do
                x <- parseInteger parseHexNoLz t
                guard $ fromIntegral low <= x && x <= fromIntegral high
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RL.reParse (RL.intRangeHex (pure ()) (low,high)) t === ex
    ]
  , testGroup "wordDecN"
    [ let n = maxBound :: Word
          t = show n
      in
      testLPM "maxBound ok" (RL.wordDecN (length t)) t (Just n)
    , let n = fromIntegral (maxBound :: Word) + 1 :: Integer
          t = show n
      in
      testLPM "(maxBound+1) fail" (RL.wordDecN (length t)) t Nothing
    , testLPM "<29 0, 1 1> ok" (RL.wordDecN 30) (replicate 29 '0' <> "1") (Just 1)
    , testProperty "random dec" $ \n ->
        forAll (let d = elements decDigits
                in frequency [(3, vectorOf n d), (1, listOf d)]) $ \s ->
          let ok = n > 0
                && length s == n
                && (read s :: Integer) < fromIntegral (maxBound :: Word)
          in classify ok "ok" $
            RL.reParse (RL.wordDecN n) s ===
            if ok then Just (read s) else Nothing
    , testProperty "random" $ \n ->
        forAll abDecString $ \t ->
          let ex = do
                guard $ n > 0 && length t == n
                x <- if all (=='0') t
                     then Just 0
                     else parseDecNoLz $ dropWhile (=='0') t
                guard $ x <= fromIntegral (maxBound :: Word)
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RL.reParse (RL.wordDecN n) t === ex
    ]
  , testGroup "wordHexN"
    [ let n = maxBound :: Word
          t = showHex n
      in
      testLPM "maxBound ok" (RL.wordHexN (length t)) t (Just n)
    , let n = fromIntegral (maxBound :: Word) + 1 :: Integer
          t = showHex n
      in
      testLPM "(maxBound+1) fail" (RL.wordHexN (length t)) t Nothing
    , testLPM "<29 0, 1 1> ok" (RL.wordHexN 30) (replicate 29 '0' <> "1") (Just 1)
    , testProperty "random hex" $ \n ->
        forAll (let d = elements hexDigits
                in frequency [(3, vectorOf n d), (1, listOf d)]) $ \s ->
          let ok = n > 0
                && length s == n
                && (read ("0x" ++ s) :: Integer) < fromIntegral (maxBound :: Word)
          in
          classify ok "ok" $
            RL.reParse (RL.wordHexN n) s ===
            if ok then Just (read ("0x" ++ s)) else Nothing
    , testProperty "random" $ \n ->
        forAll pqHexString $ \t ->
          let ex = do
                guard $ n > 0 && length t == n
                x <- if all (=='0') t
                     then Just 0
                     else parseHexNoLz $ dropWhile (=='0') t
                guard $ x <= fromIntegral (maxBound :: Word)
                pure $ fromIntegral x
          in classify (isJust ex) "ok" $
            RL.reParse (RL.wordHexN n) t === ex
    ]
  ]

decString :: Gen String
decString = do
  s <- listOf (elements decDigits)
  let s' = dropWhile (=='0') s
  pure $ if null s' then "0" else s'

decText :: Gen Text
decText = T.pack <$> decString

hexString :: Gen String
hexString = do
  s <- listOf (elements hexDigits)
  let s' = dropWhile (=='0') s
  pure $ if null s' then "0" else s'

hexText :: Gen Text
hexText = T.pack <$> hexString

decDigits :: String
decDigits = "0123456789"

hexDigits :: String
hexDigits = "0123456789ABCDEFabcdef"

abDecString :: Gen String
abDecString = listOf (elements ("ab" ++ decDigits))

abDecText :: Gen Text
abDecText = T.pack <$> abDecString

pqHexString :: Gen String
pqHexString = listOf (elements ("pq" ++ hexDigits))

pqHexText :: Gen Text
pqHexText = T.pack <$> pqHexString

showHex :: Integral a => a -> String
showHex n = (if n < 0 then ('-':) else id)
            (Num.showHex (abs (fromIntegral n :: Integer)) "")

showIntDecExtraSign :: Int -> Gen String
showIntDecExtraSign n = (<> show n) <$> sgn n
  where
    sgn x = case compare 0 x of
      LT -> elements ["+", ""]
      EQ -> elements ["-", "+", ""]
      GT -> pure ""

showIntHexExtraSign :: Int -> Gen String
showIntHexExtraSign n = (<> showHex n) <$> sgn n
  where
    sgn x = case compare 0 x of
      LT -> elements ["+", ""]
      EQ -> elements ["-", "+", ""]
      GT -> pure ""

parseInteger :: (String -> Maybe Natural) -> String -> Maybe Integer
parseInteger p s = case s of
  '-':s' -> negate . fromIntegral <$> p s'
  '+':s' -> fromIntegral <$> p s'
  _      -> fromIntegral <$> p s

parseDecNoLz :: String -> Maybe Natural
parseDecNoLz s = case s of
  "" -> Nothing
  "0" -> Just 0
  ('0':_) -> Nothing
  _ | (s1,[]) <- span isDigit s -> Just (read s1)
    | otherwise -> Nothing

parseHexNoLz :: String -> Maybe Natural
parseHexNoLz s = case s of
  "" -> Nothing
  "0" -> Just 0
  ('0':_) -> Nothing
  _ | (s1,[]) <- span isHexDigit s -> Just (read ("0x" ++ s1))
    | otherwise -> Nothing

----------------
-- Combinators
----------------

combinatorTests :: TestTree
combinatorTests = testGroup "Combinators"
  [ testGroup "pure"
    [ testPM "pure (), <e>, ok" (pure ()) "" (Just ())
    , testPM "pure (), a, fail" (pure ()) "a" Nothing
    ]
  , testGroup "liftA2" $
    let re = Ap.liftA2 (,) (RT.char 'a') (RT.char 'b') in
    [ testPM "a b, <e>, fail" re "" Nothing
    , testPM "a b, a, fail" re "a" Nothing
    , testPM "a b, b, fail" re "b" Nothing
    , testPM "a b, ab, ok" re "ab" (Just ('a','b'))
    ]
  , testGroup "<|>" $
    let ab = RT.char 'a' <|> RT.char 'b' in
    [ testPM "a <|> b, <e>, fail" ab "" Nothing
    , testPM "a <|> b, a, ok" ab "a" (Just 'a')
    , testPM "a <|> b, b, ok" ab "b" (Just 'b')
    , testGroup "bias"
      [ let re = (1 :: Int) <$ RT.char 'a' <|> 2 <$ RT.char 'a' in
        testPM "1 <$ a <|> 2 <$ a, a, ok" re "a" (Just 1)
      ]
    ]
  , testGroup "Semigroup,Monoid" $
    let go abc =
          [ testPM "<e>, fail" abc "" Nothing
          , testPM "a, fail" abc "a" Nothing
          , testPM "bc, fail" abc "bc" Nothing
          , testPM "abc, ok" abc "abc" (Just "abc")
          ]
    in
    [ testGroup "<>" $ go (RT.text "a" <> RT.text "bc")
    , testGroup "<> mempty" $ go (RT.text "abc" <> mempty)
    , testGroup "mempty <>" $ go (mempty <> RT.text "abc")
    , testGroup "sconcat" $ go (sconcat (RT.text "a" :| [RT.text "b", RT.text "c"]))
    , testGroup "mconcat" $ go (mconcat [RT.text "a", RT.text "b", RT.text "c"])
    ]
  , testGroup "many" $
    let a = many (RT.char 'a')
        pr = many (pure ())
    in
    [ testPM "many a, aa, ok" a "aa" (Just "aa")
    , testPM "many a, a, ok" a "a" (Just "a")
    , testPM "many a, <e>, ok" a "" (Just "")
    , testPM "many (pure ()), <e>, ok" pr "" (Just [])
    , testPM "many (pure ()), aaa, fail" pr "aaa" Nothing
    ]
  , testGroup "some" $
    let a = some (RT.char 'a') in
    [ testPM "some a, aa, ok" a "aa" (Just "aa")
    , testPM "some a, a, ok" a "a" (Just "a")
    , testPM "some a, <e>, fail" a "" Nothing
    ]
  , testGroup "manyMin" $
    let a = RT.manyMin (RT.char 'a')
        pr = RT.manyMin (pure ())
    in
    [ testPM "manyMin a, aa, ok" a "aa" (Just "aa")
    , testPM "manyMin a, a, ok" a "a" (Just "a")
    , testPM "manyMin a, <e>, ok" a "" (Just "")
    , testPM "manyMin (pure ()), <e>, ok" pr "" (Just [])
    , testPM "manyMin (pure ()), aaa, fail" pr "aaa" Nothing
    ]
  , testGroup "someMin" $
    let a = RT.someMin (RT.char 'a') in
    [ testPM "someMin a, aa, ok" a "aa" (Just "aa")
    , testPM "someMin a, a, ok" a "a" (Just "a")
    , testPM "someMin a, <e>, fail" a "" Nothing
    ]
  , testGroup "manyr" $
    let a = RT.manyr (RT.char 'a')
        pr = RT.manyr (pure ())
    in
    [ testPM "manyr a, <e>, ok" a "" (Just (RT.Finite ""))
    , testPM "manyr a, aaa, ok" a "aaa" (Just (RT.Finite "aaa"))
    , testPM "manyr (pure ()), <e>, ok" pr "" (Just (RT.Repeat ()))
    , testPM "manyr (pure ()), aaa, fail" pr "aaa" Nothing
    ]
  , testGroup "atLeast"
    [ testProperty "random" $ \m (NonNegative l) ->
        let t = T.replicate l "a" in
        RT.reParse (RT.atLeast m (RT.char 'a')) t ===
        if m <= l then Just (T.unpack t) else Nothing
    , testPM "bias" (RT.atLeast 2 RT.anyChar <* RT.manyText) "aaaa" (Just "aaaa")
    ]
  , testGroup "atMost"
    [ testProperty "random" $ \n (NonNegative l) ->
        let t = T.replicate l "a" in
        RT.reParse (RT.atMost n (RT.char 'a')) t ===
        if n >= l then Just (T.unpack t) else Nothing
    , testPM "bias" (RT.atMost 2 RT.anyChar <* RT.manyText) "aaaa" (Just "aa")
    ]
  , testGroup "betweenCount"
    [ testProperty "random" $ \mn (NonNegative l) ->
        let t = T.replicate l "a" in
        RT.reParse (RT.betweenCount mn (RT.char 'a')) t ===
        if inRange l mn then Just (T.unpack t) else Nothing
    , testPM "bias" (RT.betweenCount (2,3) RT.anyChar <* RT.manyText) "aaaa" (Just "aaa")
    ]
  , testGroup "atLeastMin"
    [ testProperty "random" $ \m (NonNegative l) ->
        let t = T.replicate l "a" in
        RT.reParse (RT.atLeastMin m (RT.char 'a')) t ===
        if m <= l then Just (T.unpack t) else Nothing
    , testPM "bias" (RT.atLeastMin 2 RT.anyChar <* RT.manyText) "aaaa" (Just "aa")
    ]
  , testGroup "atMostMin"
    [ testProperty "random" $ \n (NonNegative l) ->
        let t = T.replicate l "a" in
        RT.reParse (RT.atMostMin n (RT.char 'a')) t ===
        if n >= l then Just (T.unpack t) else Nothing
    , testPM "bias" (RT.atMostMin 2 RT.anyChar <* RT.manyText) "aaaa" (Just "")
    ]
  , testGroup "betweenCountMin"
    [ testProperty "random" $ \mn (NonNegative l) ->
        let t = T.replicate l "a" in
        RT.reParse (RT.betweenCountMin mn (RT.char 'a')) t ===
        if inRange l mn then Just (T.unpack t) else Nothing
    , testPM "bias" (RT.betweenCountMin (2,3) RT.anyChar <* RT.manyText) "aaaa" (Just "aa")
    ]
  , testGroup "sepBy" $
    let re = RT.char 'A' `RT.sepBy` RT.char 'x' in
    [ testGroup "A `sepBy` x"
      [ testPM "AxAx, fail" re "AxAx" Nothing
      , testPM "AxA, ok" re "AxA" (Just "AA")
      , testPM "Ax, fail" re "Ax" Nothing
      , testPM "A, ok" re "A" (Just "A")
      , testPM "<e>, ok" re "" (Just "")
      , testPM "x, fail" re "x" Nothing
      ]
    ]
  , testGroup "sepBy1" $
    let re = RT.char 'A' `RT.sepBy1` RT.char 'x' in
    [ testGroup "A `sepBy1` x"
      [ testPM "AxAx, fail" re "AxAx" Nothing
      , testPM "AxA, ok" re "AxA" (Just "AA")
      , testPM "Ax, fail" re "Ax" Nothing
      , testPM "A, ok" re "A" (Just "A")
      , testPM "<e>, fail" re "" Nothing
      , testPM "x, fail" re "x" Nothing
      ]
    ]
  , testGroup "endBy" $
    let re = RT.char 'A' `RT.endBy` RT.char 'x' in
    [ testGroup "A `endBy` x"
      [ testPM "AxAx, ok" re "AxAx" (Just "AA")
      , testPM "AxA, fail" re "AxA" Nothing
      , testPM "Ax, ok" re "Ax" (Just "A")
      , testPM "A, fail" re "A" Nothing
      , testPM "<e>, ok" re "" (Just "")
      , testPM "x, fail" re "x" Nothing
      ]
    ]
  , testGroup "endBy1" $
    let re = RT.char 'A' `RT.endBy1` RT.char 'x' in
    [ testGroup "A `endBy1` x"
      [ testPM "AxAx, ok" re "AxAx" (Just "AA")
      , testPM "AxA, fail" re "AxA" Nothing
      , testPM "Ax, ok" re "Ax" (Just "A")
      , testPM "A, fail" re "A" Nothing
      , testPM "<e>, fail" re "" Nothing
      , testPM "x, fail" re "x" Nothing
      ]
    ]
  , testGroup "sepEndBy" $
    let re = RT.char 'A' `RT.sepEndBy` RT.char 'x' in
    [ testGroup "A `sepEndBy` x"
      [ testPM "AxAx, ok" re "AxAx" (Just "AA")
      , testPM "AxA, ok" re "AxA" (Just "AA")
      , testPM "Ax, ok" re "Ax" (Just "A")
      , testPM "A, ok" re "A" (Just "A")
      , testPM "<e>, ok" re "" (Just "")
      , testPM "x, fail" re "x" Nothing
      ]
    ]
  , testGroup "sepEndBy1" $
    let re = RT.char 'A' `RT.sepEndBy1` RT.char 'x' in
    [ testGroup "A `sepEndBy1` x"
      [ testPM "AxAx, ok" re "AxAx" (Just "AA")
      , testPM "AxA, ok" re "AxA" (Just "AA")
      , testPM "Ax, ok" re "Ax" (Just "A")
      , testPM "A, ok" re "A" (Just "A")
      , testPM "<e>, fail" re "" Nothing
      , testPM "x, fail" re "x" Nothing
      ]
    ]
  , testGroup "chainl1" $
    let re = RT.chainl1 (One <$> RT.anyChar)
                        (TwoA <$ RT.char 'F' <|> TwoB <$ RT.char 'T')
    in
    [ testPM "<e>, fail" re "" Nothing
    , testPM "aa, fail" re "aFFa" Nothing
    , testPM "aFFa, fail" re "aFFa" Nothing
    , testProperty "random ok" $ \(x,opxs) ->
        let t = T.pack $ x : (opxs >>= \(op,y) -> [if op then 'T' else 'F', y])
            e = foldl (\acc (op,y) -> (if op then TwoB else TwoA) acc (One y)) (One x) opxs
        in
        RT.reParse re t === Just e
    ]
  , testGroup "chainr1" $
    let re = RT.chainr1 (One <$> RT.anyChar)
                        (TwoA <$ RT.char 'F' <|> TwoB <$ RT.char 'T')
    in
    [ testPM "<e>, fail" re "" Nothing
    , testPM "aa, fail" re "aFFa" Nothing
    , testPM "aFFa, fail" re "aFFa" Nothing
    , testProperty "random ok" $ \(xops,x) ->
        let t = T.pack $ (xops >>= \(y,op) -> [y, if op then 'T' else 'F']) ++ [x]
            e = foldr (\(y,op) acc -> (if op then TwoB else TwoA) (One y) acc) (One x) xops
        in
        RT.reParse re t === Just e
    ]
  , testGroup "many many"
    [ testPM "many (many a)" (many (many (RT.char 'a'))) "" (Just [])
    , testPM "many (many a), aaa" (many (many (RT.char 'a'))) "aaa" (Just ["aaa"])
    , testPM "many (manyr a)" (many (RT.manyr (RT.char 'a'))) "" (Just [])
    , testPM "many (manyr a), aaa" (many (RT.manyr (RT.char 'a'))) "aaa" (Just [RT.Finite "aaa"])
    , testPM "many (manyMin a)" (many (RT.manyMin (RT.char 'a'))) "" (Just [])
    , testPM "many (manyMin a), aaa" (many (RT.manyMin (RT.char 'a'))) "aaa" (Just ["a","a","a"])
    , testPM "manyr (many a)" (RT.manyr (many (RT.char 'a'))) "" (Just (RT.Repeat []))
    , testPM "manyr (many a), aaa" (RT.manyr (many (RT.char 'a'))) "aaa" (Just (RT.Finite ["aaa"]))
    , testPM "manyr (manyr a)" (RT.manyr (RT.manyr (RT.char 'a'))) "" (Just (RT.Repeat (RT.Finite "")))
    , testPM "manyr (manyr a), aaa" (RT.manyr (RT.manyr (RT.char 'a'))) "aaa" (Just (RT.Finite [RT.Finite "aaa"]))
    , testPM "manyr (manyMin a)" (RT.manyr (RT.manyMin (RT.char 'a'))) "" (Just (RT.Repeat []))
    , testPM "manyr (manyMin a), aaa" (RT.manyr (RT.manyMin (RT.char 'a'))) "aaa" (Just (RT.Finite ["a","a","a"]))
    , testPM "manyMin (many a)" (RT.manyMin (many (RT.char 'a'))) "" (Just [])
    , testPM "manyMin (many a), aaa" (RT.manyMin (many (RT.char 'a'))) "aaa" (Just ["aaa"])
    , testPM "manyMin (manyr a)" (RT.manyMin (RT.manyr (RT.char 'a'))) "" (Just [])
    , testPM "manyMin (manyr a), aaa" (RT.manyMin (RT.manyr (RT.char 'a'))) "aaa" (Just [RT.Finite "aaa"])
    , testPM "manyMin (manyMin a)" (RT.manyMin (RT.manyMin (RT.char 'a'))) "" (Just [])
    , testPM "manyMin (manyMin a), aaa" (RT.manyMin (RT.manyMin (RT.char 'a'))) "aaa" (Just ["a","a","a"])
    ]
  , testGroup "toMatch"
    [ testPM "many (a *> (b <|> c)) abacac"
             (RT.toMatch $ many (RT.char 'a' *> (RT.char 'b' <|> RT.char 'c')))
             "abacac"
             (Just "abacac")
    ]
  , testGroup "withMatch"
    [ testPM "many (a *> (b <|> c)) abacac"
             (RT.withMatch $ many (RT.char 'a' *> (RT.char 'b' <|> RT.char 'c')))
             "abacac"
             (Just ("abacac", "bcc"))
    ]
  ]
-- TODO: Would be good to have more tests for toMatch and withMatch

listCombinatorTests :: TestTree
listCombinatorTests = testGroup "List combinators"
  [ testGroup "toMatch"
    [ testLPM "many (a *> (b <|> c)) abacac"
              (RL.toMatch $ many (RL.single 'a' *> (RL.single 'b' <|> RL.single 'c')))
              "abacac"
              (Just "abacac")
    ]
  , testGroup "withMatch"
    [ testLPM "many (a *> (b <|> c)) abacac"
              (RL.withMatch $ many (RL.single 'a' *> (RL.single 'b' <|> RL.single 'c')))
              "abacac"
              (Just ("abacac", "bcc"))
    ]
  ]

-- | Test parse and match
testPM :: (Eq a, Show a) => String -> RT.REText a -> T.Text -> Maybe a -> TestTree
testPM name re t res = testGroup name
  [ testCase "parse" $ RT.parse (RT.compile re) t @?= res
  , testCase "test" $ RT.parse (RT.compile (void re)) t @?= void res
  ]

-- | Test parse and match
testLPM :: (Eq a, Show a) => String -> RL.RE c a -> [c] -> Maybe a -> TestTree
testLPM name re t res = testGroup name
  [ testCase "parse" $ RL.parse (RL.compile re) t @?= res
  , testCase "test" $ RL.parse (RL.compile (void re)) t @?= void res
  ]

zeroOneString :: Gen String
zeroOneString = listOf (elements "01")

zeroOneText :: Gen Text
zeroOneText = T.pack <$> zeroOneString

------------
-- Compile
------------

compileTests :: TestTree
compileTests = testGroup "Compile tests"
  [ testGroup "compileBounded"
    [ testCase "mixRE 15" $
      assertBool "isJust" $ isJust (R.compileBounded 15 mixRE)
    , testCase "mixRE 14" $
      assertBool "isNothing" $ isNothing (R.compileBounded 14 mixRE)
    , testCase "mixRE 0" $
      assertBool "isNothing" $ isNothing (R.compileBounded 0 mixRE)
    , testCase "mixRE -1" $
      assertBool "isNothing" $ isNothing (R.compileBounded (-1) mixRE)
    ]
  ]
-- the exact size may change in the future, just test that there is _some_
-- threshold

mixRE :: R.RE c ()
mixRE =
  (() <$) .
  R.manyr .
  many .
  (\r -> Ap.liftA2 (\_ _ -> ()) r r) .
  (\r -> r <|> r) .
  fmap (const ()) $
  R.token (const (Just ()))

--------------------
-- Operations
--------------------

textOpTests :: TestTree
textOpTests = testGroup "Text operations"
  [ testGroup "find"
    [ testCase "abc abc ok" $ RT.find (RT.text "abc") "abc" @?= Just "abc"
    , testCase "abc abcd ok" $ RT.find (RT.text "abc") "abcd" @?= Just "abc"
    , testCase "bcd abcd ok" $ RT.find (RT.text "bcd") "abcd" @?= Just "bcd"
    , testCase "bcd abcde ok" $ RT.find (RT.text "bcd") "abcde" @?= Just "bcd"
    , testCase "abc abcabc ok" $ RT.find (RT.text "abc") "abcabc" @?= Just "abc"
    , testCase "aba ababababa ok" $ RT.find (RT.text "aba") "ababababa" @?= Just "aba"
    , testCase "abc ab fail" $ RT.find (RT.text "abc") "ab" @?= Nothing
    ]
  , testGroup "findAll"
    [ testCase "abc abc 1" $ RT.findAll (RT.text "abc") "abc" @?= ["abc"]
    , testCase "abc abcd 1" $ RT.findAll (RT.text "abc") "abcd" @?= ["abc"]
    , testCase "bcd abcd 1" $ RT.findAll (RT.text "bcd") "abcd" @?= ["bcd"]
    , testCase "bcd abcde 1" $ RT.findAll (RT.text "bcd") "abcde" @?= ["bcd"]
    , testCase "abc abcabc 2" $ RT.findAll (RT.text "abc") "abcabc" @?= ["abc","abc"]
    , testCase "aba ababababa 2" $ RT.findAll (RT.text "aba") "ababababa" @?= ["aba","aba"]
    , testCase "abc ab 0" $ RT.findAll (RT.text "abc") "ab" @?= []
    ]
  , testGroup "splitOn"
    [ testCase "abc abc" $ RT.splitOn (RT.text "abc") "abc" @?= ["",""]
    , testCase "abc abcd" $ RT.splitOn (RT.text "abc") "abcd" @?= ["","d"]
    , testCase "bcd abcd" $ RT.splitOn (RT.text "bcd") "abcd" @?= ["a",""]
    , testCase "bcd abcde" $ RT.splitOn (RT.text "bcd") "abcde" @?= ["a","e"]
    , testCase "abc abcabc" $ RT.splitOn (RT.text "abc") "abcabc" @?= ["","",""]
    , testCase "aba ababababa" $ RT.splitOn (RT.text "aba") "ababababa" @?= ["","b","ba"]
    , testCase "abc ab" $ RT.splitOn (RT.text "abc") "ab" @?= ["ab"]
    ]
  , testGroup "replace"
    [ testCase "abc xyz abc" $ RT.replace ("xyz" <$ RT.text "abc") "abc" @?= Just "xyz"
    , testCase "abc xyz abcd" $ RT.replace ("xyz" <$ RT.text "abc") "abcd" @?= Just "xyzd"
    , testCase "bcd xyz abcd" $ RT.replace ("xyz" <$ RT.text "bcd") "abcd" @?= Just "axyz"
    , testCase "bcd xyz abcde" $ RT.replace ("xyz" <$ RT.text "bcd") "abcde" @?= Just "axyze"
    , testCase "abc xyz abcabc" $ RT.replace ("xyz" <$ RT.text "abc") "abcabc" @?= Just "xyzabc"
    , testCase "aba xyz ababababa" $ RT.replace ("xyz" <$ RT.text "aba") "ababababa" @?= Just "xyzbababa"
    , testCase "abc xyz ab" $ RT.replace ("xyz" <$ RT.text "abc") "ab" @?= Nothing
    ]
  , testGroup "replaceAll"
    [ testCase "abc xyz abc" $ RT.replaceAll ("xyz" <$ RT.text "abc") "abc" @?= "xyz"
    , testCase "abc xyz abcd" $ RT.replaceAll ("xyz" <$ RT.text "abc") "abcd" @?= "xyzd"
    , testCase "bcd xyz abcd" $ RT.replaceAll ("xyz" <$ RT.text "bcd") "abcd" @?= "axyz"
    , testCase "bcd xyz abcde" $ RT.replaceAll ("xyz" <$ RT.text "bcd") "abcde" @?= "axyze"
    , testCase "abc xyz abcabc" $ RT.replaceAll ("xyz" <$ RT.text "abc") "abcabc" @?= "xyzxyz"
    , testCase "aba xyz ababababa" $ RT.replaceAll ("xyz" <$ RT.text "aba") "ababababa" @?= "xyzbxyzba"
    , testCase "abc xyz ab" $ RT.replaceAll ("xyz" <$ RT.text "abc") "ab" @?= "ab"
    ]
  ]

stringOpTests :: TestTree
stringOpTests = testGroup "String operations"
  [ testGroup "find"
    [ testCase "abc abc ok" $ RL.find (RL.list "abc") "abc" @?= Just "abc"
    , testCase "abc abcd ok" $ RL.find (RL.list "abc") "abcd" @?= Just "abc"
    , testCase "bcd abcd ok" $ RL.find (RL.list "bcd") "abcd" @?= Just "bcd"
    , testCase "bcd abcde ok" $ RL.find (RL.list "bcd") "abcde" @?= Just "bcd"
    , testCase "abc abcabc ok" $ RL.find (RL.list "abc") "abcabc" @?= Just "abc"
    , testCase "aba ababababa ok" $ RL.find (RL.list "aba") "ababababa" @?= Just "aba"
    , testCase "abc ab fail" $ RL.find (RL.list "abc") "ab" @?= Nothing
    ]
  , testGroup "findAll"
    [ testCase "abc abc 1" $ RL.findAll (RL.list "abc") "abc" @?= ["abc"]
    , testCase "abc abcd 1" $ RL.findAll (RL.list "abc") "abcd" @?= ["abc"]
    , testCase "bcd abcd 1" $ RL.findAll (RL.list "bcd") "abcd" @?= ["bcd"]
    , testCase "bcd abcde 1" $ RL.findAll (RL.list "bcd") "abcde" @?= ["bcd"]
    , testCase "abc abcabc 2" $ RL.findAll (RL.list "abc") "abcabc" @?= ["abc","abc"]
    , testCase "aba ababababa 2" $ RL.findAll (RL.list "aba") "ababababa" @?= ["aba","aba"]
    , testCase "abc ab 0" $ RL.findAll (RL.list "abc") "ab" @?= []
    ]
  , testGroup "splitOn"
    [ testCase "abc abc" $ RL.splitOn (RL.list "abc") "abc" @?= ["",""]
    , testCase "abc abcd" $ RL.splitOn (RL.list "abc") "abcd" @?= ["","d"]
    , testCase "bcd abcd" $ RL.splitOn (RL.list "bcd") "abcd" @?= ["a",""]
    , testCase "bcd abcde" $ RL.splitOn (RL.list "bcd") "abcde" @?= ["a","e"]
    , testCase "abc abcabc" $ RL.splitOn (RL.list "abc") "abcabc" @?= ["","",""]
    , testCase "aba ababababa" $ RL.splitOn (RL.list "aba") "ababababa" @?= ["","b","ba"]
    , testCase "abc ab" $ RL.splitOn (RL.list "abc") "ab" @?= ["ab"]
    ]
  , testGroup "replace"
    [ testCase "abc xyz abc" $ RL.replace ("xyz" <$ RL.list "abc") "abc" @?= Just "xyz"
    , testCase "abc xyz abcd" $ RL.replace ("xyz" <$ RL.list "abc") "abcd" @?= Just "xyzd"
    , testCase "bcd xyz abcd" $ RL.replace ("xyz" <$ RL.list "bcd") "abcd" @?= Just "axyz"
    , testCase "bcd xyz abcde" $ RL.replace ("xyz" <$ RL.list "bcd") "abcde" @?= Just "axyze"
    , testCase "abc xyz abcabc" $ RL.replace ("xyz" <$ RL.list "abc") "abcabc" @?= Just "xyzabc"
    , testCase "aba xyz ababababa" $ RL.replace ("xyz" <$ RL.list "aba") "ababababa" @?= Just "xyzbababa"
    , testCase "abc xyz ab" $ RL.replace ("xyz" <$ RL.list "abc") "ab" @?= Nothing
    ]
  , testGroup "replaceAll"
    [ testCase "abc xyz abc" $ RL.replaceAll ("xyz" <$ RL.list "abc") "abc" @?= "xyz"
    , testCase "abc xyz abcd" $ RL.replaceAll ("xyz" <$ RL.list "abc") "abcd" @?= "xyzd"
    , testCase "bcd xyz abcd" $ RL.replaceAll ("xyz" <$ RL.list "bcd") "abcd" @?= "axyz"
    , testCase "bcd xyz abcde" $ RL.replaceAll ("xyz" <$ RL.list "bcd") "abcde" @?= "axyze"
    , testCase "abc xyz abcabc" $ RL.replaceAll ("xyz" <$ RL.list "abc") "abcabc" @?= "xyzxyz"
    , testCase "aba xyz ababababa" $ RL.replaceAll ("xyz" <$ RL.list "aba") "ababababa" @?= "xyzbxyzba"
    , testCase "abc xyz ab" $ RL.replaceAll ("xyz" <$ RL.list "abc") "ab" @?= "ab"
    ]
  ]

---------
-- Base
---------

earlyFailureTests :: TestTree
earlyFailureTests = testGroup "Early failure"
  [ testCase "prepareParser cases" $ do
      void (R.prepareParser (R.compile empty)) @?= Nothing
      void (R.prepareParser (R.compile (pure () *> empty))) @?= Nothing
      void (R.prepareParser (R.compile (empty *> pure ()))) @?= Nothing
      void (R.prepareParser (R.compile (empty <|> empty))) @?= Nothing

      void (R.prepareParser (R.compile R.anySingle)) @?= Just ()
      void (R.prepareParser (R.compile (pure ()))) @?= Just ()

  , testGroup "stepParser"
    [ testCase ". a,aa" $
        case R.prepareParser (R.compile R.anySingle) of
          Nothing -> assertFailure "prepare"
          Just ps0 -> do
            R.finishParser ps0 @?= Nothing
            case R.stepParser ps0 'a' of
              Just ps1 -> do
                R.finishParser ps1 @?= Just 'a'
                void (R.stepParser ps1 'b') @?= Nothing
              _ -> assertFailure "step ps0"

    , testCase "* aa,aaa" $
        case R.prepareParser (R.compile (many R.anySingle)) of
          Nothing -> assertFailure "prepare"
          Just ps0 -> do
            R.finishParser ps0 @?= Just ""
            case R.stepParser ps0 'a' of
              Just ps1 -> do
                R.finishParser ps1 @?= Just "a"
                case R.stepParser ps1 'a' of
                  Just ps2 -> do
                    R.finishParser ps2 @?= Just "aa"
                    void (R.stepParser ps2 'a') @?= Just ()
                  _ -> assertFailure "step ps1"
              _ -> assertFailure "step ps0"

    , testCase "ab ax,abx" $
        case R.prepareParser (R.compile (RL.list "ab")) of
          Nothing -> assertFailure "prepare"
          Just ps0 -> do
            R.finishParser ps0 @?= Nothing
            case R.stepParser ps0 'a' of
              Just ps1 -> do
                R.finishParser ps1 @?= Nothing
                void (R.stepParser ps1 'x') @?= Nothing
                case R.stepParser ps1 'b' of
                  Just ps2 -> do
                    R.finishParser ps2 @?= Just "ab"
                    void (R.stepParser ps2 'x') @?= Nothing
                  _ -> assertFailure "step ps1"
              _ -> assertFailure "step ps0"
    ]
  ]

---------
-- Many
---------

manyTests :: TestTree
manyTests = testGroup "Many" $ map testLaws
  [ Laws.eqLaws (Proxy :: Proxy (RT.Many A))
  , Laws.ordLaws (Proxy :: Proxy (RT.Many OrdA))
  , Laws.functorLaws (Proxy :: Proxy RT.Many)
  ]
-- Cannot use foldableLaws because it cannot handle infinite structures.

------------
-- CharSet
------------

charSetTests :: TestTree
charSetTests = localOption (QuickCheckTests 1000) $ testGroup "CharSet"
  [ testGroup "Laws" $ map testLaws $
    let p = Proxy :: Proxy CS.CharSet in
    [ Laws.eqLaws p
    , Laws.semigroupLaws p
    , Laws.commutativeSemigroupLaws p
    , Laws.idempotentSemigroupLaws p
    , Laws.monoidLaws p
    ]
  , testGroup "fromList"
    [ testProperty "valid" $ \s -> validCS (CS.fromList s)
    , testProperty "member" $ \s c -> elem c s === CS.member c (CS.fromList s)
    ]
  , testGroup "insert"
    [ testProperty "valid" $ \c cs -> validCS (CS.insert c cs)
    , testProperty "member" $ \c cs -> CS.member c (CS.insert c cs)
    ]
  , testGroup "insertRange"
    [ testProperty "valid" $ \g cs -> validCS (CS.insertRange g cs)
    , testProperty "member" $
      \g cs c ->
        (CS.member c cs || inRange c g) == CS.member c (CS.insertRange g cs)
    ]
  , testGroup "delete"
    [ testProperty "valid" $ \c cs -> validCS (CS.delete c cs)
    , testProperty "member" $ \c cs -> not (CS.member c (CS.delete c cs))
    ]
  , testGroup "deleteRange"
    [ testProperty "valid" $ \g cs -> validCS (CS.deleteRange g cs)
    , testProperty "member" $
      \g cs c ->
        (CS.member c cs && not (inRange c g))
        == CS.member c (CS.deleteRange g cs)
    ]
  , testGroup "map"
    [ testProperty "valid" $ \cs (Fn f) -> validCS (CS.map f cs)
    , testProperty "member c . map f = elem c . map f . elems" $
      \cs c (Fn f) -> CS.member c (CS.map f cs) === elem c (map f (CS.elems cs))
    ]
  , testGroup "not"
    [ testProperty "valid" $ \cs -> validCS (CS.not cs)
    , testProperty "member" $
      \cs c -> CS.member c cs === CS.notMember c (CS.not cs)
    , testProperty "not . not = id" $ \cs -> CS.not (CS.not cs) === cs
    ]
  , testGroup "union"
    [ testProperty "valid" $ \lcs rcs -> validCS (CS.union lcs rcs)
    , testProperty "member" $
      \lcs rcs c ->
        (CS.member c lcs || CS.member c rcs) === CS.member c (CS.union lcs rcs)
    ]
  , testGroup "difference"
    [ testProperty "valid" $ \lcs rcs -> validCS (CS.difference lcs rcs)
    , testProperty "member" $
      \lcs rcs c ->
        (CS.member c lcs && CS.notMember c rcs)
        === CS.member c (CS.difference lcs rcs)
    ]
  , testGroup "intersection"
    [ testProperty "valid" $ \lcs rcs -> validCS (CS.intersection lcs rcs)
    , testProperty "member" $
      \lcs rcs c ->
        (CS.member c lcs && CS.member c rcs)
        === CS.member c (CS.intersection lcs rcs)
    ]
  , testProperty "fromString" $ \s -> fromString s === CS.fromList s
  , testProperty "<>" $ \lcs rcs -> lcs <> rcs === CS.union lcs rcs
  , testProperty "singleton" $ \c -> CS.singleton c === CS.fromList [c]
  , testProperty "fromRange" $ \cl cr c ->
      CS.member c (CS.fromRange (cl,cr)) === inRange c (cl,cr)
  , testProperty "ranges" $ \cs c ->
      CS.member c cs === any (inRange c) (CS.ranges cs)
  ]

validCS :: CS.CharSet -> Property
validCS cs = counterexample (show cs) $ CS.valid cs

-----------------
-- Common utils
-----------------

data T = TA | TB | TC deriving Show

instance Arbitrary T where
  arbitrary = elements [TA,TB,TC]

data Chain a
  = One a
  | TwoA (Chain a) (Chain a)
  | TwoB (Chain a) (Chain a)
  deriving (Eq, Show)

inRange :: Ord a => a -> (a, a) -> Bool
inRange x (l,h) = l <= x && x <= h

testLaws :: Laws -> TestTree
testLaws (Laws class_ tests) =
  testGroup class_ (map (uncurry testProperty) tests)

instance Arbitrary a => Arbitrary (RT.Many a) where
  arbitrary = frequency [ (1, RT.Repeat <$> arbitrary)
                        , (3, RT.Finite <$> arbitrary)
                        ]

instance Arbitrary CS.CharSet where
  arbitrary = CS.fromList <$> arbitrary
  shrink = map CS.fromList . shrink . CS.elems

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary
  -- Arbitrary Char generates valid Unicode (perhaps it shouldn't) so this
  -- is fine.

  shrink = map T.pack . shrink . T.unpack

-- Available in Data.List in base >= 4.19
unsnoc :: [a] -> Maybe ([a], a)
unsnoc = foldr (\x -> Just . maybe ([], x) (\(~(a, b)) -> (x : a, b))) Nothing
