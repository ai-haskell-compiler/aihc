{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Token.Tests
  ( testToken
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.Token
import Zenacy.HTML.Internal.Types
import Control.Monad
import Control.Monad.ST
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )

testToken :: Test
testToken = testGroup "Zenacy.HTML.Internal.Token"
  [ testType
  , testDoctype
  , testStart
  , testAttr
  , testEnd
  , testComment
  , testChar
  , testEof
  , testCount
  , testOffset
  , testDrop
  , testSize
  , testEmit
  , testIter
  , testCapacity
  , testStartName
  , testEndName
  , testPrune
  ]

testType :: Test
testType = testCase "token type" $ do
  assertEqual "TEST 1" (Just tokenDoctypeType) $
    runST $ f tokenDoctypeInit
  assertEqual "TEST 2" (Just tokenTagStartType) $
    runST $ f tokenTagStartInit
  assertEqual "TEST 3" (Just tokenTagEndType) $
    runST $ f tokenTagEndInit
  assertEqual "TEST 4" (Just tokenCommentType) $
    runST $ f tokenCommentInit
  assertEqual "TEST 5" (Just tokenCharType) $
    runST $ f $ tokenCharInit (ctow 'a')
  assertEqual "TEST 6" (Just tokenEOFType) $
    runST $ f tokenEOFInit
  where
    f g = do
      b <- tokenBuffer
      g b
      i <- tokenFirst b
      if i == 0
         then pure Nothing
         else Just <$> tokenType i b

testDoctype :: Test
testDoctype = testCase "token doctype" $ do
  assertEqual "TEST 1"
    (Just $ TDoctype "html" False Nothing Nothing) $
    runST $ do
      t <- tokenBuffer
      tokenDoctypeInit t
      tokenDoctypeNameAppend (ctow 'h') t
      tokenDoctypeNameAppend (ctow 't') t
      tokenDoctypeNameAppend (ctow 'm') t
      tokenDoctypeNameAppend (ctow 'l') t
      packFirst t
  assertEqual "TEST 2"
    (Just $ TDoctype "html" False (Just "abc") Nothing) $
    runST $ do
      t <- tokenBuffer
      tokenDoctypeInit t
      tokenDoctypeNameAppend (ctow 'h') t
      tokenDoctypeNameAppend (ctow 't') t
      tokenDoctypeNameAppend (ctow 'm') t
      tokenDoctypeNameAppend (ctow 'l') t
      tokenDoctypePublicIdInit t
      tokenDoctypePublicIdAppend (ctow 'a') t
      tokenDoctypePublicIdAppend (ctow 'b') t
      tokenDoctypePublicIdAppend (ctow 'c') t
      packFirst t
  assertEqual "TEST 3"
    (Just $ TDoctype "html" True (Just "abc") (Just "efg")) $
    runST $ do
      t <- tokenBuffer
      tokenDoctypeInit t
      tokenDoctypeNameAppend (ctow 'h') t
      tokenDoctypeNameAppend (ctow 't') t
      tokenDoctypeNameAppend (ctow 'm') t
      tokenDoctypeNameAppend (ctow 'l') t
      tokenDoctypePublicIdInit t
      tokenDoctypePublicIdAppend (ctow 'a') t
      tokenDoctypePublicIdAppend (ctow 'b') t
      tokenDoctypePublicIdAppend (ctow 'c') t
      tokenDoctypeSystemIdInit t
      tokenDoctypeSystemIdAppend (ctow 'e') t
      tokenDoctypeSystemIdAppend (ctow 'f') t
      tokenDoctypeSystemIdAppend (ctow 'g') t
      tokenDoctypeSetForceQuirks t
      packFirst t

testStart :: Test
testStart = testCase "token start" $ do
  assertEqual "TEST 1"
    (Just $ TStart "abc" False []) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'a') t
      tokenTagNameAppend (ctow 'b') t
      tokenTagNameAppend (ctow 'c') t
      packFirst t

testAttr :: Test
testAttr = testCase "token attr" $ do
  assertEqual "TEST 1"
    (Just $ TStart "abc" False [tokenAttr "id" "108"]) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'a') t
      tokenTagNameAppend (ctow 'b') t
      tokenTagNameAppend (ctow 'c') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrValAppend (ctow '1') t
      tokenAttrValAppend (ctow '0') t
      tokenAttrValAppend (ctow '8') t
      packFirst t
  assertEqual "TEST 2"
    (Just $ TStart "a" False [tokenAttr "x" ""]) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'a') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'x') t
      packFirst t
  assertEqual "TEST 3"
    (Just $ TStart "a" False
      [tokenAttr "x" "", tokenAttr "y" ""]) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'a') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'x') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'y') t
      packFirst t
  assertEqual "TEST 4"
    (Just $ TStart "div" False
      [ tokenAttr "id" "menu"
      , tokenAttr "hidden" ""
      , tokenAttr "data" "http"
      ]) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'd') t
      tokenTagNameAppend (ctow 'i') t
      tokenTagNameAppend (ctow 'v') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrValAppend (ctow 'm') t
      tokenAttrValAppend (ctow 'e') t
      tokenAttrValAppend (ctow 'n') t
      tokenAttrValAppend (ctow 'u') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'h') t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'e') t
      tokenAttrNameAppend (ctow 'n') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'a') t
      tokenAttrNameAppend (ctow 't') t
      tokenAttrNameAppend (ctow 'a') t
      tokenAttrValAppend (ctow 'h') t
      tokenAttrValAppend (ctow 't') t
      tokenAttrValAppend (ctow 't') t
      tokenAttrValAppend (ctow 'p') t
      packFirst t

testEnd :: Test
testEnd = testCase "token end" $ do
  assertEqual "TEST 1"
    (Just $ TEnd "abc") $
    runST $ do
      t <- tokenBuffer
      tokenTagEndInit t
      tokenTagNameAppend (ctow 'a') t
      tokenTagNameAppend (ctow 'b') t
      tokenTagNameAppend (ctow 'c') t
      packFirst t

testComment :: Test
testComment = testCase "token comment" $ do
  assertEqual "TEST 1"
    (Just $ TComment "abc") $
    runST $ do
      t <- tokenBuffer
      tokenCommentInit t
      tokenCommentAppend (ctow 'a') t
      tokenCommentAppend (ctow 'b') t
      tokenCommentAppend (ctow 'c') t
      packFirst t

testChar :: Test
testChar = testCase "token char" $ do
  assertEqual "TEST 1"
    (Just $ TChar $ ctow 'a') $
    runST $ do
      t <- tokenBuffer
      tokenCharInit (ctow 'a') t
      packFirst t

testEof :: Test
testEof = testCase "token eof" $ do
  assertEqual "TEST 1"
    (Just TEOF) $
    runST $ do
      t <- tokenBuffer
      tokenEOFInit t
      packFirst t
  assertEqual "TEST 2"
    True $
    runST $ do
      t <- tokenBuffer
      tokenEOFInit t
      tokenHasEOF t
  assertEqual "TEST 3"
    True $
    runST $ do
      t <- tokenBuffer
      tokenCommentInit t
      tokenTagStartInit t
      tokenTagStartInit t
      tokenEOFInit t
      tokenHasEOF t
  assertEqual "TEST 4"
    False $
    runST $ do
      t <- tokenBuffer
      tokenCommentInit t
      tokenTagStartInit t
      tokenTagStartInit t
      tokenHasEOF t

testCount :: Test
testCount = testCase "token count" $ do
  assertEqual "TEST 1" 0 $
    runST $ do
      t <- tokenBuffer
      tokenCount t
  assertEqual "TEST 2" 1 $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenCount t
  assertEqual "TEST 3" 8 $
    runST $ do
      t <- tokenBuffer
      tokenDoctypeInit t
      tokenTagStartInit t
      tokenTagEndInit t
      tokenTagStartInit t
      tokenTagEndInit t
      tokenCommentInit t
      tokenCharInit (ctow 'a') t
      tokenEOFInit t
      tokenCount t

testOffset :: Test
testOffset = testCase "token offset" $ do
  assertEqual "TEST 1" [] $
    runST $ do
      t <- tokenBuffer
      tokenOffset t
  assertEqual "TEST 2" [4] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tokenOffset t
  assertEqual "TEST 3" [4,10] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tag t "div"
      tokenOffset t
  assertEqual "TEST 4" [4,10,16] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tag t "div"
      tag t "a"
      tokenOffset t
  where
    tag t x = do
      tokenTagStartInit t
      mapM_ (flip tokenTagNameAppend t) $ bsUnpack x

testDrop :: Test
testDrop = testCase "token drop" $ do
  assertEqual "TEST 1" ["p", "div"] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tag t "div"
      tag t "a"
      tokenDrop t
      tokenOffset t >>= mapM (f t)
  assertEqual "TEST 2" ["p"] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tag t "div"
      tag t "a"
      tokenDrop t
      tokenDrop t
      tokenOffset t >>= mapM (f t)
  assertEqual "TEST 3" [] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tag t "div"
      tag t "a"
      tokenDrop t
      tokenDrop t
      tokenDrop t
      tokenOffset t >>= mapM (f t)
  assertEqual "TEST 4" ["div"] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tokenDrop t
      tag t "div"
      tag t "a"
      tokenDrop t
      tokenOffset t >>= mapM (f t)
  assertEqual "TEST 5" [] $
    runST $ do
      t <- tokenBuffer
      tokenDrop t
      tokenOffset t >>= mapM (f t)
  assertEqual "TEST 6" [] $
    runST $ do
      t <- tokenBuffer
      tag t "p"
      tokenDrop t
      tokenOffset t >>= mapM (f t)
  where
    tag t x = do
      tokenTagStartInit t
      mapM_ (flip tokenTagNameAppend t) $ bsUnpack x
    f t x =
      tokenTagStartName x t >>= pure . maybe bsEmpty bsPack

testSize :: Test
testSize = testCase "token size" $ do
  assertEqual "TEST 1" Nothing $
    runST $ do
      t <- tokenBuffer
      sizeFirst t
  assertEqual "TEST 2" (Just 11) $
    runST $ do
      t <- tokenBuffer
      tokenDoctypeInit t
      sizeFirst t
  assertEqual "TEST 3" (Just 4) $
    runST $ do
      t <- tokenBuffer
      tokenTagEndInit t
      sizeFirst t
  assertEqual "TEST 4" (Just 4) $
    runST $ do
      t <- tokenBuffer
      tokenCommentInit t
      sizeFirst t
  assertEqual "TEST 5" (Just 3) $
    runST $ do
      t <- tokenBuffer
      tokenCharInit (ctow 'a') t
      sizeFirst t
  assertEqual "TEST 6" (Just 2) $
    runST $ do
      t <- tokenBuffer
      tokenEOFInit t
      sizeFirst t
  assertEqual "TEST 7" (Just 6) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      sizeFirst t
  assertEqual "TEST 8" (Just 10) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenAttrInit t
      sizeFirst t
  assertEqual "TEST 9" (Just 26) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenAttrInit t
      tokenAttrInit t
      tokenAttrInit t
      tokenAttrInit t
      tokenAttrInit t
      sizeFirst t

testEmit :: Test
testEmit = testCase "token emit" $ do
  assertEqual "TEST 1" ["a","b","c","d","e","f"] $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'a') t
      tokenAttrInit t
      tokenTagEndInit t
      tokenTagNameAppend (ctow 'b') t
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'c') t
      tokenAttrInit t
      tokenTagEndInit t
      tokenTagNameAppend (ctow 'd') t
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'e') t
      tokenAttrInit t
      tokenTagEndInit t
      tokenTagNameAppend (ctow 'f') t
      a <- tokenList t
      pure $ map tokenName a

testIter :: Test
testIter = testCase "token iter" $ do
  assertEqual "TEST 1" 0 $
    runST $ do
      tokenBuffer >>= tokenFirst
  assertEqual "TEST 2" 0 $
    runST $ do
      tokenBuffer >>= tokenNext

testCapacity :: Test
testCapacity = testCase "token capacity" $ do
  assertEqual "TEST 1" (100,100) $
    runST $ do
      t <- tokenBuffer
      tokenCapacity t
  assertEqual "TEST 2" (3200,25600) $
    runST $ do
      t <- tokenBuffer
      forM_ [1..200] $ const $ addToken t
      tokenCapacity t
  where
    addToken t = do
      tokenTagStartInit t
      forM_ [1..10] $ const $
        tokenTagNameAppend (ctow 'a') t
      tokenAttrInit t
      forM_ [1..10] $ const $
        tokenAttrNameAppend (ctow 'b') t
      forM_ [1..40] $ const $
        tokenAttrValAppend (ctow 'c') t
      tokenTagEndInit t
      forM_ [1..5] $ const $
        tokenTagNameAppend (ctow 'x') t

testStartName :: Test
testStartName = testCase "token start name" $ do
  assertEqual "TEST 1" [Just "p", Just "a", Nothing, Just "div"] $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'p') t
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'a') t
      tokenTagEndInit t
      tokenTagNameAppend (ctow 'a') t
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'd') t
      tokenTagNameAppend (ctow 'i') t
      tokenTagNameAppend (ctow 'v') t
      tokenOffset t >>= mapM (f t)
  assertEqual "TEST 2" (Just "i") $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'i') t
      tokenTail t >>= f t
  where
    f t x =
      tokenTagStartName x t >>= pure . maybe Nothing (Just . bsPack)

testEndName :: Test
testEndName = testCase "token end name" $ do
  assertEqual "TEST 1" (Just "b") $
    runST $ do
      t <- tokenBuffer
      tokenTagEndInit t
      tokenTagNameAppend (ctow 'b') t
      tokenTail t >>= f t
  where
    f t x =
      tokenTagEndName x t >>= pure . maybe Nothing (Just . bsPack)

testPrune :: Test
testPrune = testCase "token prune" $ do
  assertEqual "TEST 1"
    (True, Just $ TStart "div" False
      [ tokenAttr "id" "menu"
      , tokenAttr "hidden" ""
      , tokenAttr "data" "http"
      ]) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'd') t
      tokenTagNameAppend (ctow 'i') t
      tokenTagNameAppend (ctow 'v') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrValAppend (ctow 'm') t
      tokenAttrValAppend (ctow 'e') t
      tokenAttrValAppend (ctow 'n') t
      tokenAttrValAppend (ctow 'u') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'h') t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'e') t
      tokenAttrNameAppend (ctow 'n') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'a') t
      tokenAttrNameAppend (ctow 't') t
      tokenAttrNameAppend (ctow 'a') t
      tokenAttrValAppend (ctow 'h') t
      tokenAttrValAppend (ctow 't') t
      tokenAttrValAppend (ctow 't') t
      tokenAttrValAppend (ctow 'p') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrNameAppend (ctow 'a') t
      tokenAttrNameAppend (ctow 't') t
      tokenAttrNameAppend (ctow 'a') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrValAppend (ctow 'x') t
      i <- tokenTail t
      a <- tokenAttrNamePrune i t
      b <- packFirst t
      pure (a, b)
  assertEqual "TEST 2"
    (False, Just $ TStart "div" False
      [ tokenAttr "id" "1"
      ]) $
    runST $ do
      t <- tokenBuffer
      tokenTagStartInit t
      tokenTagNameAppend (ctow 'd') t
      tokenTagNameAppend (ctow 'i') t
      tokenTagNameAppend (ctow 'v') t
      tokenAttrInit t
      tokenAttrNameAppend (ctow 'i') t
      tokenAttrNameAppend (ctow 'd') t
      tokenAttrValAppend (ctow '1') t
      tokenAttrInit t
      i <- tokenTail t
      a <- tokenAttrNamePrune i t
      b <- packFirst t
      pure (a, b)

packFirst t = do
  i <- tokenFirst t
  if i == 0
     then pure Nothing
     else Just <$> tokenPack i t

sizeFirst t = do
  i <- tokenFirst t
  if i == 0
     then pure Nothing
     else Just <$> tokenSize i t

tokenName (TStart n _ _) = n
tokenName (TEnd n) = n
tokenName _ = ""
