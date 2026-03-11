{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Zenacy.HTML.Internal.Lexer.Tests
  ( testLexer
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.Lexer
import Zenacy.HTML.Internal.Token
import Zenacy.HTML.Internal.Types
import Control.Monad.ST
import Data.Default
  ( Default(..)
  )
import Data.DList
  ( DList
  )
import qualified Data.DList as D
  ( empty
  , snoc
  , toList
  )
import Data.Either.Extra
  ( fromRight
  )
import Data.Monoid
  ( (<>)
  )
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
import Text.RawString.QQ

testLexer :: Test
testLexer = testGroup "Zenacy.HTML.Internal.Lexer"
  [ testBasic
  , testBuffer
  , testComment
  , testDoctype
  , testSelfClose
  , testPrune
  , testCharRef
  , testScript
  ]

testBasic :: Test
testBasic = testCase "lexer basic" $ do
  assertEqual "TEST 1"
    [ TStart "html" False []
    , TStart "body" False []
    , TStart "a" False [tokenAttr "href" "https://example.com"]
    , TEnd "a"
    , TEnd "body"
    , TEnd "html"
    , TEOF
    ] $ getTokens htmlData

testBuffer :: Test
testBuffer = testCase "lexer buffer" $ do
  assertEqual "TEST 1"
    [ TStart "html" False []
    , TStart "body" False []
    , TStart "a" False [tokenAttr "href" "https://example.com"]
    , TEnd "a"
    , TEnd "body"
    , TEnd "html"
    , TEOF
    ] $ getTokens htmlData

testComment :: Test
testComment = testCase "lexer comment" $ do
  assertEqual "TEST 1"
    [ TComment "abcxyz"
    , TComment " abcxyz "
    , TEOF
    ] $ getTokens htmlComm

testDoctype :: Test
testDoctype = testCase "lexer doctype" $ do
  assertEqual "TEST 1"
    [ TDoctype "html" False Nothing Nothing, TEOF ] $
    getTokens [r|<!DOCTYPE html>|]
  assertEqual "TEST 2"
    [ TDoctype "html" False (Just "A") (Just "B"), TEOF ] $
    getTokens [r|<!DOCTYPE html PUBLIC "A" "B">|]
  assertEqual "TEST 3"
    [ TDoctype "html" False Nothing (Just "x"), TEOF ] $
    getTokens [r|<!DOCTYPE html SYSTEM "x">|]
  assertEqual "TEST 4"
    [ TDoctype "html" True Nothing Nothing, TEOF ] $
    getTokens [r|<!DOCTYPE html SYSTEM>|]
  assertEqual "TEST 5"
    [ TDoctype "html" False Nothing Nothing, TEOF ] $
    getTokens [r|<!doctype html>|]
  assertEqual "TEST 6"
    [ TDoctype "html" False Nothing Nothing, TEOF ] $
    getTokens [r|<!DOCTYPE HTML>|]

testSelfClose :: Test
testSelfClose = testCase "lexer self close" $ do
  assertEqual "TEST 1"
    [ TStart "meta" True [tokenAttr "charset" "UTF-8"]
    , TEOF
    ] $ getTokens [r|<meta charset="UTF-8"/>|]

testPrune :: Test
testPrune = testCase "lexer prune" $ do
  assertEqual "TEST 1"
    [ TStart "div" True [tokenAttr "id" "1"]
    , TEOF
    ] $ getTokens [r|<div id="1" id="2" id="3"/>|]

testCharRef :: Test
testCharRef = testCase "lexer char ref" $ do
  assertEqual "TEST 1"
    [ TStart "div" True [tokenAttr "a" "\226\136\128"]
    , TEOF
    ] $ getTokens [r|<div a="&forall;"/>|]
  assertEqual "TEST 2"
    [ TStart "div" True [tokenAttr "b" "\226\135\146"]
    , TEOF
    ] $ getTokens [r|<div b='&Implies;'/>|]
  assertEqual "TEST 3"
    [ TStart "div" False [tokenAttr "c" "\226\136\183"]
    , TEOF
    ] $ getTokens [r|<div c=&Proportion;>|]
  assertEqual "TEST 4"
    [ TStart "div" False []
    , TChar 226
    , TChar 136
    , TChar 130
    , TEnd "div"
    , TEOF
    ] $ getTokens [r|<div>&PartialD;</div>|]
  assertEqual "TEST 5"
    [ TStart "div" False [tokenAttr "a" "\226\136\128\226\136\128"]
    , TEOF
    ] $ getTokens [r|<div a="&#x02200;&#8704;">|]

testScript :: Test
testScript = testCase "lexer script" $ do
  run "TEST 1" "var a = 0;"
  where
    run x y = assertEqual x (res y) $ getTokens (scr y)
    scr x = "<script>" <> x <> "</script>"
    res x = [TStart "script" False []]
         <> map TChar (bsUnpack x)
         <> [TEnd "script", TEOF]

getTokens :: BS -> [Token]
getTokens s =
  runST $ do
    lexerNew def { lexerOptionInput = s } >>= \case
      Left e -> pure []
      Right x -> go x D.empty
  where
    go x a = do
      lexerNext x >>= \case
        TEOF -> pure $ D.toList $ D.snoc a TEOF
        token -> go x $ D.snoc a token

htmlData = [r|<html><body><a href="https://example.com"></a></body></html>|]

htmlComm = [r|<!--abcxyz--><!-- abcxyz -->|]
