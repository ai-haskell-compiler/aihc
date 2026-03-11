{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Text.ParserCombinators.Parsec.Numeric
  ( tests
  ) where

import Data.Functor.Identity
import Text.Parsec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Text.ParserCombinators.Parsec.Numeric

tests :: TestTree
tests = $(testGroupGenerator)

parseFromStreamTest :: (Integral r, Stream s Identity Char) => s -> r -> Assertion
parseFromStreamTest source required =
  case parse nat "" source of
    Left err -> error (show err)
    Right  v -> if v == required
                then return ()
                else error "Test from source type was not correct value."

case_parseFromByteString :: Assertion
case_parseFromByteString = parseFromStreamTest ("23"::BS.ByteString) (23::Word)

case_parseFromByteStringLazy :: Assertion
case_parseFromByteStringLazy = parseFromStreamTest ("23"::LBS.ByteString) (23::Word)

case_parseFromText :: Assertion
case_parseFromText = parseFromStreamTest ("23"::T.Text) (23::Word)

case_parseFromTextLazy :: Assertion
case_parseFromTextLazy = parseFromStreamTest ("23"::LT.Text) (23::Word)

case_parseFromString :: Assertion
case_parseFromString = parseFromStreamTest ("23"::String) (23::Word)
