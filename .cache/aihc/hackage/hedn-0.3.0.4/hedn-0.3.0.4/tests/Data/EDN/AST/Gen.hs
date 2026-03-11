{-# LANGUAGE OverloadedStrings #-}

module Data.EDN.AST.Gen
  ( module Data.EDN.AST.Gen
  ) where

import Data.Function (on)
import Data.List (nubBy)
import Data.Text (Text)
import Hedgehog (Gen)

import qualified Data.Text as Text
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.EDN.AST.Parser as Parser
import qualified Data.EDN.AST.Types as EDN

genTaggedValue :: Gen EDN.TaggedValue
genTaggedValue = genTagged genValue

genTagged :: Gen a -> Gen (EDN.Tagged Text a)
genTagged gen = Gen.choice
  [ withoutTag
  , withNS
  , withoutNS
  ]
  where
    withNS = EDN.Tagged
      <$> genTagIdent
      <*> genTagIdent
      <*> gen

    -- XXX: the spec says only some raw names are valid
    withoutNS = EDN.Tagged
      <$> pure ""
      <*> genTagIdent
      <*> gen

    withoutTag = EDN.NoTag <$> gen

genTagIdent :: Gen Text
genTagIdent = genText Parser.tagChars

genValue :: Gen EDN.Value
genValue = Gen.recursive Gen.choice atomic collections

atomic :: [Gen EDN.Value]
atomic =
  [ genNil
  , genBool
  , genString
  , genChar
  , genInteger
  , genFloating
  , genKeyword
  , genSymbol
  ]

collections :: [Gen EDN.Value]
collections =
  [ genSet
  , genMap
  , genVector
  , genList
  ]

genBool :: Gen EDN.Value
genBool = EDN.Boolean
  <$> Gen.element [True, False]

genNil :: Gen EDN.Value
genNil = pure EDN.Nil

genString :: Gen EDN.Value
genString = EDN.String
  <$> Gen.text (Range.exponential 4 16) Gen.unicode

genChar :: Gen EDN.Value
genChar = EDN.Character
  <$> Gen.unicode

genInteger :: Gen EDN.Value
genInteger = EDN.Integer . fromIntegral
  <$> Gen.int64 Range.exponentialBounded

genFloating :: Gen EDN.Value
genFloating = EDN.Floating
  <$> Gen.double (Range.exponentialFloat (-1e-308) 1e308) -- XXX: 🤷

genKeyword :: Gen EDN.Value
genKeyword = EDN.Keyword
  <$> genKeywordName

genKeywordName :: Gen Text
genKeywordName = genTextInitial Parser.keywordInitialChars Parser.keywordChars

genSymbol :: Gen EDN.Value
genSymbol = EDN.Symbol
  <$> genSymbolName
  <*> genSymbolName

genSymbolName :: Gen Text
genSymbolName = genTextInitial Parser.symbolInitialChars Parser.symbolChars

genSet :: Gen EDN.Value
genSet = EDN.mkSet
  <$> genNubList (0, 16) 0 id genTaggedValue

genMap :: Gen EDN.Value
genMap = do
  keys <- genNubList (0, 16) 0 id genMapKey
  values <- Gen.list (Range.singleton $ length keys) genTaggedValue
  pure . EDN.mkMap $ zip keys values

genMapKey :: Gen EDN.TaggedValue
genMapKey = genTagged $ Gen.choice atomic

genVector :: Gen EDN.Value
genVector = EDN.mkVec
  <$> Gen.list (Range.exponential 0 16) genTaggedValue

genList :: Gen EDN.Value
genList = EDN.mkList
  <$> Gen.list (Range.exponential 0 16) genTaggedValue

-- * Helpers

genText :: [Char] -> Gen Text
genText = Gen.text (Range.exponential 4 16) . Gen.element

genTextInitial :: [Char] -> [Char] -> Gen Text
genTextInitial initial chars = Gen.choice
  [ genSingleton
  , genLong'ish
  ]
  where
    genSingleton = Text.singleton
      <$> Gen.element initial

    genLong'ish = Text.cons
      <$> Gen.element initial
      <*> genText chars

genNubList
  :: Eq key
  => (Int, Int)
  -> Int
  -> (a -> key)
  -> Gen a
  -> Gen [a]
genNubList (from, to) atLeast key gen = do
  items <- Gen.list (Range.exponential from to) gen
  let unique = nubBy ((==) `on` key) items
  if length unique < atLeast
    then
      Gen.discard
    else
      pure $ take to unique
