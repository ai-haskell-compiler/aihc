-- Copyright (C) 2014 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Api (suite_Api) where

import Chell (nothing)
import Options
import Test.Hspec (Spec, context, shouldBe, specify)

suite_Api :: Spec
suite_Api = context "api" do
  test_RepeatedFlags
  test_CompatibleDuplicateFlags
  test_ConflictingDuplicateFlags

data RepeatedStringOpts = RepeatedStringOpts [String]
  deriving (Eq, Show)

repeatedStringList :: OptionType [String]
repeatedStringList =
  (optionType "repeated-string-list" [] (\x -> Right [x]) show)
    { optionTypeMerge = Just concat
    }

instance Options RepeatedStringOpts where
  defineOptions =
    pure RepeatedStringOpts
      <*> defineOption
        repeatedStringList
        ( \o ->
            o
              { optionShortFlags = ['s']
              }
        )

test_RepeatedFlags :: Spec
test_RepeatedFlags = specify "repeated-flags" do
  let parsed = parseOptions ["-sfoo", "-sbar", "-sbaz"]
  nothing (parsedError parsed)
  shouldBe (parsedOptions parsed) (Just (RepeatedStringOpts ["foo", "bar", "baz"]))

data CompatibleDuplicateOpts = CompatibleDuplicateOpts SubOpts1 SubOpts1
  deriving (Eq, Show)

instance Options CompatibleDuplicateOpts where
  defineOptions =
    pure CompatibleDuplicateOpts
      <*> defineOptions
      <*> defineOptions

data ConflictingDuplicateOpts = ConflictingDuplicateOpts SubOpts1 SubOpts2
  deriving (Eq, Show)

instance Options ConflictingDuplicateOpts where
  defineOptions =
    pure ConflictingDuplicateOpts
      <*> defineOptions
      <*> defineOptions

data SubOpts1 = SubOpts1 Integer
  deriving (Eq, Show)

data SubOpts2 = SubOpts2 Integer
  deriving (Eq, Show)

instance Options SubOpts1 where
  defineOptions =
    pure SubOpts1
      <*> simpleOption "int" 0 ""

instance Options SubOpts2 where
  defineOptions =
    pure SubOpts2
      <*> simpleOption "int" 1 ""

test_CompatibleDuplicateFlags :: Spec
test_CompatibleDuplicateFlags = specify "compatible-duplicate-flags" do
  let parsed = parseOptions ["--int=10"]
  nothing (parsedError parsed)
  shouldBe (parsedOptions parsed) (Just (CompatibleDuplicateOpts (SubOpts1 10) (SubOpts1 10)))

test_ConflictingDuplicateFlags :: Spec
test_ConflictingDuplicateFlags = specify "conflicting-duplicate-flags" do
  let parsed = parseOptions ["-sfoo", "-sbar", "-sbaz"]
  shouldBe (parsedError parsed) (Just "Duplicate option flag \"--int\".")
  nothing (parsedOptions parsed :: Maybe ConflictingDuplicateOpts)
