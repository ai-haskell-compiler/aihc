-- Copyright (C) 2012 John Millikin <jmillikin@gmail.com>
--
-- See license.txt for details
module OptionsTests.Tokenize (suite_Tokenize) where

import Chell (left, right)
import Options.Tokenize
import Options.Types
import Test.Hspec (Expectation, Spec, context, shouldBe, specify)

suite_Tokenize :: Spec
suite_Tokenize = context "tokenize" do
  test_Empty
  test_NoFlag
  test_ShortFlag
  test_ShortFlagUnknown
  test_ShortFlagMissing
  test_ShortFlagUnary
  test_ShortFlagDuplicate
  test_LongFlag
  test_LongFlagUnknown
  test_LongFlagMissing
  test_LongFlagUnary
  test_LongFlagDuplicate
  test_EndFlags
  test_Subcommand
  test_SubcommandUnknown
  test_Unicode

commandDefs :: OptionDefinitions
commandDefs =
  OptionDefinitions
    [ OptionInfo (OptionKey "test.a") ['a'] ["long-a"] "default" False False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.x") ['x'] ["long-x"] "default" True False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.y") ['y'] ["long-y"] "default" True False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.z") ['z'] ["long-z"] "default" True False "" Nothing Nothing ""
    ]
    []

subcommandDefs :: OptionDefinitions
subcommandDefs =
  OptionDefinitions
    [ OptionInfo (OptionKey "test.a") ['a'] ["long-a"] "default" False False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.b") ['b'] ["long-b"] "default" False False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.x") ['x'] ["long-x"] "default" True False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.y") ['y'] ["long-y"] "default" True False "" Nothing Nothing "",
      OptionInfo (OptionKey "test.z") ['z'] ["long-z"] "default" True False "" Nothing Nothing ""
    ]
    [ ( "sub1",
        [ OptionInfo (OptionKey "sub.d") ['d'] ["long-d"] "default" False False "" Nothing Nothing "",
          OptionInfo (OptionKey "sub.e") ['e'] ["long-e"] "default" True False "" Nothing Nothing ""
        ]
      ),
      ( "sub2",
        [ OptionInfo (OptionKey "sub.d") ['d'] ["long-d"] "default" True False "" Nothing Nothing "",
          OptionInfo (OptionKey "sub.e") ['e'] ["long-e"] "default" True False "" Nothing Nothing ""
        ]
      )
    ]

unicodeDefs :: OptionDefinitions
unicodeDefs =
  OptionDefinitions
    [ OptionInfo (OptionKey "test.a") ['\12354'] ["long-\12354"] "default" False False "" Nothing Nothing ""
    ]
    []

test_Empty :: Spec
test_Empty = specify "empty" do
  let (subcmd, eTokens) = tokenize commandDefs []
  shouldBe Nothing subcmd
  right eTokens

  let Right (Tokens tokens args) = eTokens
  equalTokens [] tokens
  shouldBe [] args

test_NoFlag :: Spec
test_NoFlag = specify "no-flag" do
  let (subcmd, eTokens) = tokenize commandDefs ["-", "foo", "bar"]
  shouldBe Nothing subcmd
  right eTokens

  let Right (Tokens tokens args) = eTokens
  equalTokens [] tokens
  shouldBe ["-", "foo", "bar"] args

test_ShortFlag :: Spec
test_ShortFlag = specify "short-flag" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-a", "foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.a", Token "-a" "foo")] tokens
    shouldBe ["bar"] args
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.a", Token "-a" "foo")] tokens
    shouldBe ["bar"] args

test_ShortFlagUnknown :: Spec
test_ShortFlagUnknown = specify "short-flag-unknown" do
  let (subcmd, eTokens) = tokenize commandDefs ["-c", "foo", "bar"]
  shouldBe Nothing subcmd
  left eTokens

  let Left err = eTokens
  shouldBe "Unknown flag -c" err

test_ShortFlagMissing :: Spec
test_ShortFlagMissing = specify "short-flag-missing" do
  let (subcmd, eTokens) = tokenize commandDefs ["-a"]
  shouldBe Nothing subcmd
  left eTokens

  let Left err = eTokens
  shouldBe "The flag -a requires a parameter." err

test_ShortFlagUnary :: Spec
test_ShortFlagUnary = specify "short-flag-unary" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-x", "foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.x", TokenUnary "-x")] tokens
    shouldBe ["foo", "bar"] args
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-xy", "foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.x", TokenUnary "-x"), ("test.y", TokenUnary "-y")] tokens
    shouldBe ["foo", "bar"] args

test_ShortFlagDuplicate :: Spec
test_ShortFlagDuplicate = specify "short-flag-duplicate" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-x", "-x"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens _args) = eTokens
    equalTokens [("test.x", TokenUnary "-x"), ("test.x", TokenUnary "-x")] tokens
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "-a", "foo"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens _args) = eTokens
    equalTokens [("test.a", Token "-a" "foo"), ("test.a", Token "-a" "foo")] tokens
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "-afoo"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens _args) = eTokens
    equalTokens [("test.a", Token "-a" "foo"), ("test.a", Token "-a" "foo")] tokens

test_LongFlag :: Spec
test_LongFlag = specify "long-flag" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["--long-a", "foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.a", Token "--long-a" "foo")] tokens
    shouldBe ["bar"] args
  do
    let (subcmd, eTokens) = tokenize commandDefs ["--long-a=foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.a", Token "--long-a" "foo")] tokens
    shouldBe ["bar"] args

test_LongFlagUnknown :: Spec
test_LongFlagUnknown = specify "long-flag-unknown" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["--long-c", "foo", "bar"]
    shouldBe Nothing subcmd
    left eTokens

    let Left err = eTokens
    shouldBe "Unknown flag --long-c" err
  do
    let (subcmd, eTokens) = tokenize commandDefs ["--long-c=foo", "bar"]
    shouldBe Nothing subcmd
    left eTokens

    let Left err = eTokens
    shouldBe "Unknown flag --long-c" err

test_LongFlagMissing :: Spec
test_LongFlagMissing = specify "long-flag-missing" do
  let (subcmd, eTokens) = tokenize commandDefs ["--long-a"]
  shouldBe Nothing subcmd
  left eTokens

  let Left err = eTokens
  shouldBe "The flag --long-a requires a parameter." err

test_LongFlagUnary :: Spec
test_LongFlagUnary = specify "long-flag-unary" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["--long-x", "foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.x", TokenUnary "--long-x")] tokens
    shouldBe ["foo", "bar"] args
  do
    let (subcmd, eTokens) = tokenize commandDefs ["--long-x=foo", "bar"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.x", Token "--long-x" "foo")] tokens
    shouldBe ["bar"] args

test_LongFlagDuplicate :: Spec
test_LongFlagDuplicate = specify "long-flag-duplicate" do
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-x", "--long-x"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens _args) = eTokens
    equalTokens [("test.x", TokenUnary "-x"), ("test.x", TokenUnary "--long-x")] tokens
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "--long-a", "foo"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens _args) = eTokens
    equalTokens [("test.a", Token "-a" "foo"), ("test.a", Token "--long-a" "foo")] tokens
  do
    let (subcmd, eTokens) = tokenize commandDefs ["-afoo", "--long-a=foo"]
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens _args) = eTokens
    equalTokens [("test.a", Token "-a" "foo"), ("test.a", Token "--long-a" "foo")] tokens

test_EndFlags :: Spec
test_EndFlags = specify "end-flags" do
  let (subcmd, eTokens) = tokenize commandDefs ["foo", "--", "-a", "bar"]
  shouldBe Nothing subcmd
  right eTokens

  let Right (Tokens tokens args) = eTokens
  equalTokens [] tokens
  shouldBe ["foo", "-a", "bar"] args

test_Subcommand :: Spec
test_Subcommand = specify "subcommand" do
  do
    let (subcmd, eTokens) = tokenize subcommandDefs ["-x", "sub1", "-d", "foo", "--long-e", "bar"]
    shouldBe (Just "sub1") subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.x", TokenUnary "-x"), ("sub.d", Token "-d" "foo"), ("sub.e", TokenUnary "--long-e")] tokens
    shouldBe ["bar"] args
  do
    let (subcmd, eTokens) = tokenize subcommandDefs ["-x", "sub2", "-d", "foo", "--long-e", "bar"]
    shouldBe (Just "sub2") subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.x", TokenUnary "-x"), ("sub.d", TokenUnary "-d"), ("sub.e", TokenUnary "--long-e")] tokens
    shouldBe ["foo", "bar"] args

test_SubcommandUnknown :: Spec
test_SubcommandUnknown = specify "subcommand-unknown" do
  let (subcmd, eTokens) = tokenize subcommandDefs ["foo"]
  shouldBe Nothing subcmd
  left eTokens

  let Left err = eTokens
  shouldBe "Unknown subcommand \"foo\"." err

test_Unicode :: Spec
test_Unicode = specify "unicode" do
  let shortArgs = ["-\12354", "foo", "bar"]
  let longArgs = ["--long-\12354=foo", "bar"]
  do
    let (subcmd, eTokens) = tokenize unicodeDefs shortArgs
    shouldBe Nothing subcmd
    case eTokens of
      Left err -> error err
      Right _ -> return ()
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.a", Token "-\12354" "foo")] tokens
    shouldBe ["bar"] args
  do
    let (subcmd, eTokens) = tokenize unicodeDefs longArgs
    shouldBe Nothing subcmd
    right eTokens

    let Right (Tokens tokens args) = eTokens
    equalTokens [("test.a", Token "--long-\12354" "foo")] tokens
    shouldBe ["bar"] args

equalTokens :: [(String, Token)] -> [([OptionKey], Token)] -> Expectation
equalTokens tokens = shouldBe ([([OptionKey k], t) | (k, t) <- tokens])
