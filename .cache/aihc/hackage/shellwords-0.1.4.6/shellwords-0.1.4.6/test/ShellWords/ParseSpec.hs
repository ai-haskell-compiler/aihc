{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- Case-for-case port from Python:
--
-- <https://github.com/mozillazg/python-shellwords/blob/master/tests/test_shellwords.py>
module ShellWords.ParseSpec
  ( spec
  ) where

import Prelude hiding (truncate)

import Data.Foldable (for_, traverse_)
import ShellWords.Parse (parser, runParser)
import Test.Hspec
import Text.Megaparsec.Char (string)
import Text.Megaparsec.Compat (between)

testCases :: [(String, [String])]
testCases =
  [ ("var --bar=baz", ["var", "--bar=baz"])
  , ("var --bar=\"baz\"", ["var", "--bar=baz"])
  , ("var \"--bar=baz\"", ["var", "--bar=baz"])
  , ("var \"--bar='baz'\"", ["var", "--bar='baz'"])
  , ("var --bar=`baz`", ["var", "--bar=`baz`"])
  , ("var \"--bar=\\\"baz'\"", ["var", "--bar=\"baz'"])
  , ("var \"--bar=\\'baz\\'\"", ["var", "--bar=\\'baz\\'"])
  , ("var \"--bar baz\"", ["var", "--bar baz"])
  , ("var --\"bar baz\"", ["var", "--bar baz"])
  , ("var --\"bar baz\"", ["var", "--bar baz"])
  , -- Additional test cases for whitespace
    ("var --bar=baz ", ["var", "--bar=baz"])
  , (" var --bar=baz ", ["var", "--bar=baz"])
  , (" var   --bar=baz ", ["var", "--bar=baz"])
  , -- Additional test cases for escaped spaces
    ("var --bar\\ baz", ["var", "--bar baz"])
  , ("var --bar=baz\\ bat", ["var", "--bar=baz bat"])
  , ("var --bar baz\\ bat", ["var", "--bar", "baz bat"])
  , -- N.B. sh preserves escapes in quoted values, python-shellwords does not.
    -- we behave like sh in this regard.
    ("var --bar 'baz\\ bat'", ["var", "--bar", "baz\\ bat"])
  , ("var  --bar \"baz\\ bat\"", ["var", "--bar", "baz\\ bat"])
  , ("var \"--bar=\\\\'baz\\\\'\"", ["var", "--bar=\\'baz\\'"])
  , ("var --bar='\\\\'", ["var", "--bar=\\"])
  ]

errorCases :: [String]
errorCases =
  [ "foo '"
  , "foo \""
  --
  -- Doesn't need to error since we don't have parse_backtick either.
  --
  -- , "foo `"
  ]

runTestCase :: HasCallStack => String -> [String] -> Spec
runTestCase input expected =
  it ("parses |" <> truncate 50 input <> "| correctly") $ do
    runParser parser input `shouldBeParsed` expected
 where
  truncate n x
    | length x > n = take n x <> "..."
    | otherwise = x

spec :: Spec
spec = describe "parser" $ do
  traverse_ (uncurry runTestCase) testCases

  for_ errorCases $ \input -> do
    it ("errors on |" <> input <> "|") $ do
      expectParseError $ runParser parser input

  context "Issue #3" $ do
    runTestCase
      "-LC:/Users/Vitor\\ Coimbra/AppData/Local/Programs/stack/x86_64-windows/msys2-20150512/mingw64/lib -ltag\n"
      [ "-LC:/Users/Vitor Coimbra/AppData/Local/Programs/stack/x86_64-windows/msys2-20150512/mingw64/lib"
      , "-ltag"
      ]

  context "Issue #7" $ do
    runTestCase "foo=123 bar" ["foo=123", "bar"]
    runTestCase "foo bar=123 cow" ["foo", "bar=123", "cow"]
    runTestCase "foo bar'();='bar cow" ["foo", "bar();=bar", "cow"]
    runTestCase "foo 'ba'\"r\" cow" ["foo", "bar", "cow"]

  context "As part of a larger Parser" $ do
    let parseDelimited input =
          runParser (between (string "FOO=$(") (string ")") parser) $
            "FOO=$("
              <> input
              <> ")"

    it "parses within delimiters" $ do
      parseDelimited "echo \"hi\"" `shouldBeParsed` ["echo", "hi"]

    it "handles quoted delimiter" $ do
      parseDelimited "echo \"hi (quietly)\""
        `shouldBeParsed` ["echo", "hi (quietly)"]

    it "works with white space" $ do
      parseDelimited "  echo \n\"hi\" " `shouldBeParsed` ["echo", "hi"]

    it "works with newlines" $ do
      parseDelimited "echo \n\"hi\"" `shouldBeParsed` ["echo", "hi"]

    it "works with final newline" $ do
      parseDelimited "echo \n\"hi\"\n" `shouldBeParsed` ["echo", "hi"]

expectParseError :: (Show a, HasCallStack) => Either String a -> Expectation
expectParseError = \case
  Left {} -> pure ()
  Right a' -> expectationFailure $ "Expected parse error, got:" <> show a'

shouldBeParsed
  :: (Show a, Eq a, HasCallStack)
  => Either String a
  -> a
  -> Expectation
a `shouldBeParsed` b = case a of
  Left e -> expectationFailure e
  Right a' -> a' `shouldBe` b

{- Features I'm not sure I'll be porting, certainly not yet.

def test_backtick():
    goversion, err = shell_run("go version")
    assert not err

    s = ShellWords(parse_backtick=True)
    args = s.parse("echo `go version`")

    expected = ["echo", goversion.strip('\n')]
    assert args == expected

def test_backtick_error():
    s = ShellWords(parse_backtick=True)
    try:
        s.parse("echo `go Version`")
    except:
        pass
    else:
        raise Exception("Should be an error")

def test_env():
    os.environ["FOO"] = "bar"

    s = ShellWords(parse_env=True)
    args = s.parse("echo $FOO")

    expected = ["echo", "bar"]
    assert args == expected

def test_no_env():
    s = ShellWords(parse_env=True)
    args = s.parse("echo $BAR")

    expected = ["echo", ""]
    assert args == expected

def test_dup_env():
    os.environ["FOO"] = "bar"
    os.environ["FOO_BAR"] = "baz"

    s = ShellWords(parse_env=True)
    args = s.parse("echo $$FOO$")
    expected = ["echo", "$bar$"]
    assert args == expected

    args = s.parse("echo $${FOO_BAR}$")
    expected = ["echo", "$baz$"]
    assert args == expected
-}
