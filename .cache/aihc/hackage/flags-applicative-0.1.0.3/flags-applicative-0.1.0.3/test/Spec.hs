{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Applicative ((<|>), optional)
import Data.Either (isLeft)
import Data.Foldable (asum)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec (describe, expectationFailure, hspec, it, shouldBe)

import Flags.Applicative

data Mode = Flexible | Strict deriving (Bounded, Enum, Eq, Show)

main :: IO ()
main = hspec $ do
  describe "parse" $ do
    it "should parse a single flag" $ do
      let
        parser = flag textVal "foo" ""
        res = parseFlags parser ["--foo=abc", "hi"]
      res `shouldBe` Right ("abc", ["hi"])

    it "should fail on duplicate flag" $ do
      let
        parser = (,) <$> flag textVal "foo" "" <*> flag textVal "foo" ""
        res = parseFlags parser []
      res `shouldBe` Left (DuplicateFlag "foo")

    it "should support help" $ do
      let
        parser = flag textVal "foo" ""
        res = parseFlags parser ["--foo=abc", "hi", "--help"]
      isLeft res `shouldBe` True

    it "should fail on unknown flags" $ do
      let
        parser = flag textVal "foo" ""
        res = parseFlags parser ["hi", "--bar"]
      res `shouldBe` Left (UnknownFlag "bar")

    it "should detect unexpected flags" $ do
      let
        parser = switch "bar" "" <|> switch "foo" ""
        res = parseFlags parser ["--bar", "--foo"]
      res `shouldBe` Left (UnexpectedFlags ("foo" :| []))

    it "should branch correctly with unary flags" $ do
      let
        parser = asum
          [ Right <$> flag (autoVal @String) "ok" ""
          , Left <$> flag (autoVal @String) "fail" "" ]
        res = parseFlags parser ["--ok", "\"yes\"", "no"]
      res `shouldBe` Right (Right "yes", ["no"])

    it "should branch correctly with nullary flags" $ do
      let
        parser = (True <$ switch "true" "") <|> (False <$ switch "false" "")
        res = parseFlags parser ["--true", "b", "a"]
      res `shouldBe` Right (True, ["b", "a"])

    it "should fail on inconsistent flag values" $ do
      let
        parser = flag textVal "foo" ""
        res = parseFlags parser ["--foo=1", "--foo=2"]
      res `shouldBe` Left (InconsistentFlagValues "foo")

    it "should support the same flag value multiple times" $ do
      let
        parser = flag (autoVal @Int) "foo" ""
        res = parseFlags parser ["--foo=1", "--foo=1"]
      res `shouldBe` Right (1, [])

    it "should support strings" $ do
      let
        parser = flag stringVal "bar" ""
        res = parseFlags parser ["--bar", "abc0"]
      res `shouldBe` Right ("abc0", [])

    it "should support text lists" $ do
      let
        parser = flag (listOf textVal) "bar" ""
        res = parseFlags parser ["--bar=a,b,c", "def"]
      res `shouldBe` Right (["a", "b", "c"], ["def"])

    it "should support maps" $ do
      let
        parser = flag (mapOf textVal fracVal) "bar" "" :: FlagsParser (Map Text Double)
        res = parseFlags parser ["--bar=a:1,b:0,c:2.5"]
      res `shouldBe` Right (Map.fromList [("a", 1), ("b", 0), ("c", 2.5)], [])

    it "should swallow switches" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--foo", "--bar", "--swallowed_switches=bar"]
      res `shouldBe` Right (True, [])

    it "should fail when a switch is set as a flag" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--foo=3"]
      res `shouldBe` Left (UnexpectedFlagValue "foo")

    it "should swallow flags" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--bar=2", "--swallowed_flags=bar"]
      res `shouldBe` Right (False, [])

    it "should fail when a flag is swallowed as a switch" $ do
      let
        parser = boolFlag "foo" ""
        res = parseFlags parser ["--foo", "--bar=1", "--swallowed_switches=bar"]
      res `shouldBe` Left (UnexpectedFlagValue "bar")

    it "should parse a hostname" $ do
      let
        parser = flag hostVal "host" ""
        res = parseFlags parser ["--host=foo.com"]
      res `shouldBe` Right (("foo.com", Nothing), [])

    it "should parse a hostname and a port" $ do
      let
        parser = flag hostVal "host" ""
        res = parseFlags parser ["--host=localhost:1234"]
      res `shouldBe` Right (("localhost", Just 1234), [])

    it "should fail when given an invalid port" $ do
      let
        parser = flag hostVal "host" ""
        res = parseFlags parser ["--host=localhost:1a2"]
      case res of
        Left (InvalidFlagValue "host" _ _) -> pure ()
        _ -> expectationFailure $ show res

    it "should report all missing required flags" $ do
      let
        parser = switch "foo" "" <|> switch "bar" ""
        res = parseFlags parser []
      case res of
        Left (MissingFlags ("foo" :| ["bar"])) -> pure ()
        _ -> expectationFailure $ show res

    it "should ignore conflicting flags after --" $ do
      let
        parser = switch "foo" "" <|> switch "bar" ""
        res = parseFlags parser ["--foo", "--", "--bar"]
      res `shouldBe` Right ((), ["--bar"])

    it "should ignore undeclared flags after --" $ do
      let
        parser = optional $ flag textVal "foo" ""
        res = parseFlags parser ["--", "--bar=2"]
      res `shouldBe` Right (Nothing, ["--bar=2"])

    it "should parse integral flags" $ do
      let parser = flag intVal "int" "" :: FlagsParser Int
      parseFlags parser ["--int=12"] `shouldBe` Right (12, [])
      parseFlags parser ["--int=-1"] `shouldBe` Right (-1, [])
      parseFlags parser ["--int", "0"] `shouldBe` Right (0, [])
      case parseFlags parser ["--int", "0.1"] of
        Left (InvalidFlagValue "int" "0.1" _) -> pure ()
        r -> expectationFailure $ show r
      case parseFlags parser ["--int", ""] of
        Left (InvalidFlagValue "int" "" _) -> pure ()
        r -> expectationFailure $ show r

    it "should parse fractional flags" $ do
      let parser = flag fracVal "double" "" :: FlagsParser Double
      parseFlags parser ["--double=12.1"] `shouldBe` Right (12.1, [])
      parseFlags parser ["--double=-1"] `shouldBe` Right (-1, [])
      parseFlags parser ["--double", "0"] `shouldBe` Right (0, [])
      case parseFlags parser ["--double", "0."] of
        Left (InvalidFlagValue "double" "0." _) -> pure ()
        r -> expectationFailure $ show r

    it "should parse enum flags" $ do
      let parser = flag enumVal "mode" "" :: FlagsParser Mode
      parseFlags parser ["--mode", "FLEXIBLE"] `shouldBe` Right (Flexible, [])
      parseFlags parser ["--mode=STRICT"] `shouldBe` Right (Strict, [])
      case parseFlags parser ["--mode", "NONE"] of
        Left (InvalidFlagValue "mode" "NONE" _) -> pure ()
        r -> expectationFailure $ show r
