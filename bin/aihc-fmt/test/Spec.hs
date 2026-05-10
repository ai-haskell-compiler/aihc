{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Fmt (FormatError (..), FormatOptions (..), defaultFormatOptions, formatText)
import Aihc.Parser.Lex (LexToken (..), TokenOrigin, lexModuleTokensWithExtensions)
import Aihc.Parser.Syntax (Extension (..), ExtensionSetting (..))
import Data.Text (Text)
import Data.Text qualified as T
import Test.Fmt.CLI (cliTests)
import Test.Fmt.Golden (goldenTests)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Test.Tasty.QuickCheck qualified as QC

main :: IO ()
main = do
  golden <- goldenTests
  defaultMain $
    testGroup
      "aihc-fmt"
      [ golden,
        cliTests,
        testCase "preserves lexed token stream modulo locations" test_preservesTokenStream,
        testCase "preserves unicode syntax tokens" test_preservesUnicodeSyntax,
        testCase "preserves explicit layout tokens" test_preservesExplicitLayout,
        testCase "fails closed on unalignable pretty tokens" test_failsClosedOnUnalignablePrettyTokens,
        QC.testProperty "accepts shared QuickCheck options" prop_acceptsQuickCheckOptions
      ]

prop_acceptsQuickCheckOptions :: () -> Bool
prop_acceptsQuickCheckOptions () = True

test_preservesTokenStream :: IO ()
test_preservesTokenStream = do
  let source = T.pack "module  Main  where\nx  =  1  -- trailing\n"
  case formatText defaultFormatOptions "<test>" source of
    Left err -> assertFailure (show err)
    Right formatted ->
      assertEqual
        "token stream"
        (fingerprint source)
        (fingerprint formatted)

fingerprint :: Text -> [(String, String, TokenOrigin)]
fingerprint = fingerprintWith []

fingerprintWith :: [Extension] -> Text -> [(String, String, TokenOrigin)]
fingerprintWith exts source =
  [ (show (lexTokenKind tok), show (lexTokenText tok), lexTokenOrigin tok)
  | tok <- lexModuleTokensWithExtensions exts source
  ]

test_preservesUnicodeSyntax :: IO ()
test_preservesUnicodeSyntax = do
  let opts = FormatOptions {formatExtensions = [EnableExtension UnicodeSyntax]}
      source = T.pack "module Main where\nf∷a→b\n"
      expected = T.pack "module Main where\nf ∷ a → b\n"
  case formatText opts "<test>" source of
    Left err -> assertFailure (show err)
    Right formatted -> do
      assertEqual "formatted" expected formatted
      assertEqual "token stream" (fingerprintWith [UnicodeSyntax] source) (fingerprintWith [UnicodeSyntax] formatted)

test_preservesExplicitLayout :: IO ()
test_preservesExplicitLayout = do
  let source = T.pack "module Main where { x=1 }\n"
  case formatText defaultFormatOptions "<test>" source of
    Left err -> assertFailure (show err)
    Right formatted ->
      assertEqual "token stream" (fingerprint source) (fingerprint formatted)

test_failsClosedOnUnalignablePrettyTokens :: IO ()
test_failsClosedOnUnalignablePrettyTokens = do
  let source = T.pack "{-# LANGUAGE OverloadedStrings, LambdaCase #-}\nmodule Main where\nx=1\n"
  case formatText defaultFormatOptions "<test>" source of
    Left (TokenStreamMismatch _) -> pure ()
    Left err -> assertFailure ("expected TokenStreamMismatch, got " <> show err)
    Right formatted -> assertFailure ("expected TokenStreamMismatch, got output:\n" <> T.unpack formatted)
