-- |
--
-- Module      : Network.URI.Template.Test.RFC
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Test.RFC
  ( runRFCTests
  ) where

import Prelude

import Conduit
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Foldable (fold, for_, traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Network.URI.Template.Expand
import Network.URI.Template.Internal
import Network.URI.Template.Internal.Parse (restOfLine)
import Network.URI.Template.Internal.Pretty
import Network.URI.Template.VarName
import Network.URI.Template.VarValue
import Test.Hspec
import Text.Megaparsec
import Text.Megaparsec.Char

runRFCTests :: HasCallStack => Spec
runRFCTests = do
  ecases <- runIO parseTestCases
  either (it "parsed examples" . expectationFailure) runTestCases ecases

runTestCases :: [TestCase] -> Spec
runTestCases tcs = traverse_ (runTestCase inputMaxWidth) tcs
 where
  inputMaxWidth =
    maximum -- unsafe: we don't expect empty test cases
      $ map (length . (.input))
      $ concatMap (.examples) tcs

runTestCase :: Int -> TestCase -> Spec
runTestCase inputMaxWidth tc = do
  context (renderPlain varDoc) $ do
    for_ tc.examples $ \e -> do
      it (pad inputMaxWidth e.input <> " => " <> unpack e.expected) $ do
        expandTemplate tc.vars e.template `shouldBe` e.expected
 where
  varMaxWidth =
    max inputMaxWidth
      $ maximum -- unsafe: we don't expect empty variables
      $ map (T.length . unVarName)
      $ Map.keys tc.vars

  varDoc =
    vsep
      [ "Variables"
      , indent 6
          $ vsep
          $ map (uncurry $ variablePretty varMaxWidth)
          $ Map.toList tc.vars
      , indent 4 "Examples"
      ]

pad :: Int -> String -> String
pad w s =
  let n = w - length s
  in  s <> if n > 0 then replicate n ' ' else ""

data TestCase = TestCase
  { vars :: Map VarName VarValue
  , examples :: [TestExample]
  }

data TestExample = TestExample
  { input :: String
  , template :: Template
  , expected :: Text
  }

parseTestCases :: IO (Either String [TestCase])
parseTestCases = do
  t <- T.readFile examples

  pure
    $ first (("RFC parse error:\n" <>) . errorBundlePretty)
    $ parse (some testCaseP <* eof) examples t
 where
  examples :: FilePath
  examples = "rfc/examples.txt"

type Parser = Parsec Void Text

testCaseP :: Parser TestCase
testCaseP =
  TestCase . fold
    <$> someTill (taggedP 'V' variableP) (lookAhead $ void (char 'E') <|> eof)
    <*> someTill (taggedP 'E' texampleP) (lookAhead $ void (char 'V') <|> eof)

taggedP :: Char -> Parser a -> Parser a
taggedP c p = char c *> char ' ' *> p <* hspace <* newline

texampleP :: Parser TestExample
texampleP = do
  (template, expected) <- (,) <$> templateP <*> (char ' ' *> restOfLine)
  pure
    TestExample
      { input = renderPlain $ templatePretty template
      , template
      , expected
      }
