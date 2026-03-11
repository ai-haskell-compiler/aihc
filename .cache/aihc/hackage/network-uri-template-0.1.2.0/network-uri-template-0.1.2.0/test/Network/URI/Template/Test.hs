-- |
--
-- Module      : Network.URI.Template.Test
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Test
  ( assertParse
  , refuteParse
  , runRFCTests
  ) where

import Prelude

import Data.Text (Text)
import Network.URI.Template.Internal.Parse
import Network.URI.Template.Test.RFC
import Test.Hspec

-- | Expect a parse to the given value
assertParse
  :: (Eq a, HasCallStack, Show a)
  => Parser a
  -> Text
  -> a
  -> Expectation
assertParse p input expected =
  case parse (p <* eof) input of
    Left err -> expectationFailure $ "Expected parse, got error:\n" <> errorBundlePretty err
    Right a -> a `shouldBe` expected

-- | Expect a parse error that satisfies the given predicate
refuteParse
  :: (HasCallStack, Show a)
  => Parser a
  -> Text
  -> (String -> Bool)
  -> Expectation
refuteParse p input f =
  case parse (p <* eof) input of
    Left err -> errorBundlePretty err `shouldSatisfy` f
    Right a -> expectationFailure $ "Expected error, got parse:\n" <> show a
