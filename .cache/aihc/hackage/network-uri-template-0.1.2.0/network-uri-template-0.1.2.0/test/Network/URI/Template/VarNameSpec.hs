-- |
--
-- Module      : Network.URI.Template.VarNameSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.VarNameSpec
  ( spec
  ) where

import Prelude

import Data.List (isInfixOf)
import Network.URI.Template.Test
import Network.URI.Template.VarName
import Test.Hspec

spec :: Spec
spec = do
  describe "varNameP" $ do
    it "parses a simple variable" $ do
      assertParse varNameP "var" "var"

    it "parses alphanumerics and _" $ do
      assertParse varNameP "foo_123" "foo_123"

    it "parses percent-encoding" $ do
      assertParse varNameP "foo%25bar" "foo%25bar"

    it "parses dot-separated" $ do
      assertParse varNameP "foo.b.a.r" "foo.b.a.r"

    it "rejects other characters" $ do
      refuteParse varNameP "foo-bar" ("unexpected '-'" `isInfixOf`)

    it "rejects consecutive dots" $ do
      refuteParse varNameP "foo..bar" ("unexpected '.'" `isInfixOf`)
