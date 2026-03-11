-- |
--
-- Module      : Network.URI.Template.Internal.VarSpecSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.Template.Internal.VarSpecSpec
  ( spec
  ) where

import Prelude

import Network.URI.Template.Internal.Modifier
import Network.URI.Template.Internal.VarSpec
import Network.URI.Template.Test
import Test.Hspec

spec :: Spec
spec = do
  describe "varSpecP" $ do
    it "parses a variable without modifier"
      $ assertParse varSpecP "var"
      $ VarSpec
        { name = "var"
        , modifier = Nothing
        }

    it "parses a variable with prefix modifier"
      $ assertParse varSpecP "var:3"
      $ VarSpec
        { name = "var"
        , modifier = Just $ Prefix 3
        }

    it "parses a variable with explode modifier"
      $ assertParse varSpecP "var*"
      $ VarSpec
        { name = "var"
        , modifier = Just Explode
        }
