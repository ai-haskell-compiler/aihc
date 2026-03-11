-- |
--
-- Module      : Network.URI.TemplateSpec
-- Copyright   : (c) 2025 Patrick Brisbin
-- License     : AGPL-3
-- Maintainer  : pbrisbin@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Network.URI.TemplateSpec
  ( spec
  ) where

import Network.URI.Template.Test
import Test.Hspec

spec :: Spec
spec = context "All RFC Examples" runRFCTests
