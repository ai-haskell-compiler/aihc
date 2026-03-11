-- | Helps you build command-line parsers for programs that have more
-- than one so-called /mode/; examples of such programs include @git@,
-- @darcs@, and @ghc-pkg@.
module Multiarg.Mode
  ( ArgSpec(..)
  , OptSpec
  , optSpec
  , Mode
  , mode
  , ModeResult(..)
  , parseModeLine
  ) where

import Multiarg.Mode.Internal
import Multiarg.Types
