------------------------------------------------------------------------------
-- |
-- Module      : Redact.Types
-- Description : types representing redacted text
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module Redact.Types
  ( -- * Line
    Line(..)
    -- * Part
  , Part(..)
    -- * Error
  , Error(..)
  ) where

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

------------------------------------------------------------------------------
-- $Line

-- | Lines of redacted text
--
-- @since 0.4.0.0
data Line
  = NormalLine ![Part]  -- ^ normal line of text
  | RedactLine !Text    -- ^ fully-redacted line of text
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- $Part

-- | Parts of a normal line
--
-- @since 0.4.0.0
data Part
  = Stet   !Text  -- ^ text intended to be displayed as-is
  | Redact !Text  -- ^ text intended to be made unreadable
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- $Error

-- | Error sum type
--
-- @since 0.4.0.0
data Error
  = IOError     !IOError  -- ^ I/O error
  | RedactError !String   -- ^ redact parsing error
  deriving (Eq, Show)
