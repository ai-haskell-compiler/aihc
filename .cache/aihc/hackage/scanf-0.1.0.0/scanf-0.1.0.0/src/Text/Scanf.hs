{-# LANGUAGE TypeOperators #-}

-- | Dead simple and type-safe @'scanf'@/@'printf'@.

module Text.Scanf
  ( -- * Main definitions
    scanf
  , printf
  , (:+)(..)
  , fmt
  , fmt_
  , Format

    -- * Format constructors
  , (%)
  , integer
  , int
  , double
  , string
  , char
  ) where

import Text.Scanf.Internal
import Text.Scanf.TH
