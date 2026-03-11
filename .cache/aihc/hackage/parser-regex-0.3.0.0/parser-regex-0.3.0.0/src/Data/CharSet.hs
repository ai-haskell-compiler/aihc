-- |
-- It is recommended to import this module qualified to avoid name conflicts
-- with functions from the Prelude.
--
-- Enabling [@OverloadedStrings@](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html)
-- will allow declaring @CharSet@s using string literal syntax.
--
-- @
-- {-# LANGUAGE OverloadedStrings #-}
--
-- import qualified Data.CharSet as CS
-- 
-- vowels :: CS.CharSet
-- vowels = "aeiou"
-- @
--
module Data.CharSet
  (
    -- * The @CharSet@ type
    CS.CharSet

    -- * @CharSet@ operations
    -- $ops
  , CS.singleton
  , CS.fromRange
  , CS.fromList
  , CS.fromRanges
  , CS.insert
  , CS.insertRange
  , CS.delete
  , CS.deleteRange
  , CS.map
  , CS.not
  , CS.union
  , CS.difference
  , CS.intersection
  , CS.member
  , CS.notMember
  , CS.elems
  , CS.ranges

    -- * Available @CharSet@s
  , CS.empty
  , CSets.digit
  , CSets.word
  , CSets.space
  , CSets.ascii
  , CSets.asciiAlpha
  , CSets.asciiUpper
  , CSets.asciiLower

    -- * Testing
  , CS.valid
  ) where

import qualified Regex.Internal.CharSet as CS
import qualified Regex.Internal.CharSets as CSets

-- $ops
--
-- Variables used:
--
-- * \(n\): the number of @Char@ ranges
-- * \(s\): the number of @Char@s
-- * \(C\): the maximum bits in a @Char@, i.e. 21
-- * \(n\), \(m\): the number of @Char@ ranges in the first and second sets
--   respectively, for functions taking two sets
