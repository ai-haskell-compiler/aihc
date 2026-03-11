-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Impl
-- Copyright   :  (c) Chris Kuklewicz 2006
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Maintainer  :  Andreas Abel
-- Stability   :  stable
-- Portability :  non-portable (Text.Regex.Base needs MPTC+FD)
--
-- Helper functions for defining certain instances of
-- 'RegexContext'. These help when defining instances of 'RegexContext'
-- with repeated types:
--
-- @
-- instance (RegexLike regex source) => RegexContext regex source source where
-- @
--
-- runs into overlapping restrictions. To avoid this I have each backend
-- define, for its own @Regex@ type:
--
-- @
-- instance RegexContext Regex String String where
--   match = polymatch
--   matchM = polymatchM
-- @
--
-- @
-- instance RegexContext Regex ByteString ByteString where
--   match = polymatch
--   matchM = polymatchM
-- @
--
-- @
-- instance RegexContext Regex Text Text where
--   match = polymatch
--   matchM = polymatchM
-- @
-------------------------------------------------------------------------------

module Text.Regex.Base.Impl(polymatch,polymatchM) where

import Prelude
  ( Maybe(Nothing,Just)
  , ($)
  , fst, return
  )
import Control.Monad.Fail (MonadFail(fail))

import Text.Regex.Base
import Data.Array((!))

regexFailed :: (MonadFail m) => m b
{-# INLINE regexFailed #-}
regexFailed =  fail $ "regex failed to match"

actOn :: (RegexLike r s,MonadFail m) => ((s,MatchText s,s)->t) -> r -> s -> m t
{-# INLINE actOn #-}
actOn f r s = case matchOnceText r s of
    Nothing -> regexFailed
    Just preMApost -> return (f preMApost)

polymatch :: (RegexLike a b) => a -> b -> b
{-# INLINE polymatch #-}
polymatch r s = case matchOnceText r s of
    Nothing -> empty
    Just (_, ma, _) -> fst (ma ! 0)

polymatchM :: (RegexLike a b,MonadFail m) => a -> b -> m b
{-# INLINE polymatchM #-}
polymatchM =  actOn (\ (_, ma, _) -> fst (ma ! 0))
