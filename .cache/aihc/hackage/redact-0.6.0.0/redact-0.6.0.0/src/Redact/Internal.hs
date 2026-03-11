------------------------------------------------------------------------------
-- |
-- Module      : Redact.Internal
-- Description : internal functions
-- Copyright   : Copyright (c) 2020-2024 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE ScopedTypeVariables #-}

module Redact.Internal
  ( -- * Internal
    redact
  , redact'
  ) where

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

-- (redact)
import Redact.Types (Line)

------------------------------------------------------------------------------
-- $Internal

-- | Redact text strictly
redact
  :: forall s
   . (s -> Text -> Either String (Line, s))  -- ^ step function
  -> (s -> Maybe String)                     -- ^ end function
  -> s                                       -- ^ initial state
  -> Text
  -> Either String [Line]
redact step end initialState = go initialState [] . T.lines
  where
    go :: s -> [Line] -> [Text] -> Either String [Line]
    go state acc (t:ts) = case step state t of
      Right (line, state') -> go state' (line : acc) ts
      Left err -> Left err
    go state acc [] = case end state of
      Nothing -> Right $ reverse acc
      Just err -> Left err

------------------------------------------------------------------------------

-- | Redact text leniently
redact'
  :: forall s
   . (s -> Text -> (Line, s))  -- ^ step function
  -> s                         -- ^ initial state
  -> Text
  -> [Line]
redact' step initialState = go initialState . T.lines
  where
    go :: s -> [Text] -> [Line]
    go state (t:ts) =
      let (line, state') = step state t
      in  line : go state' ts
    go _state [] = []
