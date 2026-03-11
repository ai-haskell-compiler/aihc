{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Filters and transforms for HTML trees.
module Zenacy.HTML.Internal.Filter
  ( htmlSpaceRemove
  ) where

import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.HTML
import Zenacy.HTML.Internal.Oper
import Data.Maybe
  ( mapMaybe
  )

-- | Removes whitespace and comments from an HTML structure.
-- Document elements are not accepted, and only non-empty text nodes
-- and element nodes are kept. @pre@, @code@, @samp@, and @kdb@ elements
-- are passed without modification, since whitespace is typically
-- significant in those elements.
htmlSpaceRemove :: HTMLNode -> Maybe HTMLNode
htmlSpaceRemove = go
  where
    go x = case x of
      HTMLText {}
        | htmlTextSpace x ->
            Nothing
        | otherwise ->
            Just x
      HTMLElement n s a c
        | n == "pre" || n == "code" || n == "samp" || n == "kbd" ->
            Just x
        | otherwise ->
            Just $ HTMLElement n s a $ mapMaybe go c
      _otherwise ->
        Nothing
