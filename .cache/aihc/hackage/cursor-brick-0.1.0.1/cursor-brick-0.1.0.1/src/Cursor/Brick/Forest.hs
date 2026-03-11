{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Brick.Forest where

import Brick.Types
import Brick.Widgets.Core
import Cursor.Brick.List.NonEmpty
import Cursor.Brick.Tree
import Cursor.Forest
import Cursor.Tree
import qualified Data.List.NonEmpty as NE

horizontalForestCursorWidgetM ::
  Monad m =>
  (CTree b -> m (Widget n)) ->
  (TreeCursor a b -> m (Widget n)) ->
  (CTree b -> m (Widget n)) ->
  ForestCursor a b ->
  m (Widget n)
horizontalForestCursorWidgetM prevFunc curFunc nextFunc =
  horizontalNonEmptyCursorWidgetM prevFunc curFunc nextFunc
    . forestCursorListCursor

horizontalForestCursorWidget ::
  (CTree b -> Widget n) ->
  (TreeCursor a b -> Widget n) ->
  (CTree b -> Widget n) ->
  ForestCursor a b ->
  Widget n
horizontalForestCursorWidget prevFunc curFunc nextFunc =
  horizontalNonEmptyCursorWidget prevFunc curFunc nextFunc
    . forestCursorListCursor

verticalPaddedForestCursorWidgetM ::
  forall a b n m.
  Monad m =>
  (a -> m (Widget n)) ->
  (b -> m (Widget n)) ->
  Int ->
  ForestCursor a b ->
  m (Widget n)
verticalPaddedForestCursorWidgetM goA goB padding =
  verticalForestCursorWidgetM
    goCTree
    (verticalPaddedTreeCursorWidgetM goA goB padding)
    goCTree
  where
    goCTree :: CTree b -> m (Widget n)
    goCTree (CNode b cf) = do
      top <- goB b
      bot <- goCForest cf
      pure $ top <=> padLeft (Pad padding) bot
    goCForest :: CForest b -> m (Widget n)
    goCForest = \case
      EmptyCForest -> pure emptyWidget
      ClosedForest _ -> pure emptyWidget
      OpenForest nect -> fmap vBox $ traverse goCTree $ NE.toList nect

verticalPaddedForestCursorWidget ::
  forall a b n.
  (a -> Widget n) ->
  (b -> Widget n) ->
  Int ->
  ForestCursor a b ->
  Widget n
verticalPaddedForestCursorWidget goA goB padding =
  verticalForestCursorWidget
    goCTree
    (verticalPaddedTreeCursorWidget goA goB padding)
    goCTree
  where
    goCTree :: CTree b -> Widget n
    goCTree (CNode b cf) = goB b <=> padLeft (Pad padding) (goCForest cf)
    goCForest :: CForest b -> Widget n
    goCForest = \case
      EmptyCForest -> emptyWidget
      ClosedForest _ -> emptyWidget
      OpenForest nect -> vBox $ map goCTree $ NE.toList nect

verticalForestCursorWidgetM ::
  Monad m =>
  (CTree b -> m (Widget n)) ->
  (TreeCursor a b -> m (Widget n)) ->
  (CTree b -> m (Widget n)) ->
  ForestCursor a b ->
  m (Widget n)
verticalForestCursorWidgetM prevFunc curFunc nextFunc =
  verticalNonEmptyCursorWidgetM prevFunc curFunc nextFunc
    . forestCursorListCursor

verticalForestCursorWidget ::
  (CTree b -> Widget n) ->
  (TreeCursor a b -> Widget n) ->
  (CTree b -> Widget n) ->
  ForestCursor a b ->
  Widget n
verticalForestCursorWidget prevFunc curFunc nextFunc =
  verticalNonEmptyCursorWidget prevFunc curFunc nextFunc
    . forestCursorListCursor

forestCursorWidgetM ::
  ([CTree b] -> TreeCursor a b -> [CTree b] -> m (Widget n)) ->
  ForestCursor a b ->
  m (Widget n)
forestCursorWidgetM = foldForestCursor

forestCursorWidget ::
  ([CTree b] -> TreeCursor a b -> [CTree b] -> Widget n) ->
  ForestCursor a b ->
  Widget n
forestCursorWidget = foldForestCursor
