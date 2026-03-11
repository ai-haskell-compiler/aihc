{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cursor.Brick.Tree where

import Brick.Types
import Brick.Widgets.Core
import Cursor.Tree
import qualified Data.List.NonEmpty as NE

verticalPaddedTreeCursorWidgetM ::
  forall a b n m.
  Monad m =>
  (a -> m (Widget n)) ->
  (b -> m (Widget n)) ->
  Int ->
  TreeCursor a b ->
  m (Widget n)
verticalPaddedTreeCursorWidgetM goA goB padding = treeCursorWidgetM wrap cur
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
    wrap :: [CTree b] -> b -> [CTree b] -> Widget n -> m (Widget n)
    wrap lefts above rights curW = do
      top <- goB above
      bot <-
        vBox
          <$> sequenceA
            ( concat
                [ map goCTree lefts,
                  [pure curW],
                  map goCTree rights
                ]
            )
      pure $ top <=> padLeft (Pad padding) bot
    cur :: a -> CForest b -> m (Widget n)
    cur a cf = do
      top <- goA a
      bot <- goCForest cf
      pure $ top <=> padLeft (Pad padding) bot

verticalPaddedTreeCursorWidget ::
  forall a b n.
  (a -> Widget n) ->
  (b -> Widget n) ->
  Int ->
  TreeCursor a b ->
  Widget n
verticalPaddedTreeCursorWidget goA goB padding = treeCursorWidget wrap cur
  where
    goCTree :: CTree b -> Widget n
    goCTree (CNode b cf) = goB b <=> padLeft (Pad padding) (goCForest cf)
    goCForest :: CForest b -> Widget n
    goCForest = \case
      EmptyCForest -> emptyWidget
      ClosedForest _ -> emptyWidget
      OpenForest nect -> vBox $ map goCTree $ NE.toList nect
    wrap :: [CTree b] -> b -> [CTree b] -> Widget n -> Widget n
    wrap lefts above rights curW =
      goB above
        <=> padLeft
          (Pad 2)
          ( vBox $
              concat
                [ map goCTree lefts,
                  [curW],
                  map goCTree rights
                ]
          )
    cur :: a -> CForest b -> Widget n
    cur a cf = goA a <=> padLeft (Pad padding) (goCForest cf)

treeCursorWidgetM ::
  forall a b n m.
  Monad m =>
  ([CTree b] -> b -> [CTree b] -> Widget n -> m (Widget n)) ->
  (a -> CForest b -> m (Widget n)) ->
  TreeCursor a b ->
  m (Widget n)
treeCursorWidgetM = traverseTreeCursor

treeCursorWidget ::
  forall a b n.
  ([CTree b] -> b -> [CTree b] -> Widget n -> Widget n) ->
  (a -> CForest b -> Widget n) ->
  TreeCursor a b ->
  Widget n
treeCursorWidget = foldTreeCursor
